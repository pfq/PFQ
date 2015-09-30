/*
 * Copyright (c) 2014 Bonelli Nicola <nicola@pfq.io>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met: 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer. 2.
 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#include <pragma/diagnostic_push>

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/version.h>
#include <linux/sched.h>
#include <linux/ktime.h>
#include <linux/skbuff.h>
#include <linux/netdevice.h>
#include <linux/delay.h>

#include <pragma/diagnostic_pop>

#include <pf_q-thread.h>
#include <pf_q-transmit.h>
#include <pf_q-memory.h>
#include <pf_q-sock.h>
#include <pf_q-define.h>
#include <pf_q-global.h>
#include <pf_q-GC.h>
#include <pf_q-printk.h>
#include <pf_q-netdev.h>


static inline int
__pfq_xmit(struct sk_buff *skb, struct net_device *dev, int xmit_more);


#if (LINUX_VERSION_CODE > KERNEL_VERSION(3,13,0))
static u16 __pfq_pick_tx_default(struct net_device *dev, struct sk_buff *skb)
{
	return 0;
}
#endif


/* select the right tx hw queue, and fix it (-1 means any queue).
 * unlike the linux netdev_pick_tx, it does *not* set the queue mapping in the skb */

static struct netdev_queue *
pfq_netdev_pick_tx(struct net_device *dev, struct sk_buff *skb, int *queue)
{
	if (dev->real_num_tx_queues != 1 && *queue == -1) {

		const struct net_device_ops *ops = dev->netdev_ops;

		*queue = ops->ndo_select_queue ?
#if (LINUX_VERSION_CODE < KERNEL_VERSION(3,13,0))
			ops->ndo_select_queue(dev, skb)
#elif (LINUX_VERSION_CODE == KERNEL_VERSION(3,13,0))
			ops->ndo_select_queue(dev, skb, NULL)
#else
			ops->ndo_select_queue(dev, skb, NULL, __pfq_pick_tx_default)
#endif
			: 0;
	}

	*queue = __pfq_dev_cap_txqueue(dev, *queue);

	return netdev_get_tx_queue(dev, *queue);
}



static inline
bool giveup_tx_process(atomic_t const *stop)
{
	return atomic_read(stop) == -1 || signal_pending(current) || is_kthread_should_stop();
}


static inline
ktime_t wait_until(uint64_t ts, atomic_t const *stop, bool *intr)
{
	ktime_t now;
	do
	{
		now = ktime_get_real();
		if (giveup_tx_process(stop)) {
			*intr= true;
			return now;
		}
	}
	while (ktime_to_ns(now) < ts
	       && (pfq_relax(), true));

	return now;
}


static inline
ptrdiff_t swap_sk_tx_queue(struct pfq_tx_queue *txm, int *prod)
{
	*prod = __atomic_load_n(&txm->prod.index, __ATOMIC_RELAXED);
	if (*prod == __atomic_load_n(&txm->cons.index, __ATOMIC_RELAXED))
	{
		__atomic_store_n(&txm->cons.index, *prod+1, __ATOMIC_RELAXED);
		txm->cons.off = 0;
	}

	return __atomic_load_n((*prod & 1) ? &txm->prod.off1 : &txm->prod.off0, __ATOMIC_ACQUIRE);
}


static
unsigned int dev_tx_skb_copies(struct net_device *dev, unsigned int req_copies)
{
	if (likely(req_copies == 1 || req_copies == 0))
		return 1;

	if (unlikely(!(dev->priv_flags & IFF_TX_SKB_SHARING))) {
		if (printk_ratelimit())
			printk(KERN_INFO "[PFQ] tx_skb_copies: device '%s' does not support TX_SKB_SHARING!\n",
			       dev->name);
		return 1;
	}

	if (unlikely(req_copies > Q_MAX_TX_SKB_COPY))
		return Q_MAX_TX_SKB_COPY;

	return req_copies;
}


static inline
devq_id_t
make_devq_id(struct pfq_pkthdr *hdr, devq_id_t const default_qid)
{
	devq_id_t qid = PFQ_NETQ_ID(hdr->ifindex, hdr->queue);
	if (PFQ_NETQ_IS_DEFAULT(qid))
		return default_qid;
	return qid;
}



static int
__pfq_mbuff_xmit(struct pfq_pkthdr *hdr, struct pfq_mbuff_xmit_context *ctx, int slot_size,
	       int node, atomic_t const *stop, bool last_pkt, bool *intr)
{
	unsigned int copies, total_copies;
	devq_id_t cur_qid, next_qid;
	struct pfq_pkthdr *next;
	struct sk_buff *skb;
	size_t len;
	bool last;

	/* skip this packet ? */

	cur_qid = make_devq_id(hdr, ctx->default_qid);
	if (unlikely(PFQ_NETQ_IS_NULL(cur_qid)))
		return 0;

	/* get next_qid ...*/

	next = Q_NEXT_PKTHDR(hdr, slot_size);
	next_qid = last_pkt ? PFQ_NETQ_NULL : make_devq_id(next, ctx->default_qid);


	/* dev_queue switch, or relax the batch queue (at the first packet of
	 * this new batch) ? */

	ctx->batch_cntr++;

	if (ctx->prec_qid != cur_qid || (ctx->batch_cntr == 1 && need_resched())) {

		pfq_hard_tx_unlock(&ctx->dev_queue);
		local_bh_enable();

		dev_queue_put(ctx->net, &ctx->default_dev, &ctx->dev_queue);

		if (ctx->batch_cntr == 1)
			schedule();

		dev_queue_get(ctx->net, &ctx->default_dev, cur_qid , &ctx->dev_queue);

		local_bh_disable();
		pfq_hard_tx_lock(&ctx->dev_queue);

		ctx->prec_qid  = cur_qid;
		ctx->batch_cntr = 1;
	}

	/* is this the last skb of the batch ? */

	if (ctx->batch_cntr >= xmit_batch_len || cur_qid != next_qid || last_pkt) {
		last = true;
		ctx->batch_cntr = 0;
	}
	else {
		last = false;
	}

	/* ensure the device is ok */

	if (unlikely(ctx->dev_queue.dev == NULL)) {
		if (printk_ratelimit())
			printk(KERN_INFO "[PFQ] dev: ifindex=%d not found!\n", PFQ_NETQ_IFINDEX(cur_qid));
		return 0;
	}

	/* wait until the Ts ? */

	if (hdr->tstamp.tv64 > ktime_to_ns(ctx->now)) {

		pfq_hard_tx_unlock(&ctx->dev_queue);
		local_bh_enable();

		ctx->now = wait_until(hdr->tstamp.tv64, stop, intr);

		local_bh_disable();
		pfq_hard_tx_lock(&ctx->dev_queue);

		if (*intr)
			return 0;
	}

	/* allocate a new socket buffer */

	skb = pfq_alloc_skb_pool(xmit_slot_size, GFP_KERNEL, node, ctx->skb_pool);
	if (unlikely(skb == NULL)) {
		if (printk_ratelimit())
			printk(KERN_INFO "[PFQ] Tx could not allocate an skb!\n");
		return 0;
	}

	/* fill the socket buffer */

	len = min_t(size_t, hdr->caplen, xmit_slot_size);

	skb_reset_tail_pointer(skb);
	skb->dev = ctx->dev_queue.dev;
	skb->len = 0;

	__skb_put(skb, len);

	skb_set_queue_mapping(skb, ctx->dev_queue.queue_mapping);
	skb_copy_to_linear_data(skb, hdr+1, len < 64 ? 64 : len);

	/* transmit the packet(s) */

	total_copies = copies = dev_tx_skb_copies(ctx->dev_queue.dev, hdr->data.copies);

	do {
		const bool xmit_more = !last || copies != 1;

		skb_get(skb);

		if (__pfq_xmit(skb, ctx->dev_queue.dev, xmit_more) < 0) {

			ctx->batch_cntr = 0;

			if (need_resched()) {
				pfq_hard_tx_unlock(&ctx->dev_queue);
				local_bh_enable();

				pfq_relax();

				local_bh_disable();
				pfq_hard_tx_lock(&ctx->dev_queue);
			}

			if (giveup_tx_process(stop)) {
				pfq_kfree_skb_pool(skb, ctx->skb_pool);
				*intr = true;
				return total_copies - copies;
			}
		}
		else {
			ctx->dev_queue.queue->trans_start = ctx->jiffies;
			copies--;
		}
	}
	while (copies > 0);

	pfq_kfree_skb_pool(skb, ctx->skb_pool);

	return total_copies;
}



int
pfq_sk_queue_xmit(struct pfq_sock *so, int sock_queue, int cpu, int node, atomic_t const *stop)
{
	struct pfq_tx_info const * txinfo = pfq_get_tx_queue_info(&so->opt, sock_queue);
	struct pfq_mbuff_xmit_context ctx;
	struct pfq_tx_queue *txm;
	struct pfq_pkthdr *hdr;
	ptrdiff_t prod_off;

	int total_sent = 0, disc = 0, prod_idx;
        char *begin, *end;

	/* get the Tx queue */

	txm = pfq_get_tx_queue(&so->opt, sock_queue);
	if (txm == NULL)
		return 0; /* socket not enabled... */

	/* setup ctx */

	ctx.default_qid = PFQ_NETQ_ID(txinfo->def_ifindex, txinfo->def_queue);
        ctx.prec_qid = PFQ_NETQ_ID(txinfo->def_ifindex, txinfo->def_queue);

	ctx.default_dev.ifindex = txinfo->def_ifindex;
	ctx.default_dev.dev = txinfo->def_dev;
	ctx.default_dev.net = sock_net(&so->sk);
	ctx.batch_cntr = 0;
        ctx.net = sock_net(&so->sk);

	/* enable skb_pool for Tx threads */

	if (cpu != Q_NO_KTHREAD)
	{
		/* get local pool data */
		struct pfq_percpu_pool *pool = this_cpu_ptr(percpu_pool);
		if (likely(atomic_read(&pool->enable)))
			ctx.skb_pool = &pool->tx_pool;
	}
	else {
		cpu = smp_processor_id();
		ctx.skb_pool = NULL;
	}

	/* initialize boundaries for the transmit queue */

	prod_off  = swap_sk_tx_queue(txm, &prod_idx);
	begin = txinfo->base_addr + (prod_idx & 1) * txm->size + txm->cons.off;
	end   = txinfo->base_addr + (prod_idx & 1) * txm->size + prod_off;

	/* lock the default dev_queue */

	dev_queue_get(sock_net(&so->sk), &ctx.default_dev, ctx.default_qid , &ctx.dev_queue);

	local_bh_disable();
	pfq_hard_tx_lock(&ctx.dev_queue);

	/* traverse the socket queue */

	ctx.now = ktime_get_real();
	ctx.jiffies = jiffies;

	hdr  = (struct pfq_pkthdr *)begin;

	for_each_sk_mbuff(hdr, end, 0)
	{
		devq_id_t qid;
                bool intr = false;
		int sent;

		/* skip this packet ? */

		qid = make_devq_id(hdr, ctx.default_qid);
		if (unlikely(PFQ_NETQ_IS_NULL(qid)))
			continue;

		sent = __pfq_mbuff_xmit(hdr, &ctx, 0, node, stop,
					Q_NEXT_PKTHDR(hdr, 0) >= (struct pfq_pkthdr *)end , &intr);

		/* update stats */

		total_sent += sent;
		__sparse_add(so->stats, sent, sent, cpu);
		__sparse_add(&global_stats, sent, sent, cpu);

		if (unlikely(intr))
			break;
	}

	/* unlock the current locked queue */

	pfq_hard_tx_unlock(&ctx.dev_queue);
	local_bh_enable();

	/* release the device */

	dev_queue_put(sock_net(&so->sk), &ctx.default_dev, &ctx.dev_queue);

	/* update the local consumer offset */

	txm->cons.off = prod_off;

	/* count the packets left in the shared queue */

	for_each_sk_mbuff(hdr, end, 0)
	{
		disc++;
	}

	/* update stats */

	__sparse_add(so->stats, disc, disc, cpu);
	__sparse_add(&global_stats, disc, disc, cpu);

	return total_sent;
}


static inline int
__pfq_xmit(struct sk_buff *skb, struct net_device *dev, int xmit_more)
{
	int rc = -ENOMEM;

#if(LINUX_VERSION_CODE >= KERNEL_VERSION(3,18,0))
	skb->xmit_more = xmit_more;
#else
	skb->mark = xmit_more;
#endif

	rc = dev->netdev_ops->ndo_start_xmit(skb, dev);
	if (dev_xmit_complete(rc))
		return rc;

        sparse_inc(&memory_stats, os_free);
	kfree_skb(skb);
	return -ENETDOWN;
}


int
pfq_xmit(struct sk_buff *skb, struct net_device *dev, int queue, int more)
{
	struct netdev_queue *txq;
	int ret = 0;

	/* get txq and fix the queue for this batch.
	 *
	 * note: in case the queue is set to any-queue (-1), the driver along the first skb
	 * select the queue */

	txq = pfq_netdev_pick_tx(dev, skb, &queue);

	skb_reset_mac_header(skb);
	skb_set_queue_mapping(skb, queue);

	local_bh_disable();
	HARD_TX_LOCK(dev, txq, smp_processor_id());

	ret = __pfq_xmit(skb, dev, more);

	HARD_TX_UNLOCK(dev, txq);
        local_bh_enable();

	return ret;
}


int
pfq_skb_queue_xmit(struct pfq_skbuff_queue *skbs, struct net_device *dev, int queue)
{
	struct netdev_queue *txq;
	struct sk_buff *skb;
	size_t n, last;
	int ret = 0;

	/* get txq and fix the queue for this batch.
	 *
	 * note: in case the queue is set to any-queue (-1), the driver along the first skb
	 * select the queue */

	txq = pfq_netdev_pick_tx(dev, skbs->queue[0], &queue);

	last = pfq_skbuff_queue_len(skbs) - 1;

	local_bh_disable();
	HARD_TX_LOCK(dev, txq, smp_processor_id());

	for_each_skbuff(skbs, skb, n)
	{
		skb_reset_mac_header(skb);
		skb_set_queue_mapping(skb, queue);

		if (__pfq_xmit(skb, dev, n != last) == NETDEV_TX_OK)
			++ret;
		else
			goto intr;
	}

	HARD_TX_UNLOCK(dev, txq);
	local_bh_enable();

	return ret;

intr:
	for_each_skbuff_from(ret + 1, skbs, skb, n) {
		sparse_inc(&memory_stats, os_free);
		kfree_skb(skb);
	}

	HARD_TX_UNLOCK(dev, txq);
	local_bh_enable();
	return ret;
}


int
pfq_skb_queue_xmit_by_mask(struct pfq_skbuff_queue *skbs, unsigned long long mask, struct net_device *dev, int queue)
{
	struct netdev_queue *txq;
	struct sk_buff *skb;
	int n, ret = 0;

	/* get txq and fix the queue for this batch.
	 *
	 * note: in case the queue is set to any-queue (-1), the driver along the first skb
	 * select the queue */

	txq = pfq_netdev_pick_tx(dev, skbs->queue[0], &queue);

	local_bh_disable();
	HARD_TX_LOCK(dev, txq, smp_processor_id());

	for_each_skbuff_bitmask(skbs, mask, skb, n)
	{
		skb_reset_mac_header(skb);
		skb_set_queue_mapping(skb, queue);

		if (__pfq_xmit(skb, dev, 0) == NETDEV_TX_OK)
			++ret;
		else
			goto intr;
	}

	HARD_TX_UNLOCK(dev, txq);
	local_bh_enable();
	return ret;

intr:
	for_each_skbuff_from(ret + 1, skbs, skb, n) {
		sparse_inc(&memory_stats, os_free);
		kfree_skb(skb);
	}

	HARD_TX_UNLOCK(dev, txq);
	local_bh_enable();

	return ret;
}


int
pfq_lazy_xmit(struct sk_buff __GC * skb, struct net_device *dev, int queue)
{
	struct GC_log *skb_log = PFQ_CB(skb)->log;

	if (skb_log->num_devs >= Q_GC_LOG_QUEUE_LEN) {
		if (printk_ratelimit())
			printk(KERN_INFO "[PFQ] bridge %s: too many annotation!\n", dev->name);
		return 0;
	}

	skb_set_queue_mapping(PFQ_SKB(skb), queue);
	skb_log->dev[skb_log->num_devs++] = dev;
	skb_log->xmit_todo++;

	return 1;
}


int
pfq_skb_queue_lazy_xmit(struct pfq_skbuff_GC_queue *queue, struct net_device *dev, int queue_index)
{
	struct sk_buff __GC * skb;
	int i, n = 0;

	for_each_skbuff(queue, skb, i)
	{
		if (pfq_lazy_xmit(skb, dev, queue_index))
			++n;
	}

	return n;
}


int
pfq_skb_queue_lazy_xmit_by_mask(struct pfq_skbuff_GC_queue *queue, unsigned long long mask,
			    struct net_device *dev, int queue_index)
{
	struct sk_buff __GC * skb;
	int i, n = 0;

	for_each_skbuff_bitmask(queue, mask, skb, i)
	{
		if (pfq_lazy_xmit(skb, dev, queue_index))
			++n;
	}

	return n;
}


static inline
struct sk_buff *
skb_tx_clone(struct net_device *dev, struct sk_buff *skb, gfp_t mask)
{
	if (likely(dev->priv_flags & IFF_TX_SKB_SHARING))
		return skb_get(skb);
	return skb_clone(skb, mask);
}


size_t
pfq_skb_queue_lazy_xmit_run(struct pfq_skbuff_GC_queue *skbs, struct pfq_endpoint_info const *endpoints)
{
	struct netdev_queue *txq;
	struct net_device *dev;
	struct sk_buff __GC *skb;
        size_t sent = 0;
	size_t n, i;
	int queue = -1;

	/* for each net_device... */

	for(n = 0; n < endpoints->num; n++)
	{
		size_t sent_dev = 0;
		dev = endpoints->dev[n];

		txq = NULL;
                queue = -1;

		/* scan the list of skbs, and forward them in batch fashion */

		for(i = 0; i < skbs->len; i++)
		{
                        size_t j, num;

			/* select packet and log */

			skb = skbs->queue[i];

			num = GC_count_dev_in_log(dev, PFQ_CB(skb)->log);

			if (num == 0)
				continue;

			if (queue != skb->queue_mapping) {

				queue = skb->queue_mapping;

				if (txq) {
					HARD_TX_UNLOCK(dev, txq);
					local_bh_enable();
                                }

				txq = pfq_netdev_pick_tx(dev, PFQ_SKB(skb), &queue);

				local_bh_disable();
				HARD_TX_LOCK(dev, txq, smp_processor_id());
			}

			/* forward this skb `num` times (to this device) */

			for (j = 0; j < num; j++)
			{
				const int xmit_more  = ++sent_dev != endpoints->cnt[n];
				const bool to_clone  = PFQ_CB(skb)->log->to_kernel || PFQ_CB(skb)->log->xmit_todo-- > 1;

				struct sk_buff *nskb = to_clone ? skb_tx_clone(dev, PFQ_SKB(skb), GFP_ATOMIC) :
								  skb_get(PFQ_SKB(skb));

				if (nskb && __pfq_xmit(nskb, dev, xmit_more) == NETDEV_TX_OK)
					sent++;
				else
					sparse_inc(&global_stats, abrt);
			}
		}

		if (txq) {
			HARD_TX_UNLOCK(dev, txq);
			local_bh_enable();
		}
	}

	return sent;
}


