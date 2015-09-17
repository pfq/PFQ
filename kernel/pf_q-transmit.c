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

static inline int
__pfq_xmit(struct sk_buff *skb, struct net_device *dev, struct netdev_queue *txq, int xmit_more);


static inline int
__pfq_dev_cap_txqueue(struct net_device *dev, int queue)
{
	if (unlikely(queue >= dev->real_num_tx_queues))
		return 0;
	return queue;
}


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
bool giveup_tx_process(void)
{
	return signal_pending(current) || is_kthread_should_stop();
}


static inline
ktime_t wait_until(uint64_t ts, bool *intr)
{
	ktime_t now;
	do
	{
		now = ktime_get_real();
		if (giveup_tx_process()) {
			*intr = true;
			return now;
		}
	}
	while (ktime_to_ns(now) < ts
	       && (pfq_relax(), true));

	return now;
}


static inline
int swap_sk_queue_and_wait(struct pfq_tx_queue *txs, int cpu, int *index)
{
	if (cpu != Q_NO_KTHREAD) {
		*index = __atomic_add_fetch(&txs->cons, 1, __ATOMIC_RELAXED);
		while (*index != __atomic_load_n(&txs->prod, __ATOMIC_RELAXED))
		{
#ifdef PFQ_DEBUG
			static uint32_t failures = 0;
			if ((failures++ & ((1<<24)-1))== 0)
				printk(KERN_INFO "[PFQ] swap_tx_queue_and_wait spinning!\n");
#endif
			pfq_relax();
			if (giveup_tx_process())
				return -EINTR;
		}
	}
	else {
		*index = __atomic_add_fetch(&txs->cons, 1, __ATOMIC_RELAXED);
		__atomic_store_n(&txs->prod, 1, __ATOMIC_RELAXED);
	}

	return 0;
}


static inline
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
bool tx_queue_last_pkt(struct pfq_pkthdr *hdr)
{
	struct pfq_pkthdr * next = (struct pfq_pkthdr *)((char *)(hdr+1) + ALIGN(hdr->caplen, 8));
        return next->caplen == 0;
}


int
__pfq_sk_tx_queue_xmit(struct pfq_sock *so, struct net_device *dev, size_t idx, int cpu, int node)
{
	struct netdev_queue *txq;
	struct pfq_tx_queue *txs;
	struct pfq_percpu_pool *pool;
	struct pfq_pkthdr *hdr;

	int more = 0, err = 0, total_sent = 0,
            disc = 0, queue, swap;

	char *begin, *end;
	ktime_t now;

	/* get the Tx queue */

	txs = pfq_get_tx_async_queue(&so->opt, idx);

	/* get the netdev_queue for transmission */

	queue = __pfq_dev_cap_txqueue(dev, so->opt.txq_async[idx].queue);
	txq   = netdev_get_tx_queue(dev, queue);

	/* get local cpu data */

	pool = this_cpu_ptr(percpu_pool);

	/* fix cpu */

	cpu = cpu == Q_NO_KTHREAD ? smp_processor_id() : cpu;

	/* swap the soft Tx queue */

	if ((err = swap_sk_queue_and_wait(txs, cpu, &swap)) < 0)
		return err;

	/* increment the swap index of the current queue */

	swap++;

        /* initialize pointer to the current transmit queue */

	begin = so->opt.txq_async[idx].base_addr + (swap & 1) * txs->size;
        end   = begin + txs->size;


	/* Tx loop */

	now = ktime_get_real();

	local_bh_disable();
	HARD_TX_LOCK(dev, txq, smp_processor_id());

	hdr = (struct pfq_pkthdr *)begin;

	for_each_sk_tx_mbuff(hdr, end)
	{
                unsigned int copies, skb_copies;
		struct sk_buff *skb;
		bool xmit_more, last;
                size_t len;

		last = tx_queue_last_pkt(hdr);

		/* wait until the Ts */

		if (hdr->tstamp.tv64 > ktime_to_ns(now)) {

			bool intr = false;

			HARD_TX_UNLOCK(dev, txq);
			local_bh_enable();

			now = wait_until(hdr->tstamp.tv64, &intr);

			if (intr)
				goto stop;

			local_bh_disable();
			HARD_TX_LOCK(dev, txq, smp_processor_id());
		}

		/* allocate a socket buffer */

		skb = pfq_tx_alloc_skb(xmit_slot_size, GFP_KERNEL, node);
		if (unlikely(skb == NULL)) {
			printk(KERN_INFO "[PFQ] Tx could not allocate an skb!\n");
			break;
		}

		/* fill the skb */

		len = min_t(size_t, hdr->caplen, xmit_slot_size);

		skb_reset_tail_pointer(skb);
		skb->dev = dev;
		skb->len = 0;

		__skb_put(skb, len);

		skb_set_queue_mapping(skb, queue);

		/* copy bytes into the payload */

		skb_copy_to_linear_data(skb, hdr+1, len < 64 ? 64 : len);

		/* transmit the packet */

                skb_copies = copies = dev_tx_skb_copies(dev, hdr->data.copies);

		do {
			skb_get(skb);

			xmit_more = (++more == xmit_batch_len || (last && (copies == 1))) ? (more = 0, false) : true;

			if (__pfq_xmit(skb, dev, txq, xmit_more) < 0) {

				HARD_TX_UNLOCK(dev, txq);
				local_bh_enable();

				more = xmit_batch_len - 1;
				pfq_relax();

				if (giveup_tx_process()) {
					pfq_kfree_skb_pool(skb, &pool->tx_pool);
					goto stop;
				}

				local_bh_disable();
				HARD_TX_LOCK(dev, txq, smp_processor_id());
			}
			else {
				copies--;
			}
		}
		while (copies > 0);

		/* update stats */

		total_sent += skb_copies;

		__sparse_add(&so->stats.sent, skb_copies, cpu);
		__sparse_add(&global_stats.sent, skb_copies, cpu);

		/* return the skb and move ptr to the next packet */

		pfq_kfree_skb_pool(skb, &pool->tx_pool);
	}

	HARD_TX_UNLOCK(dev, txq);
	local_bh_enable();

stop:
	/* count the packets left in the shared queue */

	for_each_sk_tx_mbuff(hdr, end)
	{
		disc++;
	}

	/* update stats */

	__sparse_add(&so->stats.disc, disc, cpu);
	__sparse_add(&global_stats.disc, disc, cpu);

	/* clear the queue */

	((struct pfq_pkthdr *)begin)->len = 0;

	return total_sent;
}


/*
 * flush the soft queue
 */

// int
// pfq_sk_queue_flush(struct pfq_sock *so, int index)
// {
// 	struct net_device *dev;
//
// 	if (so->opt.txq_async[index].task) {
// 		return 0;
// 	}
//
// 	dev = dev_get_by_index(sock_net(&so->sk), so->opt.txq[index].if_index);
// 	if (!dev) {
// 		printk(KERN_INFO "[PFQ] pfq_sk_queue_flush[%d]: bad if_index:%d!\n",
// 		       index, so->opt.txq[index].if_index);
// 		return -EPERM;
// 	}
//
// 	pfq_sk_tx_queue_xmit(so, dev, index);
// 	dev_put(dev);
// 	return 0;
// }


static inline int
__pfq_xmit(struct sk_buff *skb, struct net_device *dev, struct netdev_queue *txq, int xmit_more)
{
	int rc = -ENOMEM;

#ifndef PFQ_USE_XMIT_MORE
	xmit_more = 0;
#endif

#if(LINUX_VERSION_CODE >= KERNEL_VERSION(3,18,0))
	skb->xmit_more = xmit_more;
#else
	skb->mark = xmit_more;
#endif

	skb_reset_mac_header(skb);

	if (dev->flags & IFF_UP) {

#if (LINUX_VERSION_CODE <= KERNEL_VERSION(3,2,0))
		if (!netif_tx_queue_stopped(txq))
#else
		if (!netif_xmit_stopped(txq))
#endif
		{
			rc = dev->netdev_ops->ndo_start_xmit(skb, dev);
			if (dev_xmit_complete(rc))
				goto out;
		}
	}

        SPARSE_INC(&memory_stats.os_free);
	kfree_skb(skb);
	return -ENETDOWN;
out:
	return rc;
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

	skb_set_queue_mapping(skb, queue);

	local_bh_disable();
	HARD_TX_LOCK(dev, txq, smp_processor_id());

	ret = __pfq_xmit(skb, dev, txq, more);

	HARD_TX_UNLOCK(dev, txq);
        local_bh_enable();

	return ret;
}


int
pfq_skb_queue_xmit(struct pfq_skbuff_queue *skbs, struct net_device *dev, int queue)
{
	struct netdev_queue *txq;
	struct sk_buff *skb;
	int n, ret = 0;
	size_t last;

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
		skb_set_queue_mapping(skb, queue);

		if (__pfq_xmit(skb, dev, txq, n != last) == NETDEV_TX_OK)
			++ret;
		else
			goto intr;
	}

	HARD_TX_UNLOCK(dev, txq);
	local_bh_enable();

	return ret;

intr:
	for_each_skbuff_from(ret + 1, skbs, skb, n) {
		SPARSE_INC(&memory_stats.os_free);
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
		skb_set_queue_mapping(skb, queue);

		if (__pfq_xmit(skb, dev, txq, 0) == NETDEV_TX_OK)
			++ret;
		else
			goto intr;
	}

	HARD_TX_UNLOCK(dev, txq);
	local_bh_enable();
	return ret;

intr:
	for_each_skbuff_from(ret + 1, skbs, skb, n) {
		SPARSE_INC(&memory_stats.os_free);
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

				if (nskb && __pfq_xmit(nskb, dev, txq, xmit_more) == NETDEV_TX_OK)
					sent++;
				else
					sparse_inc(&global_stats.abrt);
			}
		}

		if (txq) {
			HARD_TX_UNLOCK(dev, txq);
			local_bh_enable();
		}
	}

	return sent;
}


int
pfq_sk_queue_xmit_NG(struct pfq_sock *so, int qindex, int cpu, int node)
{
	printk(KERN_INFO "[PFQ] SK QUEUE XMIT: queue@%p qindex=%d cpu=%d node=%d\n", so, qindex, cpu, node);
	msleep(1000);
	return 0;
}


