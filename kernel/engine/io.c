/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
 *             Andrea Di Pietro <andrea.dipietro@for.unipi.it>
 * 	       Loris Gazzarrini <loris.gazzarrini@iet.unipi.it>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * The full GNU General Public License is included in this distribution in
 * the file called "COPYING".
 *
 ****************************************************************/

#include <net/sock.h>
#ifdef CONFIG_INET
#include <net/inet_common.h>
#endif

#include <engine/io.h>
#include <engine/core.h>
#include <engine/percpu.h>
#include <engine/global.h>
#include <engine/devmap.h>

#include <engine/lang/engine.h>
#include <engine/lang/symtable.h>
#include <engine/lang/GC.h>

#include <engine/queue.h>
#include <engine/bitops.h>

#include <pfq/vlan.h>
#include <pfq/thread.h>
#include <pfq/memory.h>


int
pfq_receive(struct napi_struct *napi, struct sk_buff * skb, int direct)
{
	struct pfq_percpu_data * data;
	int cpu;

	/* if no socket is open drop the packet */

	if (unlikely(pfq_get_sock_count() == 0)) {
		sparse_inc(&memory_stats, os_free);
		kfree_skb(skb);
		return 0;
	}

        cpu = smp_processor_id();
	data = per_cpu_ptr(percpu_data, cpu);

	if (likely(skb))
	{
		struct sk_buff __GC * buff;

		/* if required, timestamp the packet now */

		if (skb->tstamp.tv64 == 0)
			__net_timestamp(skb);

		/* if vlan header is present, remove it */

		if (vl_untag && skb->protocol == cpu_to_be16(ETH_P_8021Q)) {
			skb = pfq_vlan_untag(skb);
			if (unlikely(!skb)) {
				__sparse_inc(&global_stats, lost, cpu);
				return -1;
			}
		}

		skb_reset_mac_len(skb);

		/* push the mac header: reset skb->data to the beginning of the packet */

		if (likely(skb->pkt_type != PACKET_OUTGOING))
		    skb_push(skb, skb->mac_len);

		/* pass the ownership of this skb to the garbage collector */

		buff = GC_make_buff(data->GC, skb);
		if (buff == NULL) {
			if (printk_ratelimit())
				printk(KERN_INFO "[PFQ] GC: memory exhausted!\n");
			__sparse_inc(&global_stats, lost, cpu);
			__sparse_inc(&memory_stats, os_free, cpu);
			kfree_skb(skb);
			return 0;
		}

		PFQ_CB(buff)->direct = direct;

		if ((GC_size(data->GC) < (size_t)capt_batch_len) &&
		     (ktime_to_ns(ktime_sub(skb_get_ktime(PFQ_SKB(buff)), data->last_rx)) < 1000000))
		{
			return 0;
		}

		data->last_rx = skb_get_ktime(PFQ_SKB(buff));
	}
	else {
                if (GC_size(data->GC) == 0)
		{
			return 0;
		}
	}

	return pfq_process_batch(data,
				 per_cpu_ptr(percpu_sock, cpu),
				 per_cpu_ptr(percpu_pool, cpu),
				 data->GC, cpu);
}


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


/*
 * wait function for active timestamping
 *
 */

static inline
ktime_t wait_until_busy(uint64_t ts, atomic_t const *stop, bool *intr)
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
	while (ktime_to_ns(now) < ts);
	return now;
}


static inline
ktime_t wait_until_relaxed(uint64_t ts, atomic_t const *stop, bool *intr)
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


static
ktime_t wait_until(uint64_t tv64, ktime_t now, struct net_dev_queue *dev_queue, atomic_t const *stop, bool *intr)
{
	ktime_t now_;

	if (tv64 > ktime_to_ns(now)) {

		if ((tv64 - ktime_to_ns(now)) < 100000) { /* < 100us, just like pktgen */

			now_ = wait_until_busy(tv64, stop, intr);
		}
		else {
			pfq_hard_tx_unlock(dev_queue);
			local_bh_enable();

			now_ = wait_until_relaxed(tv64, stop, intr);

			local_bh_disable();
			pfq_hard_tx_lock(dev_queue);
		}
	}
	else {
		if (!tx_rate_control_eager)
			return now;
	}

	return now_;
}



static inline
ptrdiff_t acquire_sk_tx_prod_off_by(int index, struct pfq_tx_queue *txm)
{
	return __atomic_load_n((index & 1) ? &txm->prod.off1 : &txm->prod.off0, __ATOMIC_ACQUIRE);
}


static inline
ptrdiff_t maybe_swap_sk_tx_queue(struct pfq_tx_queue *txm, unsigned int *cons_ret)
{
	unsigned int prod_idx = __atomic_load_n(&txm->prod.index, __ATOMIC_ACQUIRE);
	unsigned int cons_idx = __atomic_load_n(&txm->cons.index, __ATOMIC_RELAXED);

	ptrdiff_t prod_off = acquire_sk_tx_prod_off_by(cons_idx, txm);

	if (prod_idx != cons_idx && txm->cons.off == prod_off)
	{
		__atomic_store_n(&txm->cons.index, prod_idx, __ATOMIC_RELAXED);
		txm->cons.off = 0;
		*cons_ret = prod_idx;
		return acquire_sk_tx_prod_off_by(prod_idx, txm);
	}
	else
	{
		*cons_ret = cons_idx;
	}

	return prod_off;
}


static
unsigned int dev_tx_max_skb_copies(struct net_device *dev, unsigned int req_copies)
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


/*
 *  single packet transmission...
 */

static inline int
__pfq_xmit_retry(struct sk_buff *skb, struct net_device *dev, int xmit_more, bool retry)
{
	int rc;

#if(LINUX_VERSION_CODE >= KERNEL_VERSION(3,18,0))
	skb->xmit_more = xmit_more;
#else
	skb->mark = xmit_more;
#endif

	rc = dev->netdev_ops->ndo_start_xmit(skb, dev);
	if (dev_xmit_complete(rc)) {
		return rc;
	}

	if (!retry) {
		sparse_inc(&memory_stats, os_free);
		kfree_skb(skb);
	}

	return rc;
}


static inline int
__pfq_xmit(struct sk_buff *skb, struct net_device *dev, int xmit_more)
{
	return __pfq_xmit_retry(skb, dev, xmit_more, false);
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

	if (unlikely(netif_xmit_frozen_or_drv_stopped(txq)))
		return NETDEV_TX_BUSY;

	ret = __pfq_xmit(skb, dev, more);

	HARD_TX_UNLOCK(dev, txq);
        local_bh_enable();

	return ret;
}


/*
 * transmit a mbuff packet with copies
 */

static tx_ret
__pfq_mbuff_xmit(struct pfq_pkthdr *hdr, struct net_dev_queue *dev_queue,
		 struct pfq_mbuff_xmit_context *ctx, int copies, bool xmit_more, atomic_t const *stop, bool *intr)
{
	struct sk_buff *skb;
        tx_ret ret = { 0 };
	size_t len;

	if (!dev_queue->dev)
		return (tx_ret){.ok = 0, .fail = copies};

	/* wait until for the timestap to expire (if specified) */

	if (hdr->tstamp.tv64) {
		ctx->now = wait_until(hdr->tstamp.tv64, ctx->now, dev_queue, stop, intr);
		if (*intr)
			return (tx_ret){.ok = 0, .fail = copies};
	}

	/* allocate a new socket buffer */

	skb = pfq_alloc_skb_pool(LL_RESERVED_SPACE(dev_queue->dev) + xmit_slot_size, GFP_KERNEL, ctx->node, ctx->skb_pool);
	if (unlikely(skb == NULL)) {
		if (printk_ratelimit())
			printk(KERN_INFO "[PFQ] Tx could not allocate an skb!\n");
		return (tx_ret){.ok = 0, .fail = copies};
	}

	/* fill the socket buffer */

	skb_reserve(skb, LL_RESERVED_SPACE(dev_queue->dev));

	len = min_t(size_t, hdr->caplen, xmit_slot_size);

	skb_reset_tail_pointer(skb);
	skb->dev = dev_queue->dev;
	skb->len = 0;
	__skb_put(skb, len);

	skb_set_queue_mapping(skb, dev_queue->queue_mapping);

	skb_copy_to_linear_data(skb, hdr+1, len < 64 ? 64 : len);

	/* transmit the packet + copies */

	atomic_set(&skb->users, copies + 1);

	do {
		const bool xmit_more_ = xmit_more || copies != 1;

		/* if copies > 1, then the device support TX_SKB_SHARING */

		if (likely(!netif_xmit_frozen_or_drv_stopped(dev_queue->queue)) &&
			__pfq_xmit_retry(skb, dev_queue->dev, xmit_more_, true) == NETDEV_TX_OK) {
			ret.ok++;
			copies--;
		}
		else {
			ret.fail++;

			pfq_hard_tx_unlock(dev_queue);
			local_bh_enable();

			if (need_resched())
				schedule();

			local_bh_disable();
			pfq_hard_tx_lock(dev_queue);

			if (giveup_tx_process(stop)) {
				atomic_set(&skb->users, 1);
				*intr = true;
				break;
			}
		}
	}
	while (copies > 0);

	pfq_kfree_skb_pool(skb, ctx->skb_pool);

	if (ret.ok)
		dev_queue->queue->trans_start = ctx->jiffies;

	return ret;
}


/*
 * transmit queues of packets (from memory mapped queue)...
 */

tx_ret
pfq_sk_queue_xmit(struct pfq_sock *so, int sock_queue, int cpu, int node, atomic_t const *stop)
{
	struct pfq_tx_info const * txinfo = pfq_get_tx_queue_info(&so->opt, sock_queue);
	struct net_dev_queue dev_queue = net_dev_queue_null;
	struct pfq_mbuff_xmit_context ctx;
	int batch_cntr = 0, cons_idx;
	struct pfq_tx_queue *txm;
	struct pfq_pkthdr *hdr;
	ptrdiff_t prod_off;
        char *begin, *end;
        tx_ret ret = {0};

	/* get the Tx queue descriptor */

	txm = pfq_get_tx_queue(&so->opt, sock_queue);
	if (txm == NULL)
		return ret; /* socket not enabled... */

	/* enable skb_pool for Tx threads */

	if (cpu != Q_NO_KTHREAD)
	{
		/* get local pool data */
		struct pfq_percpu_pool *pool = this_cpu_ptr(percpu_pool);
		ctx.skb_pool = likely(atomic_read(&pool->enable)) ? &pool->tx_pool : NULL;
	}
	else {
		cpu = smp_processor_id();
		ctx.skb_pool = NULL;
	}

	/* initialize the boundaries of this queue */

	prod_off = maybe_swap_sk_tx_queue(txm, &cons_idx);
	begin    = txinfo->base_addr + (cons_idx & 1) * txm->size + txm->cons.off;
	end      = txinfo->base_addr + (cons_idx & 1) * txm->size + prod_off;

        /* setup the context */

        ctx.net = sock_net(&so->sk);
	ctx.now = ktime_get_real();
	ctx.jiffies = jiffies;
        ctx.node = node;

	hdr = (struct pfq_pkthdr *)begin;

	/* lock the dev_queue and disable bh */

	if (dev_queue_get(ctx.net, PFQ_DEVQ_ID(txinfo->def_ifindex, txinfo->def_queue), &dev_queue) < 0)
	{
		if (printk_ratelimit())
			printk(KERN_INFO "[PFQ] sk_queue_xmit: could not lock default device!\n");
		return ret;
	}

	local_bh_disable();
	pfq_hard_tx_lock(&dev_queue);

	/* traverse the socket queue */

	for_each_sk_mbuff(hdr, end, 0 /* dynamic slot size */)
	{
                bool intr = false, xmit_more = true;
		dev_queue_t qid; int copies;
                tx_ret tmp = {0};

		/* because of dynamic slot size, ensure that caplen is not set to 0 */

		if (unlikely(!hdr->caplen)) {
			if (printk_ratelimit())
				printk(KERN_INFO "[PFQ] sk_queue_xmit: zero caplen (BUG!)\n");
			break;
		}

		/* skip the current packet ? */

		qid = PFQ_DEVQ_ID(hdr->ifindex, hdr->queue);
		if (unlikely(PFQ_DEVQ_IS_NULL(qid)))
			continue;

		/* swap queue/device lock if required */

		if (qid != PFQ_DEVQ_DEFAULT &&
		    qid != dev_queue.id) {

			/* unlock the current locked queue */

			if (likely(!PFQ_DEVQ_IS_NULL(dev_queue.id))) {

				pfq_hard_tx_unlock(&dev_queue);
				local_bh_enable();

				/* release the device */
				dev_queue_put(ctx.net, &dev_queue);
			}

			/* try to get the new dev_queue */

			if (dev_queue_get(ctx.net, qid, &dev_queue) < 0)
			{
				if (printk_ratelimit())
					printk(KERN_INFO "[PFQ] sk_queue_xmit: could not lock " PFQ_DEVQ_FMT "!\n", PFQ_DEVQ_ARG(qid));
				continue;
			}

			/* disable bh and lock it */

			local_bh_disable();
			pfq_hard_tx_lock(&dev_queue);
		}

		/* get the max number of copies */

                copies = dev_tx_max_skb_copies(dev_queue.dev, hdr->data.copies);
		batch_cntr += copies;

                /* set the xmit_more */

		if (batch_cntr >= xmit_batch_len) {
			xmit_more = false;
			batch_cntr = 0;
		}

		/* transmit this packet */

		if (likely(netif_running(dev_queue.dev) &&
			netif_carrier_ok(dev_queue.dev))) {

			tmp = __pfq_mbuff_xmit(hdr, &dev_queue, &ctx, copies,
				xmit_more && (Q_SHARED_QUEUE_NEXT_PKTHDR(hdr, 0) < (struct pfq_pkthdr *)end), stop, &intr);

			/* update the return value */

			ret.value += tmp.value;
		}

		if (unlikely(intr))
			break;
	}

	/* unlock the current locked queue */

	pfq_hard_tx_unlock(&dev_queue);
	local_bh_enable();

	/* release the dev_queue */

	dev_queue_put(ctx.net, &dev_queue);

	/* update the local consumer offset */

	txm->cons.off = prod_off;

	/* count the packets left in the shared queue */

	for_each_sk_mbuff(hdr, end, 0)
	{
		/* dynamic slot size: ensure the caplen is non zero! */
		if (unlikely(!hdr->caplen))
			break;
		ret.fail++;
	}

	return ret;
}


/*
 * transmit queues of packets (from a skbuff_queue)...
 */

tx_ret
pfq_skb_queue_xmit(struct pfq_skbuff_queue *skbs, unsigned long long mask, struct net_device *dev, int queue)
{
	struct netdev_queue *txq;
	struct sk_buff *skb;
	int n, last_idx;
	tx_ret ret = {0};

	/* get txq and fix the queue for this batch.
	 *
	 * note: in case the queue is set to any-queue (-1), the driver along the first skb
	 * select the queue */

	last_idx = pfq_skbuff_queue_len(skbs) - 1;

	txq = pfq_netdev_pick_tx(dev, skbs->queue[0], &queue);

	local_bh_disable();
	HARD_TX_LOCK(dev, txq, smp_processor_id());

	for_each_skbuff_bitmask(skbs, skb, n, mask)
	{
		skb_reset_mac_header(skb);
		skb_set_queue_mapping(skb, queue);

		if (likely(!netif_xmit_frozen_or_drv_stopped(txq)) &&
			__pfq_xmit(skb, dev, !( n == last_idx || ((mask & (mask-1)) == 0))) == NETDEV_TX_OK)
			++ret.ok;
		else {
			++ret.fail;
			goto intr;
		}
	}

	HARD_TX_UNLOCK(dev, txq);
	local_bh_enable();
	return ret;

intr:
	/* the ret-i packet is already freed by the driver */

	for_each_skbuff_from(ret.ok + 1, skbs, skb, n) {
		sparse_inc(&memory_stats, os_free);
		kfree_skb(skb);
		++ret.fail;
	}

	HARD_TX_UNLOCK(dev, txq);
	local_bh_enable();
	return ret;
}

/*
 * lazy transmit packet...
 */

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
pfq_skb_queue_lazy_xmit(struct pfq_skbuff_GC_queue *queue, unsigned long long mask, struct net_device *dev, int queue_index)
{
	struct sk_buff __GC * skb;
	int i, n = 0;

	for_each_skbuff_bitmask(queue, skb, i, mask)
	{
		if (pfq_lazy_xmit(skb, dev, queue_index))
			++n;
	}

	return n;
}


int
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

				struct sk_buff *nskb = to_clone ? skb_clone(PFQ_SKB(skb), GFP_ATOMIC) : skb_get(PFQ_SKB(skb));

				if (nskb && __pfq_xmit(nskb, dev, xmit_more) == NETDEV_TX_OK)
					sent++;
				else
					sparse_inc(&global_stats, disc);
			}
		}

		if (txq) {
			HARD_TX_UNLOCK(dev, txq);
			local_bh_enable();
		}
	}

	return sent;
}


static inline
void *pfq_skb_copy_from_linear_data(const struct sk_buff *skb, void *to, size_t len)
{
	if (len < 64 && (len + skb_tailroom(skb) >= 64))
		return memcpy(to, skb->data, 64);
	return memcpy(to, skb->data, len);
}



size_t pfq_sk_rx_queue_recv(struct pfq_sock_opt *opt,
			    struct pfq_skbuff_GC_queue *skbs,
			    unsigned long long mask,
			    int burst_len,
			    pfq_gid_t gid)
{
	struct pfq_rx_queue *rx_queue = pfq_get_rx_queue(opt);
	struct pfq_pkthdr *hdr;
	int data, qlen, qindex;
	struct sk_buff __GC *skb;
	size_t n, sent = 0;

	if (unlikely(rx_queue == NULL))
		return 0;

	data = atomic_read((atomic_t *)&rx_queue->data);

	if (Q_SHARED_QUEUE_LEN(data) > opt->rx_queue_len)
		return 0;

	data = atomic_add_return(burst_len, (atomic_t *)&rx_queue->data);

	qlen = Q_SHARED_QUEUE_LEN(data) - burst_len;
	qindex = Q_SHARED_QUEUE_INDEX(data);
	hdr = (struct pfq_pkthdr *) pfq_mpsc_slot_ptr(opt, rx_queue, qindex, qlen);

	for_each_skbuff_bitmask(skbs, skb, n, mask)
	{
		size_t bytes, slot_index;
		char *pkt;

		bytes = min_t(size_t, skb->len, opt->caplen);
		slot_index = qlen + sent;
		pkt = (char *)(hdr+1);

		if (slot_index > opt->rx_queue_len) {

			if (waitqueue_active(&opt->waitqueue)) {
				wake_up_interruptible(&opt->waitqueue);
			}

			return sent;
		}

		/* copy bytes of packet */

#ifdef PFQ_USE_SKB_LINEARIZE
		if (unlikely(skb_is_nonlinear(PFQ_SKB(skb))))
#else
		if (skb_is_nonlinear(PFQ_SKB(skb)))
#endif
		{
			if (skb_copy_bits(PFQ_SKB(skb), 0, pkt, bytes) != 0) {
				printk(KERN_WARNING "[PFQ] BUG! skb_copy_bits failed (bytes=%zu, skb_len=%d mac_len=%d)!\n",
				       bytes, skb->len, skb->mac_len);
				return 0;
			}
		}
		else {
			pfq_skb_copy_from_linear_data(PFQ_SKB(skb), pkt, bytes);
		}

		/* copy state from pfq_cb annotation */

		hdr->data.mark  = skb->mark;
		hdr->data.state = PFQ_CB(skb)->state;

		/* setup the header */

		if (opt->tstamp != 0) {
			struct timespec ts;
			skb_get_timestampns(PFQ_SKB(skb), &ts);
			hdr->tstamp.tv.sec  = (uint32_t)ts.tv_sec;
			hdr->tstamp.tv.nsec = (uint32_t)ts.tv_nsec;
		}

		hdr->ifindex  = skb->dev->ifindex;
		hdr->gid      = (__force int)gid;
		hdr->len      = (uint16_t)skb->len;
		hdr->caplen   = (uint16_t)bytes;
		hdr->vlan.tci = skb->vlan_tci & ~VLAN_TAG_PRESENT;
		hdr->queue    = skb_rx_queue_recorded(PFQ_SKB(skb)) ? (uint8_t)(skb_get_rx_queue(PFQ_SKB(skb)) & 0xff) : 0;

		/* commit the slot (release semantic) */

		smp_wmb();

		hdr->commit = (uint8_t)qindex;

		if ((slot_index & 8191) == 0 &&
		    waitqueue_active(&opt->waitqueue)) {
			wake_up_interruptible(&opt->waitqueue);
		}

		sent++;

		hdr = Q_SHARED_QUEUE_NEXT_PKTHDR(hdr, opt->rx_slot_size);
	}

	return sent;
}

