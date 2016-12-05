/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#include <core/core.h>
#include <core/percpu.h>
#include <core/global.h>
#include <core/devmap.h>

#include <core/lang/engine.h>
#include <core/lang/symtable.h>

#include <core/queue.h>
#include <core/bitops.h>
#include <core/qbuff.h>
#include <core/GC.h>

#include <pfq/io.h>
#include <pfq/vlan.h>
#include <pfq/thread.h>
#include <pfq/memory.h>
#include <pfq/qbuff.h>
#include <pfq/netdev.h>
#include <pfq/prefetch.h>


/*
 * Packet Tx 
 */

#if (LINUX_VERSION_CODE > KERNEL_VERSION(3,13,0))
static uint16_t __pfq_pick_tx_default(struct net_device *dev, struct sk_buff *skb)
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


#if 0

/*
 * wait function for active timestamping
 *
 */


static inline
bool giveup_tx_process(atomic_t const *stop)
{
	return atomic_read(stop) == -1 || signal_pending(current) || is_kthread_should_stop();
}

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
		if (!global->tx_rate_control_eager)
			return now;
	}

	return now_;
}
#endif


static inline
ptrdiff_t acquire_sk_tx_prod_off_by(int index, struct pfq_tx_queue *txm)
{
	return __atomic_load_n((index & 1) ? &txm->prod.off1 : &txm->prod.off0, __ATOMIC_ACQUIRE);
}


static inline
ptrdiff_t maybe_swap_sk_tx_queue(struct pfq_tx_queue *txm, unsigned int *cons_idx_ret)
{
	unsigned int prod_idx = __atomic_load_n(&txm->prod.index, __ATOMIC_ACQUIRE);
	unsigned int cons_idx = __atomic_load_n(&txm->cons.index, __ATOMIC_RELAXED);

	ptrdiff_t prod_off = acquire_sk_tx_prod_off_by(cons_idx, txm);

	if (prod_idx != cons_idx && txm->cons.off == prod_off) /* swap this queue */
	{
		__atomic_store_n(&txm->cons.index, prod_idx, __ATOMIC_RELAXED);
		txm->cons.off = 0;
		*cons_idx_ret = prod_idx;
		return acquire_sk_tx_prod_off_by(prod_idx, txm);
	}
	else
	{
		*cons_idx_ret = cons_idx;
	}

	return prod_off;
}


static inline
struct sk_buff *
skb_clone_for_tx(struct sk_buff *skb, struct net_device *dev, gfp_t pri)
{
	if (likely(dev->priv_flags & IFF_TX_SKB_SHARING))
		return skb_get(skb);

	return skb_clone(skb, pri);
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

	if (unlikely(req_copies > Q_CORE_MAX_TX_SKB_COPY))
		return Q_CORE_MAX_TX_SKB_COPY;

	return req_copies;
}


/*
 *  single packet transmission...
 */

static inline int
__pfq_xmit(struct sk_buff *skb, struct net_device *dev, int xmit_more)
{
	int ret;

#if(LINUX_VERSION_CODE >= KERNEL_VERSION(3,18,0))
	skb->xmit_more = xmit_more;
#else
	skb->mark = xmit_more;
#endif

	ret = dev->netdev_ops->ndo_start_xmit(skb, dev);
	if (dev_xmit_complete(ret)) {
		return ret;
	}

	kfree_skb(skb);
	return ret;
}



int
pfq_xmit(struct qbuff *buff, struct net_device *dev, int queue, int more)
{
	struct netdev_queue *txq;
	struct sk_buff *skb = QBUFF_SKB(buff);
	int ret = NETDEV_TX_BUSY;

	/* get txq and fix the queue for this batch.
	 *
	 * note: in case the queue is set to any-queue (-1), the driver along the first skb
	 * select the queue */

	txq = pfq_netdev_pick_tx(dev, skb, &queue);

	skb_reset_mac_header(skb);
	skb_set_queue_mapping(skb, queue);

	local_bh_disable();
	HARD_TX_LOCK(dev, txq, smp_processor_id());

	if (!netif_xmit_frozen_or_drv_stopped(txq))
		ret = __pfq_xmit(skb, dev, more);

	HARD_TX_UNLOCK(dev, txq);
        local_bh_enable();

	return ret;
}


/*
 * transmit a mbuff packet with copies
 */

static tx_response_t
__pfq_mbuff_xmit(struct pfq_pkthdr *hdr,
		 const void *buf,
		 size_t len,
		 struct net_dev_queue *dev_queue,
		 struct pfq_mbuff_xmit_context *ctx)
{
	struct sk_buff *skb;
        tx_response_t ret = { 0 };

	if (unlikely(!dev_queue->dev))
		return (tx_response_t){.ok = 0, .fail = ctx->copies};

#if 0
	/* wait until for the timestap to expire (if specified) */
	if (hdr->tstamp.tv64) {
		ctx->now = wait_until(hdr->tstamp.tv64, ctx->now, dev_queue, ctx->stop, ctx->intr);
		if (*ctx->intr)
			return (tx_response_t){.ok = 0, .fail = ctx->copies};
	}
#endif

	/* allocate a new socket buffer */

	skb = pfq_alloc_skb_pool( len
				, GFP_ATOMIC
				, ctx->node
				, ctx->pools);
	if (unlikely(skb == NULL)) {
		if (printk_ratelimit())
			printk(KERN_INFO "[PFQ] Tx could not allocate an skb!\n");
		return (tx_response_t){.ok = 0, .fail = ctx->copies};
	}

	/* fill the socket buffer */

	skb_reserve(skb, LL_RESERVED_SPACE(dev_queue->dev));

	skb_reset_tail_pointer(skb);
	skb->dev = dev_queue->dev;
	skb->len = 0;

	__skb_put(skb, len);

	/* set the Tx queue */

	skb_set_queue_mapping(skb, dev_queue->mapping);

	/* prepare the payload... */

	skb_copy_to_linear_data(skb, buf, len);

	/* transmit the packet + copies */

	atomic_set(&skb->users, ctx->copies + 1);

	do {
		/* copies > 1 when the device support TX_SKB_SHARING */

		const bool xmit_more_ = ctx->xmit_more || ctx->copies != 1;

		if (!netif_xmit_frozen_or_drv_stopped(dev_queue->queue)) {
			if (__pfq_xmit(skb, dev_queue->dev, xmit_more_) == NETDEV_TX_OK)
				ret.ok++;
			else
				ret.fail++;
		}
		else {
			kfree_skb(skb);
			ret.fail++;
		}

		ctx->copies--;
	}
	while (ctx->copies > 0);

	/* release the packet */

	pfq_kfree_skb_pool(skb, ctx->pools);

	if (ret.ok)
		dev_queue->queue->trans_start = ctx->jiffies;

	return ret;
}


/*
 * transmit a queue of packets (from socket queue)...
 */

tx_response_t
pfq_sk_queue_xmit(struct core_sock *so,
		  int sock_queue,
		  int cpu,
		  atomic_t const *stop)
{
	struct core_tx_info const * txinfo = core_sock_get_tx_queue_info(&so->opt, sock_queue);
	struct net_dev_queue dev_queue = {.dev = NULL, .queue = NULL, .mapping = 0};
	struct pfq_mbuff_xmit_context ctx;
	struct pfq_percpu_pool *pool;
	int batch_cntr = 0, cons_idx;
	struct pfq_tx_queue *txm;
	struct pfq_pkthdr *hdr, *hdr1;
	ptrdiff_t prod_off;
        char *begin, *end;
        tx_response_t ret = {0};


	/* get the Tx queue descriptor */

	txm = core_sock_get_tx_queue(&so->opt, sock_queue);
	if (unlikely(txm == NULL))
		return ret; /* socket not enabled... */

	/* enable skb_pool for Tx threads */

	pool = this_cpu_ptr(global->percpu_pool);

	ctx.pools = likely(atomic_read(&pool->enable)) ? &pool->tx_multi : NULL;

	/* lock the Tx pool */

	if (ctx.pools) {
		spin_lock(&pool->tx_lock);
	}

	if (cpu == Q_NO_KTHREAD) {
		cpu = smp_processor_id();
	}

	/* initialize the boundaries of this queue */

	prod_off = maybe_swap_sk_tx_queue(txm, &cons_idx);
	begin    = txinfo->shmem_addr + (cons_idx & 1) * txm->size + txm->cons.off;
	end      = txinfo->shmem_addr + (cons_idx & 1) * txm->size + prod_off;

        /* setup the context */

        ctx.net	    = sock_net(&so->sk);
	ctx.now	    = ktime_get_real();
	ctx.jiffies = jiffies;
        ctx.node    = cpu == -1 ? NUMA_NO_NODE : cpu_to_node(cpu);
        ctx.intr    = false;
        ctx.stop    = stop;

	/* lock the dev_queue */

	if (pfq_dev_queue_get(ctx.net, txinfo->def_ifindex, txinfo->def_queue, &dev_queue) < 0) {

		if (ctx.pools)
			spin_unlock(&pool->tx_lock);

		if (printk_ratelimit())
			printk(KERN_INFO "[PFQ] sk_queue_xmit: could not lock default device!\n");

		return ret;
	}

	/* prefetch packets... */

	hdr  = (struct pfq_pkthdr *)begin;
        prefetch_r3(hdr);
        prefetch_r3((char *)hdr+64);

	hdr1 = PFQ_SHARED_QUEUE_NEXT_VAR_PKTHDR(hdr);
        prefetch_r3(hdr1);
        prefetch_r3((char *)hdr1+64);

	/* disable bottom half and lock the queue */

	local_bh_disable();
	HARD_TX_LOCK(dev_queue.dev, dev_queue.queue, cpu);

	for_each_sk_mbuff(hdr, end, 0 /* dynamic slot size */)
	{
                tx_response_t tmp = {0};

		hdr1 = PFQ_SHARED_QUEUE_NEXT_VAR_PKTHDR(hdr1);
                prefetch_r3(hdr1);
                prefetch_r3((char *)hdr1+64);

		/* because of dynamic slot size, ensure the caplen is not set to 0 */

		if (unlikely(!hdr->len)) {
			if (printk_ratelimit())
				printk(KERN_INFO "[PFQ] sk_queue_xmit: zero caplen (BUG!)\n");
			break;
		}

		/* get the number of copies to transmit */

                ctx.copies = dev_tx_max_skb_copies(dev_queue.dev, hdr->info.data.copies);
		batch_cntr += ctx.copies;

                /* set the xmit_more bit */

		ctx.xmit_more = likely(batch_cntr < global->xmit_batch_len) ?
				likely(PFQ_SHARED_QUEUE_NEXT_VAR_PKTHDR(hdr) < (struct pfq_pkthdr *)end) :
				(batch_cntr = 0, false);

		/* transmit this packet */

		if (likely(netif_running(dev_queue.dev) && netif_carrier_ok(dev_queue.dev))) {

			size_t len = min_t(size_t, hdr->len, global->xmit_slot_size);

			tmp = __pfq_mbuff_xmit(hdr, hdr+1, len, &dev_queue, &ctx);

			/* update the return value */
			ret.value += tmp.value;
		}

		if (unlikely(ctx.intr))
			break;
	}


	/* unlock the current queue, enable bottom half */

	HARD_TX_UNLOCK(dev_queue.dev, dev_queue.queue);
	local_bh_enable();


	/* release the dev_queue */

	pfq_dev_queue_put(&dev_queue);

	/* unlock the Tx pool... */

	if (ctx.pools);
		spin_unlock(&pool->tx_lock);


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
 * transmit queue of qbuff...
 */

tx_response_t
pfq_qbuff_queue_xmit(struct core_qbuff_queue *buffs, unsigned long long mask, struct net_device *dev, int queue)
{
	struct netdev_queue *txq;
	struct qbuff *buff;
	int n, last_idx;
	tx_response_t ret = {0};

	/* get txq and fix the queue for this batch.
	 *
	 * note: in case the queue is set to any-queue (-1), the driver along the first skb
	 * select the queue */

	last_idx = buffs->len - 1;

	txq = pfq_netdev_pick_tx(dev, QBUFF_SKB(&buffs->queue[0]), &queue);

	local_bh_disable();
	HARD_TX_LOCK(dev, txq, smp_processor_id());

	for_each_qbuff_with_mask(mask, buffs, buff, n)
	{
		skb_reset_mac_header(QBUFF_SKB(buff));
		skb_set_queue_mapping(QBUFF_SKB(buff), queue);

		if (likely(!netif_xmit_frozen_or_drv_stopped(txq))) {

			if (__pfq_xmit(QBUFF_SKB(buff), dev, !( n == last_idx || ((mask & (mask-1)) == 0))) == NETDEV_TX_OK)
				++ret.ok;
			else
				++ret.fail;
		}
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

	for_each_qbuff_from(ret.ok + 1, buffs, buff, n) {
		sparse_inc(global->percpu_mem_stats, os_free);
		kfree_skb(QBUFF_SKB(buff));
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
pfq_qbuff_lazy_xmit(struct qbuff * buff, struct net_device *dev, int queue)
{
	struct GC_log *buff_log = buff->log;

	if (buff_log->num_devs >= Q_CORE_BUFF_LOG_LEN) {
		if (printk_ratelimit())
			printk(KERN_INFO "[PFQ] bridge %s: too many annotation!\n", dev->name);
		return 0;
	}

	skb_set_queue_mapping(QBUFF_SKB(buff), queue);

	buff_log->dev[buff_log->num_devs++] = dev;
	buff_log->xmit_todo++;

	return 1;
}


int
pfq_qbuff_lazy_xmit_run(struct core_qbuff_queue *buffs, struct core_endpoint_info const *endpoints)
{
	struct netdev_queue *txq;
	struct net_device *dev;
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

		/* scan the list of buffs, and forward them in batch fashion */

		for(i = 0; i < buffs->len; i++)
		{
                        size_t j, num;

			struct qbuff * buff = &buffs->queue[i];
			struct sk_buff *skb = QBUFF_SKB(buff);

			num = GC_count_dev_in_log(dev, buff->log);
			if (num == 0)
				continue;

			if (queue != skb->queue_mapping) {

				queue = skb->queue_mapping;

				if (txq) {
					HARD_TX_UNLOCK(dev, txq);
					local_bh_enable();
                                }

				txq = pfq_netdev_pick_tx(dev, skb, &queue);

				local_bh_disable();
				HARD_TX_LOCK(dev, txq, smp_processor_id());
			}

			/* forward this skb `num` times (to this device) */

			for (j = 0; j < num; j++)
			{
				const int xmit_more = ++sent_dev != endpoints->cnt[n];
				struct sk_buff *nskb = skb_clone_for_tx(skb, dev, GFP_ATOMIC);

				if (nskb && __pfq_xmit(nskb, dev, xmit_more) == NETDEV_TX_OK)
					sent++;
				else
					sparse_inc(global->percpu_stats, disc);
			}
		}

		if (txq) {
			HARD_TX_UNLOCK(dev, txq);
			local_bh_enable();
		}
	}

	return sent;
}


/*
 * Packet(s) Rx...
 */


int
pfq_rx_run( int cpu
	  , int budget
	  , struct core_percpu_data *data
	  , struct pfq_percpu_pool *pool)
{
	struct sk_buff *skb;
	int work = 0;

	/* if no socket is open drop the packet */

	while(work < budget &&
	      (skb = core_spsc_pop(data->rx_fifo)))
	{
		struct qbuff * buff;

		++work;

		if (unlikely(core_sock_get_socket_count() == 0)) {
			if (skb) {
				sparse_inc(global->percpu_mem_stats, os_free);
				pfq_kfree_skb_pool(skb, &pool->rx_multi);
			}
			continue;
		}

		/* if vlan header is present, remove it */

		if (global->vlan_untag && skb->protocol == cpu_to_be16(ETH_P_8021Q)) {
			skb = pfq_vlan_untag(skb);
			if (unlikely(!skb)) {
				__sparse_inc(global->percpu_stats, lost, cpu);
				continue;
			}
		}

		skb_reset_mac_len(skb);

		/* push the mac header: reset skb->data to the beginning of the packet */

		if (likely(skb->pkt_type != PACKET_OUTGOING))
		    skb_push(skb, skb->mac_len);


		/* pass the ownership of this skb to the garbage collector */

		buff = GC_make_buff(data->GC, skb);
		if (unlikely(buff == NULL)) {
			if (printk_ratelimit())
				printk(KERN_INFO "[PFQ] GC: memory exhausted!\n");
			__sparse_inc(global->percpu_stats, lost, cpu);
			__sparse_inc(global->percpu_mem_stats, os_free, cpu);
			pfq_kfree_skb_pool(skb, &pool->rx_multi);
			continue;
		}

		/* push another skb to GC? */

		if ((GC_size(data->GC) < (size_t)global->capt_batch_len))
			continue;

		core_process_batch( data
				  , per_cpu_ptr(global->percpu_sock, cpu)
				  , pool
				  , data->GC
				  , cpu);
	}

	return work;
}



int
pfq_receive(struct napi_struct *napi, struct sk_buff * skb, int direct)
{
	struct core_percpu_data * data;

	int cpu;
	(void)napi;

	cpu = smp_processor_id();
	data = per_cpu_ptr(global->percpu_data, cpu);

	/* if required, timestamp the packet now */

	if (likely(skb))
	{
#if 0
		if (skb->tstamp.tv64 == 0)
			__net_timestamp(skb);
#endif

		PFQ_CB(skb)->direct = direct;

		if (!core_spsc_push(data->rx_fifo, skb)) {
			sparse_inc(global->percpu_mem_stats, os_free);
			kfree_skb(skb);
			return 0;
		}

		if (likely(core_spsc_len(data->rx_fifo) < (size_t)global->capt_batch_len)) {
			return 0;
		}
	}

	if (data->rx_napi)
		pfq_rx_run(cpu, 0, data, per_cpu_ptr(global->percpu_pool, cpu));

	return 0;
}


static inline
void *pfq_skb_copy_from_linear_data(const struct sk_buff *skb, void *to, size_t len)
{
	if (len < 64 && (len + skb_tailroom(skb) >= 64))
		return memcpy(to, skb->data, 64);
	return memcpy(to, skb->data, len);
}


size_t pfq_sk_queue_recv(struct core_sock_opt *opt,
			 struct core_qbuff_refs *buffs,
			 unsigned long long mask,
			 int burst_len,
			 pfq_gid_t gid)
{
	struct pfq_rx_queue *rx_queue = core_sock_get_rx_queue(opt);
	struct pfq_pkthdr *hdr;
	struct qbuff *buff;
	unsigned long data;
	size_t n, copied = 0;
	pfq_qver_t qver;
	int qlen;

	if (unlikely(rx_queue == NULL))
		return 0;

	data = __atomic_fetch_add(&rx_queue->shinfo, burst_len, __ATOMIC_RELAXED);
	qlen = PFQ_SHARED_QUEUE_LEN(data);
	qver = PFQ_SHARED_QUEUE_VER(data);

	hdr  = (struct pfq_pkthdr *) core_mpsc_slot_ptr(opt, rx_queue, qver, qlen);

	for_each_qbuff_with_mask(mask, buffs, buff, n)
	{
		struct sk_buff *skb = QBUFF_SKB(buff);
		size_t bytes, slot_index;
		char *pkt;

		/* prefetch skb data that is to be copied soon */

		if (likely(skb)) {
			prefetch_r3(skb->data);
			prefetch_r3((char *)skb->data+64);
		}

		/* compute the boundaries */

		bytes = min_t(size_t, skb->len, opt->caplen);
		pkt = (char *)(hdr+1);
		slot_index = qlen + copied;

		if (unlikely(slot_index >= opt->rx_queue_len)) {
#ifdef PFQ_USE_POLL
			if (waitqueue_active(&opt->waitqueue)) {
				wake_up_interruptible(&opt->waitqueue);
			}
#endif
			return copied;
		}


		/* copy bytes of packet */

#ifdef PFQ_USE_SKB_LINEARIZE
		if (unlikely(skb_is_nonlinear(skb)))
#else
		if (skb_is_nonlinear(skb))
#endif
		{

		if (skb_copy_bits(skb, 0, pkt, bytes) != 0) {
			printk(KERN_WARNING "[PFQ] BUG! skb_copy_bits failed (bytes=%zu, skb_len=%d mac_len=%d)!\n",
			       bytes, skb->len, skb->mac_len);
			return copied;
		}

		}
		else {
			pfq_skb_copy_from_linear_data(skb, pkt, bytes);
		}

		/* fill pkt header */

		if (likely(opt->tstamp != 0)) {
			struct timespec ts;
			skb_get_timestampns(skb, &ts);
			hdr->tstamp.tv.sec  = (uint32_t)ts.tv_sec;
			hdr->tstamp.tv.nsec = (uint32_t)ts.tv_nsec;
		}

		hdr->caplen = (uint16_t)bytes;
		hdr->len = (uint16_t)skb->len;

		/* copy state from pfq_cb annotation */

		hdr->info.data.mark  = skb->mark;
		hdr->info.data.state = buff->state;

		/* setup the header */

		hdr->info.ifindex = skb->dev->ifindex;
		hdr->info.gid = (__force uint16_t)gid;
		hdr->info.vlan.tci = skb->vlan_tci & ~VLAN_TAG_PRESENT;
		hdr->info.queue	= skb_rx_queue_recorded(skb) ? (uint16_t)skb_get_rx_queue(skb) : 0;

		/* commit the slot (release semantic) */

		__atomic_store_n(&hdr->info.commit, qver, __ATOMIC_RELEASE);

		/* check for pending waitqueue... */

#ifdef PFQ_USE_POLL
		if ((slot_index & 127) == 0 &&
		    waitqueue_active(&opt->waitqueue)) {
			wake_up_interruptible(&opt->waitqueue);
		}
#endif

		copied++;

		hdr = PFQ_SHARED_QUEUE_NEXT_FIX_PKTHDR(hdr, opt->rx_slot_size);
	}

	return copied;
}

