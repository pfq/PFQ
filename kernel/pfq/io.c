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

#include <lang/engine.h>
#include <lang/symtable.h>

#include <pfq/bitops.h>
#include <pfq/devmap.h>
#include <pfq/global.h>
#include <pfq/io.h>
#include <pfq/memory.h>
#include <pfq/netdev.h>
#include <pfq/percpu.h>
#include <pfq/prefetch.h>
#include <pfq/qbuff.h>
#include <pfq/queue.h>
#include <pfq/sock.h>
#include <pfq/skbuff.h>
#include <pfq/thread.h>
#include <pfq/vlan.h>


#if (LINUX_VERSION_CODE > KERNEL_VERSION(3,13,0))
static uint16_t __pfq_pick_tx_default(struct net_device *dev, struct sk_buff *skb)
{
	return 0;
}
#endif


/* select the right Tx hw queue, and fix it (-1 means any queue).
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
ktime_t wait_until(uint64_t tv64, ktime_t now, struct pfq_dev_queue *dev_queue, atomic_t const *stop, bool *intr)
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
ptrdiff_t acquire_sk_tx_prod_off_by(int index, struct pfq_shared_tx_queue *tx_queue)
{
	return __atomic_load_n((index & 1) ? &tx_queue->prod.off1 : &tx_queue->prod.off0, __ATOMIC_ACQUIRE);
}


static inline
ptrdiff_t maybe_swap_sk_tx_queue(struct pfq_shared_tx_queue *tx_queue, unsigned int *cons_idx_ret)
{
	unsigned int prod_idx = __atomic_load_n(&tx_queue->prod.index, __ATOMIC_ACQUIRE);
	unsigned int cons_idx = __atomic_load_n(&tx_queue->cons.index, __ATOMIC_RELAXED);

	ptrdiff_t prod_off = acquire_sk_tx_prod_off_by(cons_idx, tx_queue);

	if (prod_idx != cons_idx && tx_queue->cons.off == prod_off) /* swap this queue */
	{
		__atomic_store_n(&tx_queue->cons.index, prod_idx, __ATOMIC_RELAXED);
		tx_queue->cons.off = 0;
		*cons_idx_ret = prod_idx;
		return acquire_sk_tx_prod_off_by(prod_idx, tx_queue);
	}
	else
	{
		*cons_idx_ret = cons_idx;
	}

	return prod_off;
}


static inline
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
__pfq_xmit(struct sk_buff *skb, struct net_device *dev, int xmit_more, int retry)
{
	int rc;
#if(LINUX_VERSION_CODE >= KERNEL_VERSION(3,18,0))
	skb->xmit_more = xmit_more;
#else
	skb->mark = xmit_more;
#endif

	do {
		rc = dev->netdev_ops->ndo_start_xmit(skb, dev);
	}
	while ((rc != NETDEV_TX_OK) && retry-- > 0);

	if (!dev_xmit_complete(rc)) {
		kfree_skb(skb);
	}

	return rc;
}



int
pfq_xmit(struct qbuff *buff, struct net_device *dev, int queue, int more)
{
	struct netdev_queue *txq;
	struct sk_buff *skb = QBUFF_SKB(buff);
	int rc = NETDEV_TX_BUSY;

	/* get txq and fix the queue for this batch.
	 *
	 * note: in case the queue is set to any-queue (-1), the driver along the first skb
	 * select the queue */

	txq = pfq_netdev_pick_tx(dev, skb, &queue);

	skb_reset_mac_header(skb);
	skb_set_queue_mapping(skb, queue);

	local_bh_disable();
	HARD_TX_LOCK(dev, txq, smp_processor_id());

	rc = __pfq_xmit(skb, dev, more, global->tx_retry);

	HARD_TX_UNLOCK(dev, txq);
        local_bh_enable();

	return rc;
}


/*
 * transmit a buff with copies
 */

static tx_response_t
__pfq_mbuff_xmit(struct pfq_pkthdr *hdr,
		 const void *buf,
		 size_t len,
		 struct pfq_dev_queue *dev_queue,
		 struct pfq_mbuff_xmit_context *ctx)
{
	struct sk_buff *skb;
        tx_response_t rc = { 0 };

	if (unlikely(!dev_queue->dev))
		return (tx_response_t){.ok = 0, .fail = ctx->copies};

	/* allocate a new socket buffer */

	skb = pfq_alloc_skb_pool( len
				, GFP_ATOMIC
				, ctx->node
				, 1
				, ctx->tx);

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
	skb_copy_to_linear_data(skb, buf, len);

	/* transmit the packet + copies */

	atomic_set(&skb->users, ctx->copies + 1);

	do { /* copies > 1 when the device support TX_SKB_SHARING */

		const bool xmit_more_ = ctx->xmit_more || ctx->copies != 1;

		if (__pfq_xmit(skb, dev_queue->dev, xmit_more_, global->tx_retry) == NETDEV_TX_OK)
			rc.ok++;
		else
			rc.fail++;

		ctx->copies--;
	}
	while (ctx->copies > 0);

	/* release the packet */

	pfq_free_skb_pool(skb, ctx->tx);

	if (rc.ok)
	     dev_queue->queue->trans_start = ctx->jiffies;

	return rc;
}


/*
 * transmit packets from a socket queue..
 */

tx_response_t
pfq_sk_queue_xmit(struct pfq_sock *so,
		  int sock_queue,
		  int cpu,
		  atomic_t const *stop)
{
	struct pfq_txq_info const * txinfo = pfq_sock_get_tx_queue_info(&so->opt, sock_queue);
	struct pfq_dev_queue dev_queue = {.dev = NULL, .queue = NULL, .mapping = 0};
	struct pfq_mbuff_xmit_context ctx;
	struct pfq_percpu_pool *pool;
	int batch_cntr = 0, cons_idx;
	struct pfq_shared_tx_queue *tx_queue;
	struct pfq_pkthdr *hdr;
	ptrdiff_t prod_off;
        char *begin, *end;
        tx_response_t rc = {0};

	/* get the Tx queue descriptor */

	tx_queue = pfq_sock_shared_tx_queue(&so->opt, sock_queue);
	if (unlikely(tx_queue == NULL))
		return rc; /* socket not enabled... */

	/* enable skb_pool for Tx threads */

	pool = this_cpu_ptr(global->percpu_pool);
	ctx.tx = &pool->tx;

	/* lock the Tx pool */

	spin_lock(&pool->tx_lock);
	local_bh_disable();

	if (cpu == Q_NO_KTHREAD) {
		cpu = smp_processor_id();
	}

	/* initialize the boundaries of this queue */

	prod_off = maybe_swap_sk_tx_queue(tx_queue, &cons_idx);
	begin    = txinfo->shmem_addr + (cons_idx & 1) * tx_queue->size + tx_queue->cons.off;
	end      = txinfo->shmem_addr + (cons_idx & 1) * tx_queue->size + prod_off;

        /* setup the context */

        ctx.net	    = sock_net(&so->sk);
	ctx.now	    = ktime_get_real();
	ctx.jiffies = jiffies;
        ctx.node    = cpu == -1 ? NUMA_NO_NODE : cpu_to_node(cpu);
        ctx.stop    = stop;

	/* lock the dev_queue */

	if (pfq_dev_queue_get(ctx.net, txinfo->ifindex, txinfo->queue, &dev_queue) < 0) {
		local_bh_enable();
		spin_unlock(&pool->tx_lock);

		if (printk_ratelimit())
			printk(KERN_INFO "[PFQ] sk_queue_xmit: could not lock the dev_queue!\n");
		return rc;
	}

	/* prefetch packets... */

	hdr  = (struct pfq_pkthdr *)begin;
        prefetch_r3(hdr);
        prefetch_r3((char *)hdr+64);

	/* disable bottom half and lock the queue */

	HARD_TX_LOCK(dev_queue.dev, dev_queue.queue, cpu);

	for_each_sk_mbuff(hdr, end, 0 /* dynamic slot size */)
	{
		struct pfq_pkthdr *next;
                tx_response_t tmp = {0};

		next = PFQ_SHARED_QUEUE_NEXT_VAR_PKTHDR(hdr);
		prefetch_r3(next);
		prefetch_r3((char *)next+64);

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

		ctx.xmit_more = batch_cntr < global->xmit_batch_len ?
				PFQ_SHARED_QUEUE_NEXT_VAR_PKTHDR(hdr) < (struct pfq_pkthdr *)end : (batch_cntr = 0, false);

		/* transmit this packet */

		if (likely(netif_running(dev_queue.dev) && netif_carrier_ok(dev_queue.dev))) {

			size_t len = min_t(size_t, hdr->len, global->xmit_slot_size);
			tmp = __pfq_mbuff_xmit(hdr, hdr+1, len, &dev_queue, &ctx);

			rc.value += tmp.value;
		}
	}

	/* unlock the current queue, enable bottom half */

	HARD_TX_UNLOCK(dev_queue.dev, dev_queue.queue);
	local_bh_enable();

	pfq_dev_queue_put(&dev_queue);
	spin_unlock(&pool->tx_lock);

	/* update the local consumer offset */

	tx_queue->cons.off = prod_off;

	/* count the packets left in the shared queue */

	for_each_sk_mbuff(hdr, end, 0) {
		/* dynamic slot size: ensure the caplen is not zero! */
		if (unlikely(!hdr->caplen))
			break;
		rc.fail++;
	}

	return rc;
}


/*
 * transmit queue of qbuff...
 */

tx_response_t
pfq_qbuff_queue_xmit(struct pfq_qbuff_queue *buffs, unsigned __int128 mask, struct net_device *dev, int queue)
{
	struct netdev_queue *txq;
	struct qbuff *buff;
	int n, last_idx;
	tx_response_t rc = {0};

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

			if (__pfq_xmit(QBUFF_SKB(buff), dev, !( n == last_idx || ((mask & (mask-1)) == 0)), global->tx_retry) == NETDEV_TX_OK)
				++rc.ok;
			else
				++rc.fail;
		}
		else {
			++rc.fail;
			goto intr;
		}
	}

	HARD_TX_UNLOCK(dev, txq);
	local_bh_enable();
	return rc;

intr:
	/* the rc-i packet is already freed by the driver */

	for_each_qbuff_from(rc.ok + 1, buffs, buff, n) {
		sparse_inc(global->percpu_memory, os_free);
		kfree_skb(QBUFF_SKB(buff));
		++rc.fail;
	}

	HARD_TX_UNLOCK(dev, txq);
	local_bh_enable();
	return rc;
}

/*
 * lazy transmit packet...
 */

int
pfq_qbuff_lazy_xmit(struct qbuff * buff, struct net_device *dev, int queue)
{
	if (buff->fwd_dev_num >= Q_BUFF_LOG_LEN) {
		if (printk_ratelimit())
			printk(KERN_INFO "[PFQ] bridge %s: too many annotation!\n", dev->name);
		return 0;
	}

	skb_set_queue_mapping(QBUFF_SKB(buff), queue);

	buff->fwd_dev[buff->fwd_dev_num++] = dev;
	return 1;
}


int
pfq_qbuff_lazy_xmit_run(struct pfq_qbuff_queue *buffs, struct pfq_endpoint_info const *endpoints)
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

			num = pfq_count_fwd_devs(dev, buff->fwd_dev, buff->fwd_dev_num);
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
				if (likely(nskb))
				{
					if (__pfq_xmit(nskb, dev, xmit_more, global->tx_retry) == NETDEV_TX_OK)
						sent++;
					else
						sparse_inc(global->percpu_stats, disc);
				}
			}
		}

		if (txq) {
			HARD_TX_UNLOCK(dev, txq);
			local_bh_enable();
		}
	}

	return sent;
}


///////////////////////////////////////////////////////////////////////////////

/*
 * Find the next power of two.
 * from "Hacker's Delight, Henry S. Warren."
 */

static inline
unsigned clp2(unsigned int x)
{
        x = x - 1;
        x = x | (x >> 1);
        x = x | (x >> 2);
        x = x | (x >> 4);
        x = x | (x >> 8);
        x = x | (x >> 16);
        return x + 1;
}

/*
 * Optimized folding operation...
 */

static inline
uint32_t prefold(uint32_t hash)
{
	return hash ^ (hash >> 8) ^ (hash >> 16) ^ (hash >> 24);
}


static inline
unsigned int pfq_fold(unsigned int a, unsigned int b)
{
	unsigned int c;
	if (b == 1)
		return 0;
        c = b - 1;
        if (likely((b & c) == 0))
		return a & c;
        switch(b)
        {
        case 3:  return a % 3;
        case 5:  return a % 5;
        case 6:  return a % 6;
        case 7:  return a % 7;
        default: {
                const unsigned int p = clp2(b);
                const unsigned int r = a & (p-1);
                return r < b ? r : a % b;
            }
        }
}


int
pfq_receive(struct napi_struct *napi, struct sk_buff * skb)
{
	struct pfq_percpu_data * data;
	struct pfq_percpu_pool * pool;
	unsigned long bit;
	int cpu;

	/* if no socket is open drop the packet */

        cpu = smp_processor_id();

	if (unlikely(pfq_sock_counter() == 0)) {
		if (skb) {
			sparse_inc(global->percpu_memory, os_free);
			pool = per_cpu_ptr(global->percpu_pool, cpu);
			pfq_free_skb_pool(skb, &pool->rx);
		}
		return 0;
	}

	data = per_cpu_ptr(global->percpu_data, cpu);

	if (likely(skb)) /* ensure this is not the timer heartbeat */
	{
		struct pfq_lang_monad monad;
		unsigned long group_mask;
		struct qbuff *buff;

		/* if required, timestamp the packet now */
		if (skb->tstamp.tv64 == 0)
			__net_timestamp(skb);

		/* if vlan header is present, remove it */
		if (global->vlan_untag && skb->protocol == cpu_to_be16(ETH_P_8021Q)) {
			skb = pfq_vlan_untag(skb);
			if (unlikely(!skb)) {
				__sparse_inc(global->percpu_stats, lost, cpu);
				return -1;
			}
		}

		skb_reset_mac_len(skb);

		/* push the mac header: reset skb->data to the beginning of the packet */

		skb_push(skb, skb->mac_len);

		/* initialize the qbuff */

		buff = &data->qbuff_queue->queue[data->qbuff_queue->len];

		qbuff_init(buff
			  , skb
			  , &monad
			  , data->counter++);

		/* get the eligible groups */

		group_mask = pfq_devmap_get_groups( qbuff_get_ifindex(buff)
						  , qbuff_get_rx_queue(buff));


		/* process all groups for this qbuff */

		pfq_bitwise_foreach(group_mask, bit,
		{
			pfq_gid_t gid = (__force pfq_gid_t)pfq_ctz(bit);
			struct pfq_group * this_group = pfq_group_get(gid);

			struct pfq_lang_computation_tree *prg;

			/* increment counter for this group */

			__sparse_inc(this_group->stats, recv, cpu);

			/* check if bp filter is enabled */

			if (atomic_long_read(&this_group->bp_filter)) {
				if (!qbuff_run_bp_filter(buff, this_group)) {
					__sparse_inc(this_group->stats, drop, cpu);
					continue;
				}
			}

			/* check vlan filter */

			if (pfq_group_vlan_filters_enabled(gid)) {
				if (!qbuff_run_vlan_filter(buff, (pfq_gid_t)gid)) {
					__sparse_inc(this_group->stats, drop, cpu);
					continue;
				}
			}

			/* process pfq-lang */

			prg = (struct pfq_lang_computation_tree *)atomic_long_read(&this_group->comp);
			if (prg) {
				unsigned long cbit, elig_mask = 0;
				size_t to_kernel = buff->to_kernel;
				size_t num_fwd = buff->fwd_dev_num;

			 	/* setup monad for this computation */

			 	monad.fanout.class_mask = Q_CLASS_DEFAULT;
			 	monad.fanout.type = fanout_copy;
			 	monad.group = this_group;
			 	monad.state = 0;
			 	monad.shift = 0;
			 	monad.ipoff = 0;
			 	monad.ipproto = IPPROTO_NONE;
			 	monad.ep_ctx = EPOINT_SRC | EPOINT_DST;

			 	/* run the functional program */

			 	if (!pfq_lang_run(buff, prg).qbuff) {
			 		__sparse_inc(this_group->stats, drop, cpu);
			 		continue;
			 	}

			 	/* update stats */

                                 __sparse_add(this_group->stats, frwd, buff->fwd_dev_num - num_fwd, cpu);
                                 __sparse_add(this_group->stats, kern, buff->to_kernel - to_kernel, cpu);

			 	/* skip this packet? */

			 	if (is_drop(monad.fanout)) {
			 		__sparse_inc(this_group->stats, drop, cpu);
			 		continue;
			 	}

			 	/* compute the eligible mask of sockets enabled to receive this packet... */

			 	pfq_bitwise_foreach(monad.fanout.class_mask, cbit,
			 	{
			 		int class = (int)pfq_ctz(cbit);
			 		elig_mask |= (unsigned long)atomic_long_read(&this_group->sock_id[class]);
			 	});


			 	if (is_steering(monad.fanout)) { /* single or double */

			 		unsigned long steer_mask[Q_MAX_STEERING_MASK];
			 		unsigned int sbit, steer_mask_numb = 0;

					/* compute the load balacing mask list */

			 		pfq_bitwise_foreach(elig_mask, sbit,
			 		{
			 			pfq_id_t id = (__force pfq_id_t)pfq_ctz(sbit);
			 			struct pfq_sock * so = pfq_sock_get_by_id(id);
                                                int i;

			 			for(i = 0; i < so->weight; ++i)
			 			 	steer_mask[steer_mask_numb++] = sbit;
			 		});

			 		buff->fwd_mask |= steer_mask[pfq_fold(prefold(monad.fanout.hash), (unsigned int)steer_mask_numb)];

			 		if (is_double_steering(monad.fanout))
			 			buff->fwd_mask |= steer_mask[pfq_fold(prefold(monad.fanout.hash2), (unsigned int)steer_mask_numb)];
			 	}
			 	else {  /* broadcast */

			 		buff->fwd_mask |= elig_mask;
			 	}

			} else {
				buff->fwd_mask |= (unsigned long)atomic_long_read(&this_group->sock_id[0]);
			}
		}
		);

		/* this packet is ready to be enqued for transmission or dropped */

		if (buff->fwd_mask || buff->fwd_dev_num || buff->to_kernel)
		{
			/* commit this buff to the queue */

			data->qbuff_queue->len++;
		}

		/* transmit the queue or wait for the next packet? */

		if (data->qbuff_queue->len < (size_t)global->capt_batch_len &&
		     ktime_to_ns(ktime_sub(qbuff_get_ktime(buff), data->last_rx)) < 1000000) {
			return 0;
		}

		data->last_rx = qbuff_get_ktime(buff);
	}
	else {
		if (data->qbuff_queue->len == 0)
			return 0;
	}

	/* run IO now */

	__sparse_add(global->percpu_stats, recv, data->qbuff_queue->len, cpu);

	return pfq_receive_run( data
			      , per_cpu_ptr(global->percpu_pool, cpu)
			      , cpu);
}



int pfq_receive_run( struct pfq_percpu_data *data
		   , struct pfq_percpu_pool *pool
		   , int cpu)
{
	unsigned __int128 socket_mask[Q_MAX_ID] = { 0 };
	unsigned long long all_fwd_mask = 0;
	size_t n, len = data->qbuff_queue->len;
	struct pfq_endpoint_info endpoints;
        struct qbuff *buff;
        unsigned int bit;

#if 1
	/* transpose the forward matrix */

	for(n = 0; n < len; n++)
	{
		buff = &data->qbuff_queue->queue[n];
		all_fwd_mask |= buff->fwd_mask;
		pfq_bitwise_foreach(buff->fwd_mask, bit,
		{
			int index = (int)pfq_ctz(bit);
			socket_mask[index] |= (unsigned __int128)1 << n;
		})
	}

        /* forward packets to endpoints */

	pfq_bitwise_foreach(all_fwd_mask, bit,
	{
		pfq_id_t id = (__force pfq_id_t)pfq_ctz(bit);
		struct pfq_sock *so = pfq_sock_get_by_id(id);

		pfq_copy_to_endpoint_qbuffs(so, PFQ_QBUFF_QUEUE(data->qbuff_queue), socket_mask[(int __force)id], cpu);
	});

	/* forward packets to device */

	pfq_get_lazy_endpoints(PFQ_QBUFF_QUEUE(data->qbuff_queue), &endpoints);
	if (endpoints.cnt_total)
	{
		size_t total = (size_t)pfq_qbuff_lazy_xmit_run(PFQ_QBUFF_QUEUE(data->qbuff_queue), &endpoints);
		__sparse_add(global->percpu_stats, frwd, total, cpu);
		__sparse_add(global->percpu_stats, disc, endpoints.cnt_total - total, cpu);
	}

	/* forward packats to kernel and release them */

	for_each_qbuff(PFQ_QBUFF_QUEUE(data->qbuff_queue), buff, n)
	{
		if (fwd_to_kernel(buff)) {

			bool peeked = QBUFF_SKB(buff)->peeked;

			qbuff_move_or_copy_to_kernel(buff, GFP_KERNEL);

			/* only if peeked we need to free/recycle the qbuff/skb */
			if (peeked)
				qbuff_free(buff, &pool->rx);

			__sparse_inc(global->percpu_stats, kern, cpu);
		}
		else {
			/* Peeked or not, always free the qbuff/skb here */
			qbuff_free(buff, &pool->rx);
		}
	}

#else
	/* release the qbuff */

	for(n = 0; n < len; n++)
	{
		struct qbuff *buff = &data->qbuff_queue->queue[n];
		qbuff_free(buff, &pool->rx);
	}

#endif

	data->qbuff_queue->len = 0;
	return 0;
}



static inline
int pfq_copy_bits(const struct sk_buff *skb, int offset, void *to, int len)
{
	int data = skb_headroom(skb);
	int end = pfq_skb_end_offset(skb);

	if (likely(len <= (end - data - offset))) {
		skb_copy_from_linear_data_offset(skb, offset, to, len);
		return 0;
	}

	return skb_copy_bits(skb, offset, to, len);
}


size_t pfq_sk_queue_recv(struct pfq_sock_opt *opt,
			 struct pfq_qbuff_queue *buffs,
			 unsigned __int128 mask,
			 int burst_len)
{
	struct pfq_shared_rx_queue *rx_queue = pfq_sock_shared_rx_queue(opt);
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

	hdr  = (struct pfq_pkthdr *) pfq_mpsc_slot_ptr(opt, rx_queue, qver, qlen);

	for_each_qbuff_with_mask(mask, buffs, buff, n)
	{
		struct sk_buff *skb = QBUFF_SKB(buff);
		struct pfq_pkthdr lhdr;
		size_t bytes, slot_index;
		char *pkt;

		/* compute the boundaries */

		bytes = min_t(size_t, skb->len, opt->caplen);
		pkt = (char *)(hdr+1);
		slot_index = qlen + copied;

		prefetch_r2(skb->data);
		prefetch_w0(hdr);
		prefetch_w0((char *)hdr + 64);

		if (unlikely(slot_index >= opt->rx_queue_len)) {
#ifdef PFQ_USE_POLL
			if (waitqueue_active(&opt->waitqueue)) {
				wake_up_interruptible(&opt->waitqueue);
			}
#endif
			return copied;
		}


		/* copy bytes of packet */
#if 1
		if (pfq_copy_bits(skb, 0, pkt, bytes) != 0) {
			printk(KERN_WARNING "[PFQ] error: BUG! skb_copy_bits failed (bytes=%zu, skb_len=%d mac_len=%d)!\n",
			       bytes, skb->len, skb->mac_len);
			return copied;
		}
#else
		skb_copy_from_linear_data_offset(skb, 0, pkt, bytes);
#endif

		/* fill pkt header */

		if (likely(opt->tstamp != 0)) {
			struct timespec ts;
			skb_get_timestampns(skb, &ts);
			lhdr.tstamp.tv.sec  = (uint32_t)ts.tv_sec;
			lhdr.tstamp.tv.nsec = (uint32_t)ts.tv_nsec;
		}

		lhdr.caplen = (uint16_t)bytes;
		lhdr.len = (uint16_t)skb->len;

		/* copy state from pfq_cb annotation */

		lhdr.info.data.mark  = skb->mark;

		/* setup the header */

		lhdr.info.ifindex = skb->dev->ifindex;
		lhdr.info.vlan.tci = skb->vlan_tci & ~VLAN_TAG_PRESENT;
		lhdr.info.queue	= skb_rx_queue_recorded(skb) ? (uint16_t)skb_get_rx_queue(skb) : 0;

		/* commit the slot (release semantic) */

                __builtin_memcpy(hdr, &lhdr, sizeof(struct pfq_pkthdr));
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

