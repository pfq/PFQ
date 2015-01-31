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

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/version.h>
#include <linux/sched.h>

#include <linux/skbuff.h>
#include <linux/netdevice.h>

#include <pf_q-thread.h>
#include <pf_q-transmit.h>
#include <pf_q-memory.h>
#include <pf_q-sock.h>
#include <pf_q-macro.h>
#include <pf_q-global.h>
#include <pf_q-GC.h>

#include <pf_q-printk.h>

static inline int
__pfq_xmit(struct sk_buff *skb, struct net_device *dev, struct netdev_queue *txq, int xmit_more);


static inline int
__pfq_dev_cap_txqueue(struct net_device *dev, int hw_queue)
{
	if (unlikely(hw_queue >= dev->real_num_tx_queues))
		return 0;

	return hw_queue;
}


#if (LINUX_VERSION_CODE > KERNEL_VERSION(3,13,0))
static u16 __pfq_pick_tx_default(struct net_device *dev, struct sk_buff *skb)
{
	return 0;
}
#endif


/* select the right tx hw queue, and fix it (-1 means any queue) */

static struct netdev_queue *
pfq_pick_tx(struct net_device *dev, struct sk_buff *skb, int *hw_queue)
{
	if (dev->real_num_tx_queues != 1 && *hw_queue == -1) {

		const struct net_device_ops *ops = dev->netdev_ops;

		*hw_queue = ops->ndo_select_queue ?
#if (LINUX_VERSION_CODE < KERNEL_VERSION(3,13,0))
			ops->ndo_select_queue(dev, skb)
#elif (LINUX_VERSION_CODE == KERNEL_VERSION(3,13,0))
			ops->ndo_select_queue(dev, skb, NULL)
#else
			ops->ndo_select_queue(dev, skb, NULL, __pfq_pick_tx_default)
#endif
			: 0;
	}

	*hw_queue = __pfq_dev_cap_txqueue(dev, *hw_queue);

	return netdev_get_tx_queue(dev, *hw_queue);
}


static struct netdev_queue *
__pfq_pick_tx(struct net_device *dev, int *hw_queue)
{
	if (*hw_queue == -1 || *hw_queue >= dev->real_num_tx_queues)
		*hw_queue = 0;

	return netdev_get_tx_queue(dev, *hw_queue);
}


static inline
int giveup_tx(int cpu)
{
	if (cpu == Q_NO_KTHREAD)
		return signal_pending(current);

	return kthread_should_stop();
}


static size_t
batch_drain(struct pfq_skbuff_batch *skbs, struct local_data *local, struct net_device *dev, int hw_queue)
{
	struct sk_buff *skb;
	int sent, i;

	/* transmit the batch */

	sent = pfq_batch_xmit(skbs, dev, hw_queue);

	/* free the transmitted skb... */

	for_each_skbuff_upto(sent, skbs, skb, i)
		pfq_kfree_skb_pool(skb, &local->tx_pool);

	/* ... discard them from the batch */

	pfq_skbuff_batch_drop_n(skbs, sent);

	/* get unsent skb */

	for_each_skbuff(skbs, skb, i)
		skb_get(skb);

	return sent;
}


int
__pfq_queue_xmit(size_t idx, struct pfq_tx_opt *to, struct net_device *dev, int cpu, int node)
{
	struct pfq_skbuff_short_batch skbs;

	struct pfq_tx_queue *soft_txq;
	struct netdev_queue *txq;

	struct pfq_pkthdr_tx * hdr;
	struct local_data *local;
	size_t len, tot_sent = 0;
	unsigned int n, index;
       	int last_batch_len, hw_queue;

	char *ptr, *end;

	/* get the Tx queue */

	soft_txq = pfq_get_tx_queue(to, idx);

	/* get the netdev_queue for transmission */

	hw_queue = to->queue[idx].hw_queue;

	txq = __pfq_pick_tx(dev, &hw_queue);

	/* swap the soft Tx queue */

	if (cpu != Q_NO_KTHREAD) {
		index = __atomic_add_fetch(&soft_txq->cons, 1, __ATOMIC_RELAXED);
		while (index != __atomic_load_n(&soft_txq->prod, __ATOMIC_RELAXED))
		{
			pfq_relax();
			if (giveup_tx(cpu))
				break;
		}
	}
	else {
		index = __atomic_add_fetch(&soft_txq->cons, 1, __ATOMIC_RELAXED);
		__atomic_store_n(&soft_txq->prod, 1, __ATOMIC_RELAXED);
	}

	index++;

	/* get local cpu data */

	local = __this_cpu_ptr(cpu_data);

        /* initialize pointer to the current transmit queue */

	ptr = to->queue[idx].base_addr + (index & 1) * soft_txq->size;
        end = to->queue[idx].base_addr + 2 * soft_txq->size;

	/* initialize the batch */

	pfq_skbuff_batch_init(SKBUFF_BATCH_ADDR(skbs));

	/* Tx loop */

	hdr = (struct pfq_pkthdr_tx *)ptr;

	for(n = 0; ptr < end && hdr->len != 0; n++, hdr = (struct pfq_pkthdr_tx *)ptr)
	{
		struct sk_buff *skb;

		/* if the batch is full, transmit it */

		if (pfq_skbuff_batch_len(SKBUFF_BATCH_ADDR(skbs)) == batch_len) {

			int sent = batch_drain(SKBUFF_BATCH_ADDR(skbs), local, dev, hw_queue);
			tot_sent += sent;

		       	__sparse_add(&to->stats.sent, sent, cpu);
		       	__sparse_add(&global_stats.sent, sent, cpu);

			/* break the loop when giveup is needed */

			if (sent == 0) {
				pfq_relax();
				if (giveup_tx(cpu)) {
		       			__sparse_add(&to->stats.disc, batch_len, cpu);
		       			__sparse_add(&global_stats.disc, batch_len, cpu);
		       			break;
				}
			}

			continue;
		}

		/* allocate a packet */

	 	skb = pfq_tx_alloc_skb(max_len, GFP_KERNEL, node);
	 	if (unlikely(skb == NULL)) {
	 		printk(KERN_INFO "[PFQ] Tx could not allocate an skb!\n");
	 		break;
		}

	 	/* fill the skb */

	 	len = min_t(size_t, hdr->len, max_len);

	 	skb_reset_tail_pointer(skb);
	 	skb->dev = dev;
	 	skb->len = 0;
	 	__skb_put(skb, len);

	 	skb_get(skb);

		skb_set_queue_mapping(skb, hw_queue);

	 	/* copy bytes in the socket buffer */

		if (skb_is_nonlinear(skb))
	 		skb_store_bits(skb, 0, hdr+1, len);
                else
			skb_copy_to_linear_data(skb, hdr+1, len < 64 ? 64 : len);

                /* transmit packet */

		pfq_skbuff_short_batch_push(SKBUFF_BATCH_ADDR(skbs), skb);

	 	/* move ptr to the next packet */

	 	ptr += sizeof(struct pfq_pkthdr_tx) + ALIGN(hdr->len, 8);
	}

	/* send the last batch */

	while ((last_batch_len=pfq_skbuff_batch_len(SKBUFF_BATCH_ADDR(skbs)))) {

		int sent = batch_drain(SKBUFF_BATCH_ADDR(skbs), local, dev, hw_queue);
		tot_sent += sent;

		__sparse_add(&to->stats.sent, sent, cpu);
		__sparse_add(&global_stats.sent, sent, cpu);

		/* break the loop when giveup is needed */

		if (sent == 0) {
			pfq_relax();
			if (giveup_tx(cpu)) {
				__sparse_add(&to->stats.disc, batch_len, cpu);
				__sparse_add(&global_stats.disc, batch_len, cpu);
				break;
			}
		}
	}

	/* update stat for discarded packets */

	for(n = 0; ptr < end && hdr->len != 0; n++, hdr = (struct pfq_pkthdr_tx *)ptr)
	{
		ptr += sizeof(struct pfq_pkthdr_tx) + ALIGN(hdr->len, 8);
	}

	__sparse_add(&to->stats.disc, n, cpu);
	__sparse_add(&global_stats.disc, n, cpu);

	/* clear the queue */

	ptr = to->queue[idx].base_addr + (index & 1) * soft_txq->size;
	hdr = (struct pfq_pkthdr_tx *)ptr;
        hdr->len = 0;

	return tot_sent;
}


/*
 * flush the logic queue or wakeup the related kernel thread
 */

int
pfq_queue_flush_or_wakeup(struct pfq_sock *so, int index)
{
	struct net_device *dev;

	if (so->tx_opt.queue[index].task) {
		wake_up_process(so->tx_opt.queue[index].task);
		return 0;
	}

	dev = dev_get_by_index(sock_net(&so->sk), so->tx_opt.queue[index].if_index);
	if (!dev) {
		printk(KERN_INFO "[PFQ] pfq_queue_flush_or_wakeup[%d]: bad if_index:%d!\n", index, so->tx_opt.queue[index].if_index);
		return -EPERM;
	}

	pfq_queue_xmit(index, &so->tx_opt, dev);
	dev_put(dev);
	return 0;
}


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

	kfree_skb(skb);
	return -ENETDOWN;
out:
	return rc;
}


int
pfq_xmit(struct sk_buff *skb, struct net_device *dev, int hw_queue, int more)
{
	struct netdev_queue *txq;
	int ret = 0;

	/* get txq and fix the hw_queue for this batch.
	 *
	 * note: in case the hw_queue is set to any-queue (-1), the driver along the first skb
	 * select the queue */

	txq = pfq_pick_tx(dev, skb, &hw_queue);

	skb_set_queue_mapping(skb, hw_queue);

	__netif_tx_lock_bh(txq);

	ret = __pfq_xmit(skb, dev, txq, more);

	__netif_tx_unlock_bh(txq);

	return ret;
}


int
pfq_batch_xmit(struct pfq_skbuff_batch *skbs, struct net_device *dev, int hw_queue)
{
	struct netdev_queue *txq;
	struct sk_buff *skb;
	int n, ret = 0;
	size_t last;

	/* get txq and fix the hw_queue for this batch.
	 *
	 * note: in case the hw_queue is set to any-queue (-1), the driver along the first skb
	 * select the queue */

	txq = pfq_pick_tx(dev, skbs->queue[0], &hw_queue);

	last = pfq_skbuff_batch_len(skbs) - 1;

	__netif_tx_lock_bh(txq);

	for_each_skbuff(skbs, skb, n)
	{
		skb_set_queue_mapping(skb, hw_queue);

		if (__pfq_xmit(skb, dev, txq, n != last) == NETDEV_TX_OK)
			++ret;
		else
			goto intr;
	}

	__netif_tx_unlock_bh(txq);
	return ret;

intr:
	for_each_skbuff_from(ret + 1, skbs, skb, n)
		kfree_skb(skb);

	__netif_tx_unlock_bh(txq);
	return ret;
}


int
pfq_batch_xmit_by_mask(struct pfq_skbuff_batch *skbs, unsigned long long mask, struct net_device *dev, int hw_queue)
{
	struct netdev_queue *txq;
	struct sk_buff *skb;
	int n, ret = 0;

	/* get txq and fix the hw_queue for this batch.
	 *
	 * note: in case the hw_queue is set to any-queue (-1), the driver along the first skb
	 * select the queue */

	txq = pfq_pick_tx(dev, skbs->queue[0], &hw_queue);

	__netif_tx_lock_bh(txq);

	for_each_skbuff_bitmask(skbs, mask, skb, n)
	{
		skb_set_queue_mapping(skb, hw_queue);

		if (__pfq_xmit(skb, dev, txq, 0) == NETDEV_TX_OK)
			++ret;
		else
			goto intr;
	}

	__netif_tx_unlock_bh(txq);
	return ret;

intr:
	for_each_skbuff_from(ret + 1, skbs, skb, n)
		kfree_skb(skb);

	__netif_tx_unlock_bh(txq);
	return ret;
}


int
pfq_lazy_xmit(struct gc_buff buff, struct net_device *dev, int hw_queue)
{
	struct gc_log *log = PFQ_CB(buff.skb)->log;

	if (log->num_devs >= Q_GC_LOG_QUEUE_LEN) {
		if (printk_ratelimit())
			printk(KERN_INFO "[PFQ] bridge %s: too many annotation!\n", dev->name);
		return 0;
	}

	skb_set_queue_mapping(buff.skb, hw_queue);
	log->dev[log->num_devs++] = dev;
	log->xmit_todo++;

	return 1;
}


int
pfq_batch_lazy_xmit(struct gc_queue_buff *queue, struct net_device *dev, int hw_queue)
{
	struct gc_buff buff;
	int i, n = 0;

	for_each_gcbuff(queue, buff, i)
	{
		if (pfq_lazy_xmit(buff, dev, hw_queue))
			++n;
	}

	return n;
}


int
pfq_batch_lazy_xmit_by_mask(struct gc_queue_buff *queue, unsigned long long mask, struct net_device *dev, int hw_queue)
{
	struct gc_buff buff;
	int i, n = 0;

	for_each_gcbuff_bitmask(queue, mask, buff, i)
	{
		if (pfq_lazy_xmit(buff, dev, hw_queue))
			++n;
	}

	return n;
}


size_t
pfq_lazy_xmit_exec(struct gc_data *gc, struct gc_fwd_targets const *t)
{
	struct netdev_queue *txq;
	struct net_device *dev;
	struct sk_buff *skb;
        size_t sent = 0;
	size_t n, i, k;
	int queue;

	/* for each net_device... */

	for(n = 0; n < t->num; n++)
	{
		dev = t->dev[n];
		k = 0; txq = NULL;

		/* scan the list of skbs, and forward them in batch fashion */

		for(i = 0; i < gc->pool.len; i++)
		{
			struct gc_log *log;
                        size_t j, num;

			/* select the packet */

                        log = &gc->log[i];

			num = gc_count_dev_in_log(dev, log);
			if (num == 0)
				continue;

			skb = gc->pool.queue[i].skb;

			/* the first packet for this dev determines the hw queue */

			if (!txq) {
				queue = skb->queue_mapping;
				txq = pfq_pick_tx(dev, skb, &queue);

				__netif_tx_lock_bh(txq);
			}

			/* forward this skb `num` times */

                        for (j = 0; j < num; j++)
			{
				const int xmit_more = ++k != t->cnt[n];

				skb = log->xmit_todo-- > 1 ? skb_clone(skb, GFP_ATOMIC) : skb_get(skb);
				if (skb) {
					if (__pfq_xmit(skb, dev, txq, xmit_more) == NETDEV_TX_OK)
		   				sent++;
				}
			}

		}

		if (txq)
			__netif_tx_unlock_bh(txq);
	}

	return sent;
}

