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

#include <linux/skbuff.h>
#include <linux/netdevice.h>

#include <pf_q-transmit.h>
#include <pf_q-memory.h>
#include <pf_q-sock.h>
#include <pf_q-macro.h>
#include <pf_q-global.h>
#include <pf_q-GC.h>

static inline u16
__pfq_dev_cap_txqueue(struct net_device *dev, u16 hw_queue)
{
        if (unlikely(hw_queue >= dev->real_num_tx_queues))
                return 0;

        return hw_queue;
}

#if (LINUX_VERSION_CODE > KERNEL_VERSION(3,13,0))
static u16 __pfq_pick_tx(struct net_device *dev, struct sk_buff *skb)
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
                                ops->ndo_select_queue(dev, skb, NULL, __pfq_pick_tx)
#endif
                                : 0;
        }

        *hw_queue = __pfq_dev_cap_txqueue(dev, *hw_queue);

        return netdev_get_tx_queue(dev, *hw_queue);
}


static int
__pfq_queue_xmit_batch(size_t qindex, struct pfq_skbuff_batch *skbs, struct net_device *dev, struct pfq_tx_opt *to, int cpu, struct local_data *local)
{
	struct pfq_tx_queue_hdr *txq = pfq_get_tx_queue_hdr(to, qindex);

	struct sk_buff *skb;
	int len, sent, i;

#ifdef PFQ_TX_PROFILE
	cycles_t start = get_cycles();
#endif

	/* get length of this batch */

	len  = pfq_skbuff_batch_len(skbs);

	/* transmit the skbs */

	sent = pfq_queue_xmit(skbs, dev, to->queue[qindex].hw_queue);

	if (printk_ratelimit())
		printk("PFQ: len:%d sent:%d\n", len, sent);

	/* update stats */

	__sparse_add(&to->stats.sent, sent, cpu);
	__sparse_add(&to->stats.disc, len - sent, cpu);
	__sparse_add(&global_stats.sent, sent, cpu);
	__sparse_add(&global_stats.disc, len - sent, cpu);

	/* free the slots in the socket queue */

	pfq_spsc_read_commit_n(txq, len);

	/* recycle the skbuff now... */

	for_each_skbuff(skbs, skb, i)
		pfq_kfree_skb_recycle(skb, &local->tx_recycle_list);

	pfq_skbuff_batch_flush(skbs);

#ifdef PFQ_TX_PROFILE
	if (printk_ratelimit()) {
		cycles_t stop = get_cycles();
		printk(KERN_INFO "[PFQ] TX avg cpu-cycle: %llu_tsc (batch len = %d).\n", (stop - start)/len, len);
	}
#endif
	return sent;
}


int
__pfq_tx_queue_flush(size_t qindex, struct pfq_tx_opt *to, struct net_device *dev, int cpu, int node)
{
	struct pfq_skbuff_batch skbs;

	struct local_data *local;
        struct pfq_pkt_hdr * h;
        struct sk_buff *skb;
        size_t len;

        int index, avail, n;

	struct pfq_tx_queue_hdr *txq = pfq_get_tx_queue_hdr(to, qindex);

	/* transmit the batch queue... */

	pfq_skbuff_batch_init(&skbs);

        local = __this_cpu_ptr(cpu_data);
        avail = pfq_spsc_read_avail(txq);
	index = pfq_spsc_read_index(txq);

        for(n = 0; n < avail; ++n)
        {
		if (unlikely(index >= to->size)) {
                        if(printk_ratelimit())
                                printk(KERN_WARNING "[PFQ] bogus queue index: size=%zu index=%d\n", to->size, index);
                        break;
                }

                h = (struct pfq_pkt_hdr *) (to->queue[qindex].base_addr + index * txq->slot_size);

                skb = pfq_tx_alloc_skb(to->maxlen, GFP_KERNEL, node);
                if (unlikely(skb == NULL))
		        break;

                skb->dev = dev;

                /* copy packet to this skb: */

                len = min_t(size_t, h->len, txq->max_len);

                /* set the tail */

                skb_reset_tail_pointer(skb);

                skb->len = 0;

                skb_put(skb, len);

                /* copy bytes in the socket buffer */

                skb_copy_to_linear_data(skb, h+1, len < 64 ? 64 : len);

                /* take this skb: skb_get */

		atomic_set(&skb->users, 2);

                /* send the packets... */

		pfq_skbuff_batch_push(&skbs, skb);

		if (pfq_skbuff_batch_len(&skbs) == batch_len)
			__pfq_queue_xmit_batch(qindex, &skbs, dev, to, cpu, local);

		/* get the next index... */

                index = pfq_spsc_next_index(txq, index);
        }

	if (pfq_skbuff_batch_len(&skbs))
		 __pfq_queue_xmit_batch(qindex, &skbs, dev, to, cpu, local);

        return n;
}


/*
 * transmit the logic queue or wakeup the related kernel thread
 */

int pfq_tx_queue_flush_or_wakeup(struct pfq_sock *so, int index)
{
	struct pfq_tx_queue_hdr *txq = pfq_get_tx_queue_hdr(&so->tx_opt, index);
	struct net_device *dev;

        if (!pfq_spsc_read_avail(txq))
        	return 0;

	if (so->tx_opt.queue[index].task) {
		wake_up_process(so->tx_opt.queue[index].task);
		return 0;
	}

	dev = dev_get_by_index(sock_net(&so->sk), so->tx_opt.queue[index].if_index);
	if (!dev)
		return -EPERM;

	pfq_tx_queue_flush(index, &so->tx_opt, dev);

	put_cpu();
	dev_put(dev);
	return 0;
}


static int
__pfq_queue_xmit(struct sk_buff *skb, struct net_device *dev, struct netdev_queue *txq)
{
        int rc = -ENOMEM;

        skb_reset_mac_header(skb);

        /* Disable soft irqs for various locks below. Also
         * stops preemption for RCU.
         */

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
pfq_queue_xmit(struct pfq_skbuff_batch *skbs, struct net_device *dev, int hw_queue)
{
       	struct netdev_queue *txq;
	struct sk_buff *skb;
	int i, n = 0;

        /* get txq and fix the hw_queue for this batch.
         *
         * note: in case the hw_queue is set to any-queue (-1), the driver along the first skb
         * select the queue */

        txq = pfq_pick_tx(dev, skbs->queue[0], &hw_queue);

	if (printk_ratelimit())
	printk(KERN_INFO "PFQ: txq:%p, dev:%p, queue:%d\n", txq, dev, hw_queue);

	__netif_tx_lock_bh(txq);

	for_each_skbuff(skbs, skb, i)
	{
                skb_set_queue_mapping(skb, hw_queue);

		if (__pfq_queue_xmit(skb, dev, txq) == NETDEV_TX_OK)
			++n;
	}

	__netif_tx_unlock_bh(txq);

	return n;
}


int pfq_queue_xmit_by_mask(struct pfq_skbuff_batch *skbs, unsigned long long mask, struct net_device *dev, int hw_queue)
{
       	struct netdev_queue *txq;
	struct sk_buff *skb;
	int i, n = 0;

        /* get txq and fix the hw_queue for this batch.
         *
         * note: in case the hw_queue is set to any-queue (-1), the driver along the first skb
         * select the queue */

        txq = pfq_pick_tx(dev, skbs->queue[0], &hw_queue);

	if(printk_ratelimit())
		printk("PFQ: dev:%p, hw_queue=%d\n", dev, hw_queue);

	__netif_tx_lock_bh(txq);

	for_each_skbuff_bitmask(skbs, mask, skb, i)
	{
                skb_set_queue_mapping(skb, hw_queue);

		if (__pfq_queue_xmit(skb, dev, txq) == NETDEV_TX_OK)
			++n;
	}

	__netif_tx_unlock_bh(txq);

	return n;
}


int pfq_lazy_xmit(struct gc_buff buff, struct net_device *dev, int hw_queue)
{
	struct gc_log *log = PFQ_CB(buff.skb)->log;

       	if (log->num_fwd >= Q_GC_LOG_QUEUE_LEN) {

		if (printk_ratelimit())
        		printk(KERN_INFO "[PFQ] bridge %s: too many annotation!\n", dev->name);

        	return 0;
	}

	skb_set_queue_mapping(buff.skb, hw_queue);
	log->dev[log->num_fwd++] = dev;

	return 1;
}



int pfq_lazy_queue_xmit(struct gc_queue_buff *queue, struct net_device *dev, int hw_queue)
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


int pfq_lazy_queue_xmit_by_mask(struct gc_queue_buff *queue, unsigned long long mask, struct net_device *dev, int hw_queue)
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


int pfq_lazy_xmit_exec(struct gc_buff buff)
{
	struct gc_log *log = PFQ_CB(buff.skb)->log;

	struct sk_buff *skb;

	int ret = 0, num_fwd, i;

	num_fwd = log->num_fwd;

	for(i = 0; i < num_fwd; ++i)
	{
		struct net_device *dev = log->dev[i];

		skb = (i == num_fwd-1) ? skb_get(buff.skb) : skb_clone(buff.skb, GFP_ATOMIC);
		if (skb)
		{
			if (pfq_xmit(skb, dev, skb->queue_mapping) != 1) {
#ifdef PFQ_DEBUG
				if (printk_ratelimit())
					printk(KERN_INFO "[PFQ] forward pfq_xmit: error on device %s!\n", dev->name);
#endif
			}
			else {
				ret++;
			}
		}
	}

	return ret;
}



