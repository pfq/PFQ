/*
 * Copyright (c) 2014 Bonelli Nicola <nicola.bonelli@cnit.it>
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
#include <pf_q-transmit.h>
#include <pf_q-common.h>
#include <pf_q-global.h>

static inline u16
__pfq_dev_cap_txqueue(struct net_device *dev, u16 queue_index)
{
        if (unlikely(queue_index >= dev->real_num_tx_queues))
                return 0;

        return queue_index;
}

/* select the right tx queue, and fix queue_index (-1 means any queue) */

static struct netdev_queue *
pfq_pick_tx(struct net_device *dev, struct sk_buff *skb, int *queue_index)
{
        if (dev->real_num_tx_queues != 1 && *queue_index == -1)
        {
                const struct net_device_ops *ops = dev->netdev_ops;
                *queue_index = ops->ndo_select_queue
                                ?
#if (LINUX_VERSION_CODE < KERNEL_VERSION(3,13,0))
                                ops->ndo_select_queue(dev, skb)
#elif (LINUX_VERSION_CODE == KERNEL_VERSION(3,13,0))
                                ops->ndo_select_queue(dev, skb, NULL)
#else
                                ops->ndo_select_queue(dev, skb, NULL, NULL)
#endif
                                : 0;
        }

        *queue_index = __pfq_dev_cap_txqueue(dev, *queue_index);

        return netdev_get_tx_queue(dev, *queue_index);
}


int pfq_tx_queue_flush(struct pfq_tx_opt *to, struct net_device *dev, int cpu, int node)
{
	struct pfq_batch_queue_skb skbs;

	struct local_data *local;
        struct pfq_pkt_hdr * h;
        struct sk_buff *skb;
        size_t len;
        int n, index, avail;

#ifdef PFQ_TX_PROFILE
	static int pkt_counter;
#endif

	/* transmit the batch queue... */

	void pfq_tx_batch_queue(void)
	{
		struct sk_buff *skb;
        	int sent, i;

		sent = pfq_queue_xmit(PFQ_BOUNDED_QUEUE(&skbs), dev, to->hw_queue);

                /* update stats */

                __sparse_add(&to->stat.sent, sent, cpu);
                __sparse_add(&to->stat.disc, pfq_bounded_queue_len(PFQ_BOUNDED_QUEUE(&skbs)) - sent, cpu);

		/* free/recycle the packets now... */

		pfq_bounded_queue_for_each(skb, i, PFQ_BOUNDED_QUEUE(&skbs))
		{
			pfq_kfree_skb_recycle(skb, &local->tx_recycle_list);
		}

		pfq_spsc_read_commit_n(to->queue_ptr, pfq_bounded_queue_len(PFQ_BOUNDED_QUEUE(&skbs)));

		pfq_bounded_queue_flush(PFQ_BOUNDED_QUEUE(&skbs));
	}


	pfq_bounded_queue_init(PFQ_BOUNDED_QUEUE(&skbs));

        local = __this_cpu_ptr(cpu_data);

        avail = pfq_spsc_read_avail(to->queue_ptr);
	index = pfq_spsc_read_index(to->queue_ptr);

        for(n = 0; n < avail; ++n)
        {

#ifdef PFQ_TX_PROFILE
		cycles_t start = get_cycles();
#endif
		if (unlikely(index >= to->size))
                {
                        if(printk_ratelimit())
                                printk(KERN_WARNING "[PFQ] bogus spsc index! q->size=%zu index=%d\n", to->size, index);
                        break;
                }

                h = (struct pfq_pkt_hdr *) (to->base_addr + index * to->queue_ptr->slot_size);

                skb = pfq_tx_alloc_skb(to->maxlen, GFP_KERNEL, node);
                if (unlikely(skb == NULL))
		        break;

                skb->dev = dev;

                /* copy packet to this skb: */

                len = min_t(size_t, h->len, to->queue_ptr->max_len);

                /* set the tail */

                skb_reset_tail_pointer(skb);

                skb->len = 0;

                skb_put(skb, len);

                /* copy bytes in the socket buffer */

#if 1
                skb_copy_to_linear_data_offset(skb, 0, h+1, len);
#else
                if (skb_store_bits(skb, 0, h+1, len) < 0)
                {
                        pfq_kfree_skb_recycle(skb, &local->tx_recycle_list);
                        break;
                }
#endif

                /* take this skb: skb_get */

		atomic_set(&skb->users, 2);

                /* send the packets... */

		pfq_bounded_queue_push(PFQ_BOUNDED_QUEUE(&skbs), Q_BATCH_QUEUE_LEN, skb);

		if (pfq_bounded_queue_len(PFQ_BOUNDED_QUEUE(&skbs)) == batch_len)
		{
			pfq_tx_batch_queue();
		}

		/* get the next index... */

                index = pfq_spsc_next_index(to->queue_ptr, index);

#ifdef PFQ_TX_PROFILE
		if ((pkt_counter++ % 1048576) == 0)
		{
			cycles_t stop = get_cycles();
			printk(KERN_INFO "[PFQ] TX cpu-cycle: %llu\n", (stop - start));
		}
#endif
        }

	if (pfq_bounded_queue_len(PFQ_BOUNDED_QUEUE(&skbs))) {

		pfq_tx_batch_queue();
	}

        return n;
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
			{
				goto out;
			}
		}
	}

	kfree_skb(skb);
	return -ENETDOWN;

out:
	return rc;
}


int pfq_queue_xmit(struct pfq_bounded_queue_skb *skbs, struct net_device *dev, int queue_index)
{
       	struct netdev_queue *txq;
	struct sk_buff *skb;
	int i, n = 0;

        /* get txq and fix the queue_index for this batch.
         *
         * note: in case the queue_index is set to any-queue (-1), the driver along the first skb
         * select the queue */

        txq = pfq_pick_tx(dev, skbs->queue[0], &queue_index);

	__netif_tx_lock_bh(txq);

	pfq_bounded_queue_for_each(skb, i, skbs)
	{
                skb_set_queue_mapping(skb, queue_index);

		if (__pfq_queue_xmit(skb, dev, txq) == NETDEV_TX_OK)
			++n;
	}

	__netif_tx_unlock_bh(txq);

	return n;
}


int pfq_queue_xmit_by_mask(struct pfq_bounded_queue_skb *skbs, unsigned long long skbs_mask, struct net_device *dev, int queue_index)
{
       	struct netdev_queue *txq;
	struct sk_buff *skb;
	int i, n = 0;

        /* get txq and fix the queue_index for this batch.
         *
         * note: in case the queue_index is set to any-queue (-1), the driver along the first skb
         * select the queue */

        txq = pfq_pick_tx(dev, skbs->queue[0], &queue_index);

	__netif_tx_lock_bh(txq);

	pfq_bounded_queue_for_each_bitmask(skb, skbs_mask, i, skbs)
	{
                skb_set_queue_mapping(skb, queue_index);

		if (__pfq_queue_xmit(skb, dev, txq) == NETDEV_TX_OK)
			++n;
	}

	__netif_tx_unlock_bh(txq);

	return n;
}


int pfq_lazy_xmit(struct sk_annot *ska, struct sk_buff *skb, struct net_device *dev, int queue_index)
{
       	if (ska->num_fwd >= Q_MAX_SKB_DEV_ANNOT) {

		if (printk_ratelimit())
        		printk(KERN_INFO "[PFQ] bridge %s: too many annotation!\n", dev->name);

        	return 0;
	}

	skb_set_queue_mapping(skb, queue_index);
	ska->dev[ska->num_fwd++] = dev;

	return 1;
}



int pfq_lazy_queue_xmit(struct sk_annot *skas, struct pfq_bounded_queue_skb *skbs, struct net_device *dev, int queue_index)
{
	struct sk_buff *skb;
	int i, n = 0;

	pfq_bounded_queue_for_each(skb, i, skbs)
	{
		if (pfq_lazy_xmit(&skas[i], skb, dev, queue_index))
			++n;
	}

	return n;
}


int pfq_lazy_queue_xmit_by_mask(struct sk_annot *skas, struct pfq_bounded_queue_skb *skbs, unsigned long long skbs_mask, struct net_device *dev, int queue_index)
{
	struct sk_buff *skb;
	int i, n = 0;

	pfq_bounded_queue_for_each_bitmask(skb, skbs_mask, i, skbs)
	{
		if (pfq_lazy_xmit(&skas[i], skb, dev, queue_index))
			++n;
	}

	return n;
}


int pfq_lazy_exec(struct sk_annot *ska, struct sk_buff *skb)
{
	struct sk_buff *nskb;
	int ret = 0, num_fwd, i;

	num_fwd = ska->num_fwd;

	for(i = 0; i < num_fwd; ++i)
	{
		struct net_device *dev = ska->dev[i];

		nskb = (i == num_fwd-1) ? skb : skb_clone(skb, GFP_ATOMIC);
		if (nskb)
		{
			if (pfq_xmit(nskb, dev, nskb->queue_mapping) != 1) {

				if (printk_ratelimit())
					printk(KERN_INFO "[PFQ] forward pfq_xmit: error on device %s!\n", dev->name);
			}
			else {
				ret++;
			}
		}
	}

	return ret;
}


