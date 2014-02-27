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

#include <pf_q-memory.h>
#include <pf_q-sock.h>
#include <pf_q-transmit.h>
#include <pf_q-common.h>
#include <pf_q-global.h>

static inline u16 pfq_dev_cap_txqueue(struct net_device *dev, u16 queue_index)
{
        if (unlikely(queue_index >= dev->real_num_tx_queues))
                return 0;

        return queue_index;
}


struct netdev_queue *pfq_pick_tx(struct net_device *dev, struct sk_buff *skb, int queue_index)
{
        if (dev->real_num_tx_queues != 1 && queue_index == -1)
        {
                const struct net_device_ops *ops = dev->netdev_ops;
                queue_index = ops->ndo_select_queue
                                ?
#if (LINUX_VERSION_CODE < KERNEL_VERSION(3,13,0))
                                ops->ndo_select_queue(dev, skb)
#else
                                ops->ndo_select_queue(dev, skb, NULL)
#endif
                                : 0;
        }

        queue_index = pfq_dev_cap_txqueue(dev, queue_index);

        skb_set_queue_mapping(skb, queue_index);
        return netdev_get_tx_queue(dev, queue_index);
}


int pfq_tx_queue_flush(struct pfq_tx_opt *to, struct net_device *dev, int cpu, int node)
{
	struct pfq_non_intrusive_skb skbs;

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

		sent = pfq_queue_xmit(&skbs, dev, to->hw_queue);

                /* update stats */

                __sparse_add(&to->stat.sent, sent, cpu);
                __sparse_add(&to->stat.disc, pfq_non_intrusive_len(&skbs) - sent, cpu);

		/* free/recycle the packets now... */

		pfq_non_intrusive_for_each(skb, i, &skbs)
		{
			pfq_kfree_skb_recycle(skb, &local->tx_recycle_list);
		}

		pfq_spsc_read_commit_n(to->queue_info, pfq_non_intrusive_len(&skbs));

		pfq_non_intrusive_flush(&skbs);
	}


	pfq_non_intrusive_init(&skbs);

        local = __this_cpu_ptr(cpu_data);

        avail = pfq_spsc_read_avail(to->queue_info);
	index = pfq_spsc_read_index(to->queue_info);

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

                h = (struct pfq_pkt_hdr *) (to->base_addr + index * to->queue_info->slot_size);

                skb = pfq_tx_alloc_skb(to->maxlen, GFP_KERNEL, node);
                if (unlikely(skb == NULL))
		        break;

                skb->dev = dev;

                /* copy packet to this skb: */

                len = min_t(size_t, h->len, to->queue_info->max_len);

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

		pfq_non_intrusive_push(&skbs, skb);

		if (pfq_non_intrusive_len(&skbs) == batch_len)
		{
			pfq_tx_batch_queue();
		}

		/* get the next index... */

                index = pfq_spsc_next_index(to->queue_info, index);

#ifdef PFQ_TX_PROFILE
		if ((pkt_counter++ % 1048576) == 0)
		{
			cycles_t stop = get_cycles();
			printk(KERN_INFO "[PFQ] TX cpu-cycle: %llu\n", (stop - start));
		}
#endif
        }

	if (pfq_non_intrusive_len(&skbs)) {

		pfq_tx_batch_queue();
	}

        return n;
}



int __pfq_queue_xmit(struct sk_buff *skb, struct net_device *dev, struct netdev_queue *txq, int queue_index)
{
        int rc = -ENOMEM;

        skb_reset_mac_header(skb);

        skb_set_queue_mapping(skb, queue_index);

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


int pfq_queue_xmit(struct pfq_non_intrusive_skb *skbs, struct net_device *dev, int queue_index)
{
       	struct netdev_queue *txq;
	struct sk_buff *skb;
	int i, n = 0;

        queue_index = pfq_dev_cap_txqueue(dev, queue_index);

        txq = netdev_get_tx_queue(dev, queue_index);

	__netif_tx_lock_bh(txq);

	pfq_non_intrusive_for_each(skb, i, skbs)
	{
		if (__pfq_queue_xmit(skb, dev, txq, queue_index) == NETDEV_TX_OK)
			++n;
	}

	__netif_tx_unlock_bh(txq);

	return n;
}


