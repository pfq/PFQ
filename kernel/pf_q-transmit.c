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



int pfq_queue_xmit(struct sk_buff *skb, int queue_index)
{
        struct net_device *dev = skb->dev;
        struct netdev_queue *txq;
        int rc = -ENOMEM;

        skb_reset_mac_header(skb);

        /* Disable soft irqs for various locks below. Also
         * stops preemption for RCU.
         */

        rcu_read_lock_bh();

        txq = pfq_pick_tx(dev, skb, queue_index);

        if (dev->flags & IFF_UP) {
                int cpu = smp_processor_id(); /* ok because BHs are off */

                if (txq->xmit_lock_owner != cpu) {

                        HARD_TX_LOCK(dev, txq, cpu);

#if (LINUX_VERSION_CODE <= KERNEL_VERSION(3,2,0))
                        if (!netif_tx_queue_stopped(txq)) {
#else
                        if (!netif_xmit_stopped(txq)) {
#endif

                                // rc = hard_start_xmit(skb, dev, txq);

                                rc = dev->netdev_ops->ndo_start_xmit(skb, dev);

                                if (dev_xmit_complete(rc)) {
                                         HARD_TX_UNLOCK(dev, txq);
                                         goto out;
                                }
                        }

                        HARD_TX_UNLOCK(dev, txq);
                }
        }

        rc = -ENETDOWN;
        rcu_read_unlock_bh();

        kfree_skb(skb);
        return rc;
out:
        rcu_read_unlock_bh();
        return rc;
}

