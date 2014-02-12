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
#include <linux/skbuff.h>

#include <linux/netdevice.h>

struct netdev_queue *pfq_pick_tx(struct net_device *dev, struct sk_buff *skb, int queue_index)
{
        if (dev->real_num_tx_queues != 1)
        {
                if (skb_get_queue_mapping(skb) == -1)
                {
                        const struct net_device_ops *ops = dev->netdev_ops;

                        if (ops->ndo_select_queue)
                                queue_index = ops->ndo_select_queue(dev, skb);
                        else
                                queue_index = __netdev_pick_tx(dev, skb);
                }
        }

        if (dev->real_num_tx_queues == 1)
                skb_set_queue_mapping(skb, 0);
        else
                skb_set_queue_mapping(skb, queue_index);

        return netdev_get_tx_queue(dev, queue_index);
}



int pfq_queue_xmit(struct sk_buff *skb)
{
        struct net_device *dev = skb->dev;
        const struct net_device_ops * ops = skb->dev->netdev_ops;
        struct netdev_queue *txq;
        int rc = -ENOMEM;

        skb_reset_mac_header(skb);

        /* Disable soft irqs for various locks below. Also
         * stops preemption for RCU.
         */

        rcu_read_lock_bh();

        txq = pfq_pick_tx(dev, skb, skb_get_queue_mapping(skb));

        if (dev->flags & IFF_UP) {
                int cpu = smp_processor_id(); /* ok because BHs are off */

                if (txq->xmit_lock_owner != cpu) {

                        HARD_TX_LOCK(dev, txq, cpu);

                        if (!netif_xmit_stopped(txq)) {

                                rc = ops->ndo_start_xmit(skb, dev);

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

