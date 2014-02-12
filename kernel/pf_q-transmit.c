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

#include <pf_q-memory.h>

struct netdev_queue *pfq_pick_tx(struct net_device *dev, struct sk_buff *skb, int queue_index)
{
        if (dev->real_num_tx_queues != 1)
        {
                if (queue_index == -1)
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


int pfq_queue_xmit(struct sk_buff *skb, int ifindex, int queue_index)
{
         struct net_device *dev = skb->dev;
         struct netdev_queue *txq;
         struct Qdisc *q;
         int rc = -ENOMEM;

         skb_reset_mac_header(skb);

         /* Disable soft irqs for various locks below. Also
          * stops preemption for RCU.
          */
         rcu_read_lock_bh();

         // skb_update_prio(skb);

         // txq = netdev_pick_tx(dev, skb);
         //

         // txq = pfq_pick_tx(dev, skb, queue_index);
         txq = __netdev_pick_tx(dev, skb);

         if (txq == NULL)
         {
                printk(KERN_WARNING "MERDA!\n");
                goto out;
         }

         // q = rcu_dereference_bh(txq->qdisc);

         // trace_net_dev_queue(skb);
         //
         // if (q->enqueue) {
         //         rc = __dev_xmit_skb(skb, q, dev, txq);
         //         goto out;
         // }

         /* The device has no queue. Common case for software devices:
            loopback, all the sorts of tunnels...

            Really, it is unlikely that netif_tx_lock protection is necessary
            here.  (f.e. loopback and IP tunnels are clean ignoring statistics
            counters.)
            However, it is possible, that they rely on protection
            made by us here.

            Check this and shot the lock. It is not prone from deadlocks.
            Either shot noqueue qdisc, it is even simpler 8)
          */
         if (dev->flags & IFF_UP) {
                 int cpu = smp_processor_id(); /* ok because BHs are off */

                 if (txq->xmit_lock_owner != cpu) {

                         // if (__this_cpu_read(xmit_recursion) > RECURSION_LIMIT)
                         //         goto recursion_alert;

                         HARD_TX_LOCK(dev, txq, cpu);

                         if (!netif_xmit_stopped(txq)) {

                                 // __this_cpu_inc(xmit_recursion);

                                 // rc = dev_hard_start_xmit(skb, dev, txq);

                                 rc = 0;
                                 pfq_kfree_skb(skb);

                                 // __this_cpu_dec(xmit_recursion);


                                 if (dev_xmit_complete(rc)) {
                                         HARD_TX_UNLOCK(dev, txq);
                                         goto out;
                                 }
                         }
                         HARD_TX_UNLOCK(dev, txq);
                         // net_crit_ratelimited("Virtual device %s asks to queue packet!\n",
                         //                      dev->name);
                 } else {
                         /* Recursion is detected! It is possible,
                          * unfortunately
                          */
//  recursion_alert:
                         // net_crit_ratelimited("Dead loop on virtual device %s, fix it urgently!\n",
                         //                      dev->name);
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

