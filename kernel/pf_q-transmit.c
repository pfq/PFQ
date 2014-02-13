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

#if 0
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

        printk(KERN_WARNING "real_num_tx = %d queue_index=%d\n", dev->real_num_tx_queues, queue_index);

        if (dev->real_num_tx_queues == 1)
                skb_set_queue_mapping(skb, 0);
        else
                skb_set_queue_mapping(skb, queue_index);

        return netdev_get_tx_queue(dev, queue_index);
}
#endif


static inline u16 pfq_dev_cap_txqueue(struct net_device *dev, u16 queue_index)
{
        if (unlikely(queue_index >= dev->real_num_tx_queues)) {
                net_warn_ratelimited("%s selects TX queue %d, but real number of TX queues is %d\n",
                                     dev->name, queue_index,
                                     dev->real_num_tx_queues);
                return 0;
        }
        return queue_index;
}


struct netdev_queue *pfq_netdev_pick_tx(struct net_device *dev,
                                    struct sk_buff *skb)
{
        int queue_index = 0;

        if (dev->real_num_tx_queues != 1) {
                const struct net_device_ops *ops = dev->netdev_ops;
                if (ops->ndo_select_queue)
                        queue_index = ops->ndo_select_queue(dev, skb);
                else
                        queue_index = __netdev_pick_tx(dev, skb);
                queue_index = pfq_dev_cap_txqueue(dev, queue_index);
        }

        skb_set_queue_mapping(skb, queue_index);
        return netdev_get_tx_queue(dev, queue_index);
}


int pfq_hard_start_xmit(struct sk_buff *skb, struct net_device *dev,
                        struct netdev_queue *txq)
{
        const struct net_device_ops *ops = dev->netdev_ops;
        int rc = NETDEV_TX_OK;
        unsigned int skb_len;

        // if (likely(!skb->next)) {
        //         netdev_features_t features;

        //         /*
        //          * If device doesn't need skb->dst, release it right now while
        //          * its hot in this cpu cache
        //          */
        //         if (dev->priv_flags & IFF_XMIT_DST_RELEASE)
        //                 skb_dst_drop(skb);

        //         features = netif_skb_features(skb);

        //         if (vlan_tx_tag_present(skb) &&
        //             !vlan_hw_offload_capable(features, skb->vlan_proto)) {
        //                 skb = __vlan_put_tag(skb, skb->vlan_proto,
        //                                      vlan_tx_tag_get(skb));
        //                 if (unlikely(!skb))
        //                         goto out;

        //                 skb->vlan_tci = 0;
        //         }

        //         /* If encapsulation offload request, verify we are testing
        //          * hardware encapsulation features instead of standard
        //          * features for the netdev
        //          */
        //         if (skb->encapsulation)
        //                 features &= dev->hw_enc_features;

        //         if (netif_needs_gso(skb, features)) {
        //                 if (unlikely(dev_gso_segment(skb, features)))
        //                         goto out_kfree_skb;
        //                 if (skb->next)
        //                         goto gso;
        //         } else {
        //                 if (skb_needs_linearize(skb, features) &&
        //                     __skb_linearize(skb))
        //                         goto out_kfree_skb;

        //                 /* If packet is not checksummed and device does not
        //                  * support checksumming for this protocol, complete
        //                  * checksumming here.
        //                  */
        //                 if (skb->ip_summed == CHECKSUM_PARTIAL) {
        //                         if (skb->encapsulation)
        //                                 skb_set_inner_transport_header(skb,
        //                                         skb_checksum_start_offset(skb));
        //                         else
        //                                 skb_set_transport_header(skb,
        //                                         skb_checksum_start_offset(skb));
        //                         if (!(features & NETIF_F_ALL_CSUM) &&
        //                              skb_checksum_help(skb))
        //                                 goto out_kfree_skb;
        //                 }
        //         }

        //         if (!list_empty(&ptype_all))
        //                 dev_queue_xmit_nit(skb, dev);

        //         skb_len = skb->len;
        //         rc = ops->ndo_start_xmit(skb, dev);
        //         trace_net_dev_xmit(skb, rc, dev, skb_len);
        //         if (rc == NETDEV_TX_OK)
        //                 txq_trans_update(txq);
        //         return rc;
        // }

gso:
        do {
                struct sk_buff *nskb = skb->next;

                skb->next = nskb->next;
                nskb->next = NULL;

                // if (!list_empty(&ptype_all))
                //        dev_queue_xmit_nit(nskb, dev);

                skb_len = nskb->len;
                rc = ops->ndo_start_xmit(nskb, dev);

                // trace_net_dev_xmit(nskb, rc, dev, skb_len);

                if (unlikely(rc != NETDEV_TX_OK)) {

                        if (rc & ~NETDEV_TX_MASK)
                                // goto out_kfree_gso_skb;
                                goto out_kfree_skb;

                        nskb->next = skb->next;
                        skb->next = nskb;
                        return rc;
                }

                txq_trans_update(txq);

                if (unlikely(netif_xmit_stopped(txq) && skb->next))
                        return NETDEV_TX_BUSY;

        } while (skb->next);


        return rc;

/* out_kfree_gso_skb: */
/*         if (likely(skb->next == NULL)) { */
/*                 skb->destructor = DEV_GSO_CB(skb)->destructor; */
/*                 consume_skb(skb); */
/*                 return rc; */
/*         } */

out_kfree_skb:
        kfree_skb(skb);
out:
        return rc;
}


int pfq_queue_xmit(struct sk_buff *skb, int ifindex, int queue_index)
{
        struct net_device *dev = skb->dev;
        struct netdev_queue *txq;
        int rc = -ENOMEM;

        skb_reset_mac_header(skb);

        /* Disable soft irqs for various locks below. Also
         * stops preemption for RCU.
         */
        rcu_read_lock_bh();

        // skb_update_prio(skb);

        txq = pfq_netdev_pick_tx(dev, skb);

        printk(KERN_WARNING "--> txq @ %p\n", txq);

        // trace_net_dev_queue(skb);

        if (dev->flags & IFF_UP) {
                int cpu = smp_processor_id(); /* ok because BHs are off */

                if (txq->xmit_lock_owner != cpu) {

                        // if (__this_cpu_read(xmit_recursion) > RECURSION_LIMIT)
                        //         goto recursion_alert;

                        // HARD_TX_LOCK(dev, txq, cpu);

                        if (!netif_xmit_stopped(txq)) {

                                // __this_cpu_inc(xmit_recursion);

                                // rc = pfq_hard_start_xmit(skb, dev, txq);

                                // __this_cpu_dec(xmit_recursion);
                                // if (dev_xmit_complete(rc)) {
                                //         HARD_TX_UNLOCK(dev, txq);
                                //         goto out;
                                // }
                                //

                                kfree_skb(skb);
                                goto out;
                        }

                        // HARD_TX_UNLOCK(dev, txq);

                        // net_crit_ratelimited("Virtual device %s asks to queue packet!\n",
                        //                      dev->name);
                } else {
                        /* Recursion is detected! It is possible,
                         * unfortunately
                         */
// recursion_alert:
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

