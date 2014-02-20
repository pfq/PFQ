/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
 * 	           Loris Gazzarrini <loris.gazzarrini@iet.unipi.it>
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

#ifndef _PF_Q_MEMORY_H_
#define _PF_Q_MEMORY_H_

#include <linux/version.h>
#include <linux/skbuff.h>

#include <pf_q-common.h>
#include <pf_q-prefetch-queue.h>

/* per-cpu data... */

struct local_data
{
        unsigned long           eligible_mask;
        unsigned long           sock_mask [Q_MAX_ID];
        int                     sock_cnt;
        int 			        flowctrl;
        struct pfq_prefetch_skb    prefetch_queue;
        struct sk_buff_head     recycle_list;
};


extern int recycle_len;

extern struct local_data __percpu    * cpu_data;

struct sk_buff * __pfq_alloc_skb(unsigned int size, gfp_t priority, int fclone, int node);
struct sk_buff * pfq_dev_alloc_skb(unsigned int length);
struct sk_buff * __pfq_netdev_alloc_skb(struct net_device *dev, unsigned int length, gfp_t gfp);


#ifdef PFQ_USE_SKB_RECYCLE
#pragma message "[PFQ] *** using skb recycle ***"
#endif

#ifdef NET_SKBUFF_DATA_USES_OFFSET
static inline
unsigned int pfq_skb_end_offset(const struct sk_buff *skb)
{
    return skb->end;
}
#else
static inline
unsigned int pfq_skb_end_offset(const struct sk_buff *skb)
{
    return skb->end - skb->head;
}
#endif


static inline
bool pfq_skb_is_recycleable(const struct sk_buff *skb)
{
    // if (irqs_disabled())
    //    return false;

#if (LINUX_VERSION_CODE >= KERNEL_VERSION(3,1,0))
    if (skb_shinfo(skb)->tx_flags & SKBTX_DEV_ZEROCOPY)
        return false;
#endif

    if (skb_is_nonlinear(skb) || skb->fclone != SKB_FCLONE_UNAVAILABLE)
        return false;

    // skb_shared() and skb_cloned() are tested later...

    return true;
}


static inline
void pfq_kfree_skb_recycle(struct sk_buff *skb, struct sk_buff_head *list)
{
#ifdef PFQ_USE_SKB_RECYCLE
    if (pfq_skb_is_recycleable(skb)
            && skb_queue_len(list) <= recycle_len)
    {
            __skb_queue_head(list, skb);
            return;
    }
#endif
	kfree_skb(skb);
}


static inline
void pfq_kfree_skb(struct sk_buff *skb)
{
#ifdef PFQ_USE_SKB_RECYCLE
        struct local_data * local = __this_cpu_ptr(cpu_data);
        struct sk_buff_head * list = &local->recycle_list;
	    return pfq_kfree_skb_recycle(skb, list);
#else
        return pfq_kfree_skb_recycle(skb, NULL);
#endif
}


static inline
struct sk_buff * pfq_skb_recycle(struct sk_buff *skb)
{
        //  skb_recycle(skb); removed from kernel 3.7!!!

        struct skb_shared_info *shinfo;

        if (skb->destructor) {
            skb->destructor(skb);
            skb->destructor = NULL;
        }

        shinfo = skb_shinfo(skb);
        // memset(shinfo, 0, offsetof(struct skb_shared_info, dataref));
        atomic_set(&shinfo->dataref,1);

        memset(skb, 0, offsetof(struct sk_buff, tail));
        skb->data = skb->head + NET_SKB_PAD;

        skb_reset_tail_pointer(skb);
        return skb;
}


static inline
struct sk_buff * pfq_netdev_alloc_skb(struct net_device *dev, unsigned int length)
{
        return __pfq_netdev_alloc_skb(dev, length, GFP_ATOMIC);
}


static inline
struct sk_buff * __pfq_netdev_alloc_skb_ip_align(struct net_device *dev, unsigned int length, gfp_t gfp)
{
        struct sk_buff *skb = __pfq_netdev_alloc_skb(dev, length + NET_IP_ALIGN, gfp);
        if (NET_IP_ALIGN && likely(skb))
                skb_reserve(skb, NET_IP_ALIGN);
        return skb;
}


static inline
struct sk_buff * pfq_netdev_alloc_skb_ip_align(struct net_device *dev, unsigned int length)
{
        return __pfq_netdev_alloc_skb_ip_align(dev, length, GFP_ATOMIC);
}


static inline
struct sk_buff * ____pfq_alloc_skb(unsigned int size, gfp_t priority, int fclone, int node)
{

#ifdef PFQ_USE_SKB_RECYCLE
        struct local_data * local = __this_cpu_ptr(cpu_data);
        struct sk_buff *skb;

        if (!fclone)
        {
            skb = skb_peek_tail(&local->recycle_list);

            if (skb != NULL)
            {
                if (!skb_shared(skb) && !skb_cloned(skb))
                {
                    skb = __skb_dequeue_tail(&local->recycle_list);

                    if (pfq_skb_end_offset(skb) >= SKB_DATA_ALIGN(size + NET_SKB_PAD)) {
                        return pfq_skb_recycle(skb);
                    }
                    else {
                        kfree_skb(skb);
                    }
                }
            }
        }
#endif

        return __alloc_skb(size, priority, fclone, node);
}


static inline
struct sk_buff * pfq_alloc_skb(unsigned int size, gfp_t priority)
{
        return ____pfq_alloc_skb(size, priority, 0, NUMA_NO_NODE);
}


static inline
struct sk_buff * pfq_alloc_skb_fclone(unsigned int size, gfp_t priority)
{
        return __pfq_alloc_skb(size, priority, 1, NUMA_NO_NODE);
}


#endif /* _PF_Q_MEMORY_H_ */
