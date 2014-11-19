/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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

#ifndef _PF_Q_MEMORY_H_
#define _PF_Q_MEMORY_H_

#include <linux/version.h>
#include <linux/skbuff.h>
#include <linux/hardirq.h>
#include <net/dst.h>

#include <pf_q-skbuff-list.h>
#include <pf_q-macro.h>
#include <pf_q-percpu.h>
#include <pf_q-sparse.h>
#include <pf_q-global.h>

extern int recycle_len;
extern struct local_data __percpu * cpu_data;


struct pfq_recycle_stat
{
        uint64_t        os_alloc;
        uint64_t        os_free;
        uint64_t        rc_alloc;
        uint64_t        rc_free;
        uint64_t        rc_miss;

        uint64_t        err_intdis;
        uint64_t        err_shared;
        uint64_t        err_cloned;
        uint64_t        err_memory;
};


extern struct pfq_recycle_stat pfq_get_recycle_stats(void);

extern struct sk_buff * __pfq_alloc_skb(unsigned int size, gfp_t priority, int fclone, int node);
extern struct sk_buff * pfq_dev_alloc_skb(unsigned int length);
extern struct sk_buff * __pfq_netdev_alloc_skb(struct net_device *dev, unsigned int length, gfp_t gfp);


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


static inline bool pfq_skb_is_parkable(const struct sk_buff *skb)
{

#if LINUX_VERSION_CODE >= KERNEL_VERSION(3,1,0)
    	if (skb_shinfo(skb)->tx_flags & SKBTX_DEV_ZEROCOPY)
    	   	return false;
#endif

    	if (skb_is_nonlinear(skb) || skb->fclone != SKB_FCLONE_UNAVAILABLE)
    		return false;

    	return true;
}


static inline bool pfq_skb_is_recycleable(const struct sk_buff *skb, int skb_size)
{
    	if (irqs_disabled()) {
#ifdef PFQ_USE_EXTENDED_PROC
                sparse_inc(&memory_stats.err_intdis);
#endif
    		return false;
	}

    	if (skb_shared(skb)) {
#ifdef PFQ_USE_EXTENDED_PROC
                sparse_inc(&memory_stats.err_shared);
#endif
    	    	return false;
	}

	if(skb_cloned(skb)) {
#ifdef PFQ_USE_EXTENDED_PROC
                sparse_inc(&memory_stats.err_cloned);
#endif
    	    	return false;
	}

    	skb_size = SKB_DATA_ALIGN(skb_size + NET_SKB_PAD);

    	if (pfq_skb_end_offset(skb) < skb_size) {
#ifdef PFQ_USE_EXTENDED_PROC
                sparse_inc(&memory_stats.err_memory);
#endif
    	    	return false;
	}

    	return true;
}


static inline
void pfq_kfree_skb_recycle(struct sk_buff *skb, struct pfq_sk_buff_list *list)
{
#ifdef PFQ_USE_SKB_RECYCLE

        if (pfq_skb_is_parkable(skb))
        {
                if (pfq_sk_buff_list_size(list) <= recycle_len)
                {
                        if (likely(pfq_sk_buff_queue_head(list, skb)))
			{
#ifdef PFQ_USE_EXTENDED_PROC
                        	sparse_inc(&memory_stats.rc_free);
#endif
                        	return;
			}
                }
        }
#endif

#ifdef PFQ_USE_EXTENDED_PROC
        sparse_inc(&memory_stats.os_free);
#endif
        consume_skb(skb);
}


static void skb_release_head_state(struct sk_buff *skb)
{
    	skb_dst_drop(skb);

	if (skb->destructor) {
    		WARN_ON(in_irq());
    	    	skb->destructor(skb);
    	}

#if IS_ENABLED(CONFIG_NF_CONNTRACK)
	nf_conntrack_put(skb->nfct);
#endif
#ifdef NET_SKBUFF_NF_DEFRAG_NEEDED
       	nf_conntrack_put_reasm(skb->nfct_reasm);
#endif
#ifdef CONFIG_BRIDGE_NETFILTER
	nf_bridge_put(skb->nf_bridge);
#endif
#ifdef CONFIG_NET_SCHED
	skb->tc_index = 0;
#ifdef CONFIG_NET_CLS_ACT
	skb->tc_verd = 0;
#endif
#endif

}

static inline
struct sk_buff * pfq_skb_recycle(struct sk_buff *skb)
{
        //  skb_recycle(skb); removed from kernel 3.7!!!

        struct skb_shared_info *shinfo;

	skb_release_head_state(skb);

        shinfo = skb_shinfo(skb);
        memset(shinfo, 0, offsetof(struct skb_shared_info, dataref));
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
struct sk_buff * ____pfq_alloc_skb_recycle(unsigned int size, gfp_t priority, int fclone, int node, struct pfq_sk_buff_list *recycle_list)
{

#ifdef PFQ_USE_SKB_RECYCLE
        struct sk_buff *skb;

        if (!fclone)
        {
                skb = pfq_sk_buff_peek_tail(recycle_list);

                if (likely(skb != NULL))
                {
                        if (pfq_skb_is_recycleable(skb, size))
                        {

#ifdef PFQ_USE_EXTENDED_PROC
                                        sparse_inc(&memory_stats.rc_alloc);
#endif
					pfq_sk_buff_dequeue_tail(recycle_list);
                                        return pfq_skb_recycle(skb);
                        }
                }
#ifdef PFQ_USE_EXTENDED_PROC
		else {
                	sparse_inc(&memory_stats.rc_miss);
		}
#endif
        }
#endif

#ifdef PFQ_USE_EXTENDED_PROC
        sparse_inc(&memory_stats.os_alloc);
#endif
        return __alloc_skb(size, priority, fclone, node);
}


static inline
int pfq_skb_recycle_init(void)
{
        int cpu;
        for_each_possible_cpu(cpu)
        {
                struct local_data *this_cpu = per_cpu_ptr(cpu_data, cpu);

                if (pfq_sk_buff_list_init(&this_cpu->tx_recycle_list) != 0)
                	return -ENOMEM;

                if (pfq_sk_buff_list_init(&this_cpu->rx_recycle_list) != 0)
                	return -ENOMEM;
        }

        return 0;
}


static inline
void pfq_skb_recycle_enable(bool value)
{
        int cpu;

        smp_wmb();
        for_each_possible_cpu(cpu)
        {
                struct local_data *this_cpu = per_cpu_ptr(cpu_data, cpu);
                atomic_set(&this_cpu->enable_recycle, value);
        }
        smp_wmb();
}


static inline
int pfq_skb_recycle_purge(void)
{
        int cpu, total = 0;
        for_each_possible_cpu(cpu)
        {
                struct local_data *local = per_cpu_ptr(cpu_data, cpu);

                total += pfq_sk_buff_list_size(&local->rx_recycle_list);
                total += pfq_sk_buff_list_size(&local->tx_recycle_list);

                pfq_sk_buff_list_free(&local->rx_recycle_list);
                pfq_sk_buff_list_free(&local->tx_recycle_list);
        }

#ifdef PFQ_USE_EXTENDED_PROC
        sparse_add(&memory_stats.os_free, total);
#endif
        return total;
}


static inline
struct sk_buff * pfq_alloc_skb(unsigned int size, gfp_t priority)
{
#ifdef PFQ_USE_SKB_RECYCLE
        struct local_data *this_cpu = __this_cpu_ptr(cpu_data);

        if (atomic_read(&this_cpu->enable_recycle))
        {
                return ____pfq_alloc_skb_recycle(size, priority, 0, NUMA_NO_NODE, &this_cpu->rx_recycle_list);
        }

#ifdef PFQ_USE_EXTENDED_PROC
        sparse_inc(&memory_stats.os_alloc);
#endif
#endif
        return __alloc_skb(size, priority, 0, NUMA_NO_NODE);
}


static inline
struct sk_buff * pfq_tx_alloc_skb(unsigned int size, gfp_t priority, int node)
{
#ifdef PFQ_USE_SKB_RECYCLE
        struct local_data *this_cpu = __this_cpu_ptr(cpu_data);

        if (atomic_read(&this_cpu->enable_recycle))
        {
                return ____pfq_alloc_skb_recycle(size, priority, 0, node, &this_cpu->tx_recycle_list);
        }

#ifdef PFQ_USE_EXTENDED_PROC
        sparse_inc(&memory_stats.os_alloc);
#endif
#endif
        return __alloc_skb(size, priority, 0, NUMA_NO_NODE);
}


static inline
struct sk_buff * pfq_alloc_skb_fclone(unsigned int size, gfp_t priority)
{
        return __pfq_alloc_skb(size, priority, 1, NUMA_NO_NODE);
}


#endif /* _PF_Q_MEMORY_H_ */
