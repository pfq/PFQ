/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PFQ_MEMORY_H
#define PFQ_MEMORY_H

#include <pragma/diagnostic_push>
#include <linux/version.h>
#include <linux/skbuff.h>
#include <linux/hardirq.h>
#include <net/dst.h>
#include <net/xfrm.h>
#include <pragma/diagnostic_pop>

#include <core/global.h>
#include <core/percpu.h>
#include <core/define.h>
#include <core/stats.h>

#include <pfq/pool.h>
#include <pfq/global.h>
#include <pfq/sparse.h>
#include <pfq/percpu.h>
#include <pfq/skbuff.h>



extern struct sk_buff * __pfq_alloc_skb(unsigned int size, gfp_t priority, int fclone, int node);
extern struct sk_buff * pfq_dev_alloc_skb(unsigned int length);
extern struct sk_buff * __pfq_netdev_alloc_skb(struct net_device *dev, unsigned int length, gfp_t gfp);


static inline bool pfq_skb_is_recycleable(const struct sk_buff *skb)
{
	if (unlikely(irqs_disabled())) {
		sparse_inc(global->percpu_memory, err_irqdis);
		return false;
	}

	if (skb->fclone != SKB_FCLONE_UNAVAILABLE) {
		sparse_inc(global->percpu_memory, err_fclone);
		return false;
	}

	/*  check whether the skb is shared with someone else.. */

	if (unlikely(atomic_read(&skb->users) > 1)) {
		sparse_inc(global->percpu_memory, err_shared);
		return false;
	}

	if(unlikely(skb_cloned(skb))) {
		sparse_inc(global->percpu_memory, err_cloned);
		return false;
	}

	if (unlikely(skb_is_nonlinear(skb))) {
		sparse_inc(global->percpu_memory, err_nolinr);
		return false;
	}
#if 0
	skb_size = SKB_DATA_ALIGN(skb_size);
	if (unlikely(pfq_skb_end_offset(skb) < skb_size)) {
		sparse_inc(global->percpu_memory, err_memory);
		return false;
	}
#endif

	return true;
}


static inline void
pfq_skb_release_head_state(struct sk_buff *skb)
{
	if (unlikely(skb->_skb_refdst)) {
                sparse_inc(global->percpu_memory, dbg_dst_drop);
		skb_dst_drop(skb);
	}

#ifdef CONFIG_XFRM
	secpath_put(skb->sp);
#endif
	if (unlikely(skb->destructor)) {
                sparse_inc(global->percpu_memory, dbg_skb_dtor);
		WARN_ON(in_irq());
		skb->destructor(skb);
	}

// #if IS_ENABLED(CONFIG_NF_CONNTRACK)
// 	nf_conntrack_put(skb->nfct);
// #endif
//
// #ifdef NET_SKBUFF_NF_DEFRAG_NEEDED
// 	nf_conntrack_put_reasm(skb->nfct_reasm);
// #endif
// #ifdef CONFIG_BRIDGE_NETFILTER
// 	nf_bridge_put(skb->nf_bridge);
// #endif

}


static inline
void pfq_kfree_skb_list(struct sk_buff *segs)
{
	while (segs) {
		struct sk_buff *next = segs->next;
                sparse_inc(global->percpu_memory, dbg_skb_free_frag);
		kfree_skb(segs);
		segs = next;
	}
}


#if defined(PFQ_FULL_SKB_RECYCLE)
static inline
void skb_free_head(struct sk_buff *skb)
{
        unsigned char *head = skb->head;

        if (skb->head_frag) {
                sparse_inc(global->percpu_memory, dbg_skb_free_frag);
                skb_free_frag(head);
	}
        else {
                sparse_inc(global->percpu_memory, dbg_skb_free_head);
                kfree(head);
	}
}
#endif

static inline void
pfq_skb_release_data(struct sk_buff *skb)
{
	int i;
	struct skb_shared_info *shinfo = skb_shinfo(skb);

	for (i = 0; i < shinfo->nr_frags; i++) {
                sparse_inc(global->percpu_memory, dbg_skb_frag_unref);
		__skb_frag_unref(&shinfo->frags[i]);
	}

	if (unlikely(shinfo->frag_list))
		pfq_kfree_skb_list(shinfo->frag_list);

#if defined(PFQ_FULL_SKB_RECYCLE)
        skb_free_head(skb);
#endif

}


static inline
struct sk_buff *
pfq_skb_recycle(struct sk_buff *skb)
{
	struct skb_shared_info *shinfo;

#if (LINUX_VERSION_CODE > KERNEL_VERSION(3,5,0))
	bool pfmemalloc;
#endif

	int  pool;
        uint32_t id;
        struct sk_buff *addr;

	/* reset skb */

#if (LINUX_VERSION_CODE > KERNEL_VERSION(3,5,0))
	pfmemalloc = skb->pfmemalloc;
#endif

	pool = PFQ_CB(skb)->pool;
        id   = PFQ_CB(skb)->id;
        addr = PFQ_CB(skb)->addr;

	//
	// size = SKB_DATA_ALIGN(size);
        //

	memset(skb, 0, offsetof(struct sk_buff, tail));

	// skb->truesize = SKB_TRUESIZE(size);
	//

#if (LINUX_VERSION_CODE > KERNEL_VERSION(3,5,0))
	skb->pfmemalloc = pfmemalloc;
#endif

	PFQ_CB(skb)->pool = pool;
	PFQ_CB(skb)->addr = addr;
	PFQ_CB(skb)->id = id;

	atomic_set(&skb->users, 1);

	/* head + end are already set */

	skb->data = skb->head;
	skb_reset_tail_pointer(skb);

	//
        // skb->end = skb->tail + size;
        //

	skb->mac_header = (typeof(skb->mac_header))~0U;
	skb->transport_header = (typeof(skb->transport_header))~0U;

	shinfo = skb_shinfo(skb);

#if defined(PFQ_FULL_SKB_RECYCLE)
	memset(shinfo, 0, offsetof(struct skb_shared_info, dataref));
	kmemcheck_annotate_variable(shinfo->destructor_arg);
#endif
	atomic_set(&shinfo->dataref,1);

	skb->nf_trace = 1;
	return skb;
}


static inline
struct sk_buff * pfq_netdev_alloc_skb(struct net_device *dev, unsigned int length)
{
	return __pfq_netdev_alloc_skb(dev, length, GFP_ATOMIC);
}


static inline
struct sk_buff * __pfq_netdev_alloc_skb_ip_align(struct net_device *dev,
						 unsigned int length, gfp_t gfp)
{
	struct sk_buff *skb = __pfq_netdev_alloc_skb(dev, length + NET_IP_ALIGN,
						     gfp);
	if (likely(skb))
		skb_reserve(skb, NET_IP_ALIGN);
	return skb;
}


static inline
struct sk_buff * pfq_netdev_alloc_skb_ip_align(struct net_device *dev,
					       unsigned int length)
{
	return __pfq_netdev_alloc_skb_ip_align(dev, length, GFP_ATOMIC);
}


static inline
struct sk_buff *
____pfq_alloc_skb_pool(unsigned int size, gfp_t priority, int fclone, int node, int idx, struct core_spsc_fifo *pool)
{
#ifdef PFQ_USE_SKB_POOL
	if (likely(pool)) {
		struct sk_buff *skb = core_spsc_peek(pool);
		if (likely(skb != NULL)) {
			if(pfq_skb_is_recycleable(skb)) {
				sparse_inc(global->percpu_memory, pool_pop[idx]);
				core_spsc_consume(pool);

				pfq_skb_release_head_state(skb);
				pfq_skb_release_data(skb);

				return pfq_skb_recycle(skb);
			}
			else {
				sparse_inc(global->percpu_memory, pool_norecycl[idx]);
			}
		}
		else {
			sparse_inc(global->percpu_memory, pool_empty[idx]);
		}
	}
#endif
	sparse_inc(global->percpu_memory, os_alloc);
	return  __alloc_skb(size, priority, fclone, node);
}


static inline
void pfq_kfree_skb_pool(struct sk_buff *skb, struct pfq_skb_pool *pool)
{
#ifdef PFQ_USE_SKB_POOL
	if (skb->nf_trace) {
		const int idx = PFQ_CB(skb)->pool;
		if (pool->fifo) {
			if (!core_spsc_push(pool->fifo, skb)) {
				pfq_skb_dump("[PFQ] BUG [internal error!]", skb);
				sparse_inc(global->percpu_memory, os_free);
				kfree_skb(skb);
				return;
			}

			sparse_inc(global->percpu_memory, pool_push[idx]);
			return;
		}
	}
#endif
	sparse_inc(global->percpu_memory, os_free);
	kfree_skb(skb);
}


static inline
struct sk_buff * pfq_alloc_skb(unsigned int size, gfp_t priority)
{
#ifdef PFQ_USE_SKB_POOL

	if (likely(atomic_read(&global->pool_enabled))) {
		struct pfq_percpu_pool *cpu_pool = this_cpu_ptr(global->percpu_pool);
		struct core_spsc_fifo *pool = pfq_skb_pool_get(&cpu_pool->rx, size);
		return ____pfq_alloc_skb_pool(size, priority, 0, NUMA_NO_NODE, 0, pool);
	}

#endif
	sparse_inc(global->percpu_memory, os_alloc);
	return __alloc_skb(size, priority, 0, NUMA_NO_NODE);
}


static inline
struct sk_buff * pfq_alloc_skb_fclone(unsigned int size, gfp_t priority)
{
	return __pfq_alloc_skb(size, priority, 1, NUMA_NO_NODE);
}


/* explicit pool allocation/free: pool can be NULL */


static inline
struct sk_buff *
pfq_alloc_skb_pool(unsigned int size, gfp_t priority, int node, int idx, struct pfq_skb_pool *pool)
{
#ifdef PFQ_USE_SKB_POOL
	return ____pfq_alloc_skb_pool(size, priority, 0, node, idx, pool->fifo);
#endif
	sparse_inc(global->percpu_memory, os_alloc);
	return __alloc_skb(size, priority, 0, NUMA_NO_NODE);
}



#endif /* PFQ_MEMORY_H */
