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

#include <pfq/alloc.h>
#include <pfq/define.h>
#include <pfq/global.h>
#include <pfq/percpu.h>
#include <pfq/pool.h>
#include <pfq/skbuff.h>
#include <pfq/sparse.h>
#include <pfq/stats.h>

#include <linux/version.h>
#include <linux/skbuff.h>
#include <linux/hardirq.h>
#include <net/dst.h>
#include <net/xfrm.h>


extern struct sk_buff * __pfq_alloc_skb(unsigned int size, gfp_t priority, int fclone, int node);
extern struct sk_buff * pfq_dev_alloc_skb(unsigned int length);
extern struct sk_buff * __pfq_netdev_alloc_skb(struct net_device *dev, unsigned int length, gfp_t gfp);


static inline bool pfq_skb_is_recycleable(const struct sk_buff *skb)
{
#ifdef PFQ_USE_EXTRA_COUNTERS
	if(unlikely(PFQ_CB(skb)->head != skb->head)) {
		sparse_inc(global->percpu_memory, err_broken);
		return false;
	}

	if (unlikely(irqs_disabled())) {
		sparse_inc(global->percpu_memory, err_irqdis);
		return false;
	}

	if (unlikely(skb->fclone != SKB_FCLONE_UNAVAILABLE)) {
		sparse_inc(global->percpu_memory, err_fclone);
		return false;
	}

	if (unlikely(atomic_read(&skb->users) > 1)) {
		sparse_inc(global->percpu_memory, err_shared);
		return false;
	}

	if(unlikely(skb_cloned(skb))) {
		sparse_inc(global->percpu_memory, err_cloned);
		return false;
	}

#if 0
	if (unlikely(skb_is_nonlinear(skb))) {
		sparse_inc(global->percpu_memory, err_nolinr);
		return false;
	}
#endif
#else
	if(unlikely(PFQ_CB(skb)->head != skb->head		||
		    irqs_disabled()				||
		    skb->fclone != SKB_FCLONE_UNAVAILABLE	||
		    atomic_read(&skb->users) > 1		||
		    skb_cloned(skb)))
		return false;
#endif
	return true;
}


static inline void
pfq_skb_release_head_state(struct sk_buff *skb)
{
	if (unlikely(skb->_skb_refdst)) {
#ifdef PFQ_USE_EXTRA_COUNTERS
                sparse_inc(global->percpu_memory, dbg_dst_drop);
#endif
		skb_dst_drop(skb);
	}

#ifdef CONFIG_XFRM
	secpath_put(skb->sp);
#endif
	if (unlikely(skb->destructor)) {
#ifdef PFQ_USE_EXTRA_COUNTERS
                sparse_inc(global->percpu_memory, dbg_skb_dtor);
#endif
		WARN_ON(in_irq());
		skb->destructor(skb);
	}
}


static inline
void pfq_kfree_skb_list(struct sk_buff *segs)
{
	while (segs) {
		struct sk_buff *next = segs->next;
#ifdef PFQ_USE_EXTRA_COUNTERS
                sparse_inc(global->percpu_memory, dbg_skb_free_frag);
#endif
		kfree_skb(segs);
		segs = next;
	}
}


static inline void
pfq_skb_release_data(struct sk_buff *skb)
{
	int i;
	struct skb_shared_info *shinfo = skb_shinfo(skb);

	for (i = 0; i < shinfo->nr_frags; i++) {
#ifdef PFQ_USE_EXTRA_COUNTERS
                sparse_inc(global->percpu_memory, dbg_skb_frag_unref);
#endif
		__skb_frag_unref(&shinfo->frags[i]);
	}

	if (unlikely(shinfo->frag_list))
		pfq_kfree_skb_list(shinfo->frag_list);

        if (unlikely(skb->head_frag)) {
		unsigned char *head = skb->head;
#ifdef PFQ_USE_EXTRA_COUNTERS
                sparse_inc(global->percpu_memory, dbg_skb_free_frag);
#endif
                skb_free_frag(head);
	}
}


static inline
struct sk_buff *
pfq_skb_recycle(struct sk_buff *skb)
{
	struct skb_shared_info *shinfo;

	shinfo = skb_shinfo(skb);
	__builtin_memset(shinfo, 0, offsetof(struct skb_shared_info, dataref));
	atomic_set(&shinfo->dataref,1);
	// kmemcheck_annotate_variable(shinfo->destructor_arg);
#if 1
	__builtin_memcpy(skb, skb+2048, sizeof(struct sk_buff));
#else
	unsigned int size = 2048;
	void *data;
	data = skb->data;
	size -= SKB_DATA_ALIGN(sizeof(struct skb_shared_info));
	memset(skb, 0, offsetof(struct sk_buff, tail));

	skb->truesize = SKB_TRUESIZE(size);
	atomic_set(&skb->users, 1);
	skb->head = data;
	skb->data = data;
	skb_reset_tail_pointer(skb);
	skb->end = skb->tail + size;
	skb->mac_header = (typeof(skb->mac_header))~0U;
	skb->transport_header = (typeof(skb->transport_header))~0U;
#endif

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
____pfq_alloc_skb_pool(unsigned int size, gfp_t priority, int fclone, int node, int idx, struct pfq_spsc_fifo *pool)
{
#ifdef PFQ_USE_SKB_POOL
	if (likely(pool)) {
		struct sk_buff *skb = pfq_spsc_pop(pool);
		if (likely(skb && pfq_skb_is_recycleable(skb))) {

			sparse_inc(global->percpu_memory, pool_pop[idx]);

			// pfq_skb_release_head_state(skb);

			pfq_skb_release_data(skb);

			return pfq_skb_recycle(skb);
		}
		else {
			if (skb)
				sparse_inc(global->percpu_memory, pool_norecycl[idx]);
			else
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
	if (likely(skb->peeked)) {
		const int idx = PFQ_CB(skb)->pool;
		if (likely(pool->fifo)) {
			if (unlikely(!pfq_spsc_push(pool->fifo, skb))) {
				pfq_printk_skb("[PFQ] internal error", skb);
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
		struct pfq_spsc_fifo *pool = pfq_skb_pool_get(&cpu_pool->rx, size);
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
