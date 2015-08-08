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

#ifndef PF_Q_MEMORY_H
#define PF_Q_MEMORY_H

#include <pragma/diagnostic_push>
#include <linux/version.h>
#include <linux/skbuff.h>
#include <linux/hardirq.h>
#include <net/dst.h>
#include <pragma/diagnostic_pop>

#include <pf_q-skbuff-pool.h>
#include <pf_q-macro.h>
#include <pf_q-percpu.h>
#include <pf_q-sparse.h>
#include <pf_q-global.h>

extern int skb_pool_size;
extern struct local_data __percpu * cpu_data;

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


static inline bool pfq_skb_is_recycleable(const struct sk_buff *skb, int skb_size)
{
	if (irqs_disabled()) {
		SPARSE_INC(&memory_stats.err_intdis);
		return false;
	}

	if (skb_is_nonlinear(skb))
		return false;

	if (skb->fclone != SKB_FCLONE_UNAVAILABLE)
		return false;

	/*  check whether the skb is shared with someone else.. */

	if (atomic_read(&skb->users) > 1) {
		SPARSE_INC(&memory_stats.err_shared);
		return false;
	}

	if(skb_cloned(skb)) {
		SPARSE_INC(&memory_stats.err_cloned);
		return false;
	}

	skb_size = SKB_DATA_ALIGN(skb_size + NET_SKB_PAD);

	if (pfq_skb_end_offset(skb) < skb_size) {
		SPARSE_INC(&memory_stats.err_memory);
		return false;
	}

	return true;
}


static inline void
skb_release_head_state(struct sk_buff *skb)
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
	/*  skb_recycle(skb); removed from kernel 3.7 */

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
struct sk_buff * __pfq_netdev_alloc_skb_ip_align(struct net_device *dev,
						 unsigned int length, gfp_t gfp)
{
	struct sk_buff *skb = __pfq_netdev_alloc_skb(dev, length + NET_IP_ALIGN,
						     gfp);
	if (NET_IP_ALIGN && likely(skb))
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
____pfq_alloc_skb_pool(unsigned int size, gfp_t priority, int fclone, int node, struct pfq_skb_pool *pool)
{

#ifdef PFQ_USE_SKB_POOL
	struct sk_buff *skb = pfq_skb_pool_pop(pool);
	if (likely(skb != NULL)) {
		SPARSE_INC(&memory_stats.pool_pop);

		if (likely(pfq_skb_is_recycleable(skb, size))) {
			SPARSE_INC(&memory_stats.pool_alloc);
			return pfq_skb_recycle(skb);
		} else {
			SPARSE_INC(&memory_stats.err_norecyl);
			SPARSE_INC(&memory_stats.os_free);
			kfree_skb(skb);
		}
	}
	else {
		SPARSE_INC(&memory_stats.err_pop);
	}
#endif

	SPARSE_INC(&memory_stats.os_alloc);
	return  __alloc_skb(size, priority, fclone, node);
}


static inline
void pfq_kfree_skb_pool(struct sk_buff *skb, struct pfq_skb_pool *pool)
{
#ifdef PFQ_USE_SKB_POOL
	bool ret = pfq_skb_pool_push(pool, skb);
	if (ret)
		SPARSE_INC(&memory_stats.pool_push);
	else
		SPARSE_INC(&memory_stats.err_push);
#else
	SPARSE_INC(&memory_stats.os_free);
	kfree_skb(skb);
#endif
}


static inline
struct sk_buff * pfq_alloc_skb(unsigned int size, gfp_t priority)
{
#ifdef PFQ_USE_SKB_POOL
	struct local_data *this_cpu = this_cpu_ptr(cpu_data);

	if (likely(atomic_read(&this_cpu->enable_skb_pool)))
		return ____pfq_alloc_skb_pool(size, priority, 0, NUMA_NO_NODE, &this_cpu->rx_pool);

	SPARSE_INC(&memory_stats.os_alloc);
#endif
	return __alloc_skb(size, priority, 0, NUMA_NO_NODE);
}


static inline
struct sk_buff * pfq_tx_alloc_skb(unsigned int size, gfp_t priority, int node)
{
#ifdef PFQ_USE_SKB_POOL
	struct local_data *this_cpu = this_cpu_ptr(cpu_data);

	if (likely(atomic_read(&this_cpu->enable_skb_pool)))
		return ____pfq_alloc_skb_pool(size, priority, 0, node, &this_cpu->tx_pool);

	SPARSE_INC(&memory_stats.os_alloc);
#endif
	return __alloc_skb(size, priority, 0, NUMA_NO_NODE);
}


static inline
struct sk_buff * pfq_alloc_skb_fclone(unsigned int size, gfp_t priority)
{
	return __pfq_alloc_skb(size, priority, 1, NUMA_NO_NODE);
}


#endif /* PF_Q_MEMORY_H */
