/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PF_Q_SKBUFF_POOL_H
#define PF_Q_SKBUFF_POOL_H

#include <warning/push>
#include <linux/skbuff.h>
#include <warning/pop>

struct pfq_sk_buff_pool
{
	struct sk_buff ** skbs;
	size_t size;
	size_t p_idx;
	size_t c_idx;
};


static inline
int pfq_sk_buff_pool_init (struct pfq_sk_buff_pool *pool, size_t size)
{
	if (size > 0) {
		pool->skbs = kzalloc(sizeof(struct sk_buff *) * size, GFP_KERNEL);
		if (pool->skbs == NULL) {
			printk(KERN_INFO "[PFQ] pfq_sk_buff_pool_init: out of memory!\n");
			return -ENOMEM;
		}
	}
	else {
		pool->skbs = NULL;
	}

	pool->size  = size;
	pool->p_idx = 0;
	pool->c_idx = 0;
	return 0;
}


static inline
size_t
pfq_sk_buff_pool_purge(struct pfq_sk_buff_pool *pool)
{
	size_t n, total = 0;
	for(n = 0; n < pool->size; n++)
	{
		if (pool->skbs[n]) {
			total++;
			kfree_skb(pool->skbs[n]);
			pool->skbs[n] = NULL;
		}
	}

	pool->p_idx = 0;
	pool->c_idx = 0;
	return total;
}


static inline
size_t pfq_sk_buff_pool_free(struct pfq_sk_buff_pool *pool)
{
	size_t total = pfq_sk_buff_pool_purge(pool);
	kfree(pool->skbs);
	pool->skbs = NULL;
	pool->size = 0;
	return total;
}


static inline
struct sk_buff *pfq_sk_buff_pool_get(struct pfq_sk_buff_pool *pool)
{
	if (likely(pool->skbs)) {

		struct sk_buff *skb = __atomic_load_n(&pool->skbs[pool->c_idx], __ATOMIC_RELAXED);
		if (likely(skb)) {
			__atomic_store_n(&pool->skbs[pool->c_idx], NULL, __ATOMIC_RELAXED);
		}

		if (++pool->c_idx >= pool->size)
			pool->c_idx = 0;

		return skb;
	}
	return NULL;
}


static inline
void pfq_sk_buff_pool_put(struct pfq_sk_buff_pool *pool, struct sk_buff *nskb)
{
	if (likely(pool->skbs)) {

		/* most of the time skb is NULL */

		struct sk_buff *skb = __atomic_load_n(&pool->skbs[pool->p_idx], __ATOMIC_RELAXED);
		if (likely(!skb)) {
			__atomic_store_n(&pool->skbs[pool->p_idx], nskb, __ATOMIC_RELAXED);
		}
		else {
			kfree_skb(nskb);
		}

		if (++pool->p_idx >= pool->size)
			pool->p_idx = 0;

	} else {
		kfree_skb(nskb);
	}
}

#endif /* PF_Q_SKBUFF_POOL_H */
