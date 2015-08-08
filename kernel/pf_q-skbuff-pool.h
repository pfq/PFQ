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

#include <pragma/diagnostic_push>
#include <linux/skbuff.h>
#include <pragma/diagnostic_pop>

#include <pf_q-global.h>
#include <pf_q-stats.h>

struct pfq_skb_pool
{
	struct sk_buff ** skbs;
	size_t size;
	size_t p_idx;
	size_t c_idx;
};



void	pfq_skb_pool_enable(bool value);
int     pfq_skb_pool_init_all(void);
int	pfq_skb_pool_free_all(void);
int	pfq_skb_pool_flush_all(void);

int	pfq_skb_pool_init (struct pfq_skb_pool *pool, size_t size);
size_t	pfq_skb_pool_free (struct pfq_skb_pool *pool);
size_t	pfq_skb_pool_flush(struct pfq_skb_pool *pool);

struct  pfq_pool_stat pfq_get_skb_pool_stats(void);


static inline
struct sk_buff *pfq_skb_pool_pop(struct pfq_skb_pool *pool)
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
bool pfq_skb_pool_push(struct pfq_skb_pool *pool, struct sk_buff *nskb)
{
	bool ret = false;
	if (likely(pool->skbs)) {

		struct sk_buff *skb = __atomic_load_n(&pool->skbs[pool->p_idx], __ATOMIC_RELAXED);
		if (likely(!skb)) {
			__atomic_store_n(&pool->skbs[pool->p_idx], nskb, __ATOMIC_RELAXED);
			ret = true;
		}
		else {
			kfree_skb(nskb);
		}

		if (++pool->p_idx >= pool->size)
			pool->p_idx = 0;

	} else {
		kfree_skb(nskb);
	}

	return ret;
}

#endif /* PF_Q_SKBUFF_POOL_H */
