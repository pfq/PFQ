/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PFQ_SKBUFF_POOL_H
#define PFQ_SKBUFF_POOL_H

#include <pragma/diagnostic_push>
#include <linux/skbuff.h>
#include <pragma/diagnostic_pop>

#include <core/spsc_fifo.h>
#include <core/global.h>
#include <core/stats.h>


typedef struct core_spsc_fifo pfq_skb_pool_t;


extern int	pfq_skb_pool_init_all(void);
extern int	pfq_skb_pool_free_all(void);

extern void	pfq_skb_pool_toggle(bool);

extern struct	core_pool_stat pfq_get_skb_pool_stats(void);



static inline
void pfq_skb_pool_enable(void)
{
	pfq_skb_pool_toggle(true);
}

static inline
void pfq_skb_pool_disable(void)
{
	pfq_skb_pool_toggle(false);
}


static inline
struct sk_buff *pfq_skb_pool_pop(pfq_skb_pool_t *pool)
{
	return core_spsc_pop(pool);
}

static inline
struct sk_buff *pfq_skb_pool_peek(pfq_skb_pool_t *pool)
{
	return core_spsc_peek(pool);
}

static inline
void pfq_skb_pool_discard(pfq_skb_pool_t *pool)
{
	core_spsc_discard(pool);
}

static inline
bool pfq_skb_pool_push(pfq_skb_pool_t *pool, struct sk_buff *skb)
{
	return core_spsc_push(pool, skb);
}

static inline
size_t pfq_skb_pool_size(pfq_skb_pool_t const *pool)
{
	return core_spsc_len(pool);
}

#endif /* PFQ_SKBUFF_POOL_H */
