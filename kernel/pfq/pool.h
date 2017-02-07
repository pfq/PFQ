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


#define PFQ_POOL_SKB_SIZE		2048
#define PFQ_POOL_CACHELINE_PAD		(64/sizeof(void *))


struct pfq_skb_pool
{
	struct core_spsc_fifo *fifo;
};


extern int	pfq_skb_pool_init_all(void);
extern int	pfq_skb_pool_free_all(void);

extern struct  core_pool_stat pfq_get_skb_pool_stats(void);


static inline
struct core_spsc_fifo *pfq_skb_pool_get(struct pfq_skb_pool *pool, size_t size)
{
	if (likely(size <= PFQ_POOL_SKB_SIZE))
		return pool->fifo;
	return NULL;
}



#endif /* PFQ_SKBUFF_POOL_H */
