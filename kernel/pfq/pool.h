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


#define PFQ_POOL_SKB_SML  320
#define PFQ_POOL_SKB_MID  576
#define PFQ_POOL_SKB_LRG  2048


struct pfq_skb_pools
{
	struct core_spsc_fifo *fifo_sml;
	struct core_spsc_fifo *fifo_mid;
	struct core_spsc_fifo *fifo_lrg;
};


extern int	pfq_skb_pool_init_all(void);
extern int	pfq_skb_pool_free_all(void);

extern struct  core_pool_stat pfq_get_skb_pool_stats(void);

#define PFQ_SKB_POOL_IDX(size) (size <= PFQ_POOL_SKB_SML ? 0 : \
				size <= PFQ_POOL_SKB_MID ? 1 : \
				size <= PFQ_POOL_SKB_LRG ? 2 : (-1))

static inline
struct core_spsc_fifo *pfq_skb_pool_get(struct pfq_skb_pools *pools, size_t size)
{
	if (size <= PFQ_POOL_SKB_SML)
		return pools->fifo_sml;
	if (size <= PFQ_POOL_SKB_MID)
		return pools->fifo_mid;
	if (size <= PFQ_POOL_SKB_LRG)
		return pools->fifo_lrg;
	return NULL;
}


static inline
struct core_spsc_fifo *pfq_skb_pool_idx(struct pfq_skb_pools *pools, size_t idx)
{
	switch(idx)
	{
	case 0: return pools->fifo_sml;
	case 1: return pools->fifo_mid;
	case 2: return pools->fifo_lrg;
	default:return NULL;
	}
}




#endif /* PFQ_SKBUFF_POOL_H */
