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

#ifndef PF_Q_SKBUFF_H
#define PF_Q_SKBUFF_H

#include <pragma/diagnostic_push>
#include <linux/skbuff.h>
#include <pragma/diagnostic_pop>

#include <pf_q-types.h>

struct pfq_monad;
struct GC_log;


struct pfq_cb
{
	struct GC_log	 *log;
	struct pfq_monad *monad;
        unsigned long	  group_mask;
	int		  direct;
};


#define PFQ_SKB(skb)  ((struct sk_buff __force *)skb)
#define PFQ_CB(skb)   ((struct pfq_cb *)PFQ_SKB(skb)->cb)


#define for_each_skbuff(q, skb, n) \
        for((n) = 0; ((n) < (q)->len) && ((skb) = (q)->queue[n]); \
                __builtin_prefetch((__force void const *)(q)->queue[n+1], 0, 1), (n)++)


#define for_each_skbuff_from(x, q, skb, n) \
        for((n) = (x); ((n) < (q)->len) && ((skb) = (q)->queue[n]); \
                __builtin_prefetch((__force void const *)(q)->queue[n+1], 0, 1), (n)++)


#define for_each_skbuff_upto(max, q, skb, n) \
        for((n) = 0; ((n) < (max)) && ((n) < (q)->len) && ((skb) = (q)->queue[n]); \
                __builtin_prefetch((__force void const *)(q)->queue[n+1], 0, 1), (n)++)


#define for_each_skbuff_backward(q, skb, n) \
        for((n) = (q)->len; ((n) > 0) && ((skb) = (q)->queue[n-1]); \
                __builtin_prefetch((__force void const *)(q)->queue[n-2], 0, 1), (n)--)


#define for_each_skbuff_bitmask(q, mask, skb, n) \
        for((n) = pfq_ctz(mask); (mask) && (skb = (q)->queue[n]); \
                (mask) ^=(1ULL << (n)), n = pfq_ctz(mask))


#endif /* PF_Q_SKBUFF_H */
