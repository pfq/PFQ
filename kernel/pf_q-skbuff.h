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

#ifndef _PF_Q_SKBUFF_H_
#define _PF_Q_SKBUFF_H_

#include <linux/skbuff.h>

struct pfq_monad;
struct gc_log;


struct pfq_cb
{
	unsigned long 	 mark;
        unsigned long 	 group_mask;
	struct gc_log 	 *log;
	struct pfq_monad *monad;
	int 		 direct;
};


#define PFQ_CB(skb) ((struct pfq_cb *)(skb)->cb)


/* wrapper used in garbage collector */

struct gc_buff
{
 	struct sk_buff *skb;
};


#define for_each_skbuff(batch, skb, n) \
        for((n) = 0; ((n) < (batch)->len) && ((skb) = (batch)->queue[n]); \
                __builtin_prefetch((batch)->queue[n+1], 0, 1), (n)++)


#define for_each_skbuff_from(x, batch, skb, n) \
        for((n) = (x); ((n) < (batch)->len) && ((skb) = (batch)->queue[n]); \
                __builtin_prefetch((batch)->queue[n+1], 0, 1), (n)++)


#define for_each_skbuff_upto(max, batch, skb, n) \
        for((n) = 0; ((n) < (max)) && ((n) < (batch)->len) && ((skb) = (batch)->queue[n]); \
                __builtin_prefetch((batch)->queue[n+1], 0, 1), (n)++)


#define for_each_skbuff_backward(batch, skb, n) \
        for((n) = (batch)->len; ((n) > 0) && ((skb) = (batch)->queue[n-1]); \
                __builtin_prefetch((batch)->queue[n-2], 0, 1), (n)--)


#define for_each_skbuff_bitmask(batch, mask, skb, n) \
        for((n) = pfq_ctz(mask); (mask) && ((skb = (batch)->queue[n]), true); \
                (mask) ^=(1UL << (n)), n = pfq_ctz(mask))


#endif /* _PF_Q_SKBUFF_H_ */
