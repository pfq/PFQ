/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
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

#include <lang/types.h>

#include <pf_q-types.h>
#include <pf_q-bitops.h>
#include <pf_q-define.h>


struct pfq_skbuff_batch;
struct pfq_lang_monad;

struct GC_log;
struct GC_skbuff_batch;
struct GC_skbuff_queue;


struct pfq_cb
{
	struct GC_log	 *log;
	struct pfq_lang_monad *monad;
        unsigned long	  group_mask;
        uint32_t	  state;
	bool		  direct;
};


struct pfq_skbuff_batch
{
        size_t len;
        struct sk_buff *queue[Q_SKBUFF_BATCH];
};


struct pfq_skbuff_queue
{
        size_t len;
        struct sk_buff *queue[];
};


struct pfq_skbuff_GC_queue
{
        size_t len;
        struct sk_buff __GC *queue[];
};


#define PFQ_SKB(skb)  ((struct sk_buff __force *)skb)
#define PFQ_CB(skb)   ((struct pfq_cb *)PFQ_SKB(skb)->cb)


#define SKBUFF_QUEUE_ADDR(q) \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct GC_skbuff_queue ), (struct pfq_skbuff_queue *)&q, \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct GC_skbuff_batch ), (struct pfq_skbuff_queue *)&q, \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct pfq_skbuff_batch), (struct pfq_skbuff_queue *)&q, (void) 0)))


#define SKBUFF_GC_QUEUE_ADDR(q) \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct GC_skbuff_queue ), (struct pfq_skbuff_GC_queue *)&q, \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct GC_skbuff_batch ), (struct pfq_skbuff_GC_queue *)&q, (void) 0))


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


static inline
void pfq_skbuff_batch_drop_n(struct pfq_skbuff_batch *q, size_t n)
{
	size_t i;

	if (n > q->len)
		n = q->len;

	for(i = 0; i < (q->len - n); i++)
	{
		q->queue[i] = q->queue[i + n];
	}

	q->len -= n;
}


static inline
int pfq_skbuff_batch_push(struct pfq_skbuff_batch *q, struct sk_buff *skb)
{
        if (q->len < Q_SKBUFF_BATCH)
                return q->queue[q->len++] = skb, 0;
        return -ENOMEM;
}


static inline
void pfq_skbuff_batch_init(struct pfq_skbuff_batch *q)
{
	q->len = 0;
}


static inline
struct sk_buff *
pfq_skbuff_batch_pop(struct pfq_skbuff_batch *q)
{
        if (q->len > 0)
                return q->queue[--q->len];
        return NULL;
}


static inline
void pfq_skbuff_batch_clear(struct pfq_skbuff_batch *q)
{
        q->len = 0;
}


static inline
size_t pfq_skbuff_batch_len(struct pfq_skbuff_batch *q)
{
        return q->len;
}


static inline
void pfq_skbuff_queue_init(struct pfq_skbuff_queue *q)
{
	q->len = 0;
}


static inline
struct sk_buff *
pfq_skbuff_queue_pop(struct pfq_skbuff_queue *q)
{
        if (q->len > 0)
                return q->queue[--q->len];
        return NULL;
}


static inline
void pfq_skbuff_queue_clear(struct pfq_skbuff_queue *q)
{
        q->len = 0;
}


static inline
size_t pfq_skbuff_queue_len(struct pfq_skbuff_queue *q)
{
        return q->len;
}

#endif /* PF_Q_SKBUFF_H */
