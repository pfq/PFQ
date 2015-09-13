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


#ifndef PF_Q_SKB_QUEUE_H
#define PF_Q_SKB_QUEUE_H

#include <pf_q-types.h>

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


struct pfq_skbuff_batch;
struct GC_skbuff_batch;
struct GC_skbuff_queue;


#define SKBUFF_QUEUE_ADDR(q) \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct GC_skbuff_queue ), (struct pfq_skbuff_queue *)&q, \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct GC_skbuff_batch ), (struct pfq_skbuff_queue *)&q, \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct pfq_skbuff_batch), (struct pfq_skbuff_queue *)&q, (void) 0)))


#define SKBUFF_GC_QUEUE_ADDR(q) \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct GC_skbuff_queue ), (struct pfq_skbuff_GC_queue *)&q, \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct GC_skbuff_batch ), (struct pfq_skbuff_GC_queue *)&q, (void) 0))


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


#endif /* PF_Q_SKB_QUEUE_H */
