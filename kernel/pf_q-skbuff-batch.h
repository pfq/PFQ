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


#ifndef PF_Q_SKB_BATCH_H
#define PF_Q_SKB_BATCH_H


#include <pf_q-bitops.h>
#include <pf_q-macro.h>
#include <pf_q-skbuff.h>


struct pfq_skbuff_short_batch
{
        size_t len;
        struct sk_buff *queue[Q_SKBUFF_SHORT_BATCH];
};

struct pfq_skbuff_long_batch
{
        size_t len;
        struct sk_buff *queue[Q_SKBUFF_LONG_BATCH];
};

struct pfq_skbuff_batch
{
        size_t len;
        struct sk_buff *queue[];
};


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
int pfq_skbuff_short_batch_push(struct pfq_skbuff_batch *q, struct sk_buff *skb)
{
        if (q->len < Q_SKBUFF_SHORT_BATCH)
                return q->queue[q->len++] = skb, 0;
        return -ENOMEM;
}


static inline
int pfq_skbuff_long_batch_push(struct pfq_skbuff_batch *q, struct sk_buff *skb)
{
        if (q->len < Q_SKBUFF_LONG_BATCH)
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


#endif /* PF_Q_SKB_BATCH_H */
