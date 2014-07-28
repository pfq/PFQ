/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola.bonelli@cnit.it>
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


#ifndef _PF_Q_BOUNDED_QUEUE_H_
#define _PF_Q_BOUNDED_QUEUE_H_


#include <pf_q-bitops.h>
#include <pf_q-common.h>


struct pfq_bounded_queue_skb
{
        size_t len;
        struct sk_buff *queue[Q_BOUNDED_QUEUE_LEN];  	/* sk_buff */
};


#define pfq_bounded_queue_for_each(skb, n, q) \
        for(n = 0; (n != (q)->len) && (skb = (q)->queue[n]); \
                __builtin_prefetch((q)->queue[n+1], 0, 1), n++)


#define pfq_bounded_queue_for_each_backward(skb, n, q) \
        for(n = (q)->len; (n > 0) && (skb = (q)->queue[n-1]); \
                __builtin_prefetch((q)->queue[n-2], 0, 1), n--)


#define pfq_bounded_queue_for_each_bitmask(skb, mask, n, q) \
        for(n = pfq_ctz(mask); mask && ((skb = (q)->queue[n]), true); \
                mask ^=(1UL << n), n = pfq_ctz(mask))


static inline
int pfq_bounded_queue_push(struct pfq_bounded_queue_skb *q, struct sk_buff *skb)
{
        if (q->len < Q_BOUNDED_QUEUE_LEN)
                return q->queue[q->len++] = skb, 0;
        return -1;
}


static inline
void pfq_bounded_queue_init(struct pfq_bounded_queue_skb *q)
{
    q->len = 0;
}


static inline
struct sk_buff * pfq_bounded_queue_pop(struct pfq_bounded_queue_skb *q)
{
        if (q->len > 0)
                return q->queue[--q->len];
        return NULL;
}


static inline
void pfq_bounded_queue_flush(struct pfq_bounded_queue_skb *q)
{
        q->len = 0;
}


static inline
size_t pfq_bounded_queue_len(struct pfq_bounded_queue_skb *q)
{
        return q->len;
}


#endif /* _PF_Q_BOUNDED_QUEUE_H_ */
