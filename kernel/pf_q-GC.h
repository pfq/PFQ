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

#ifndef _PF_Q_GC_H_
#define _PF_Q_GC_H_

#include <linux/string.h>
#include <linux/skbuff.h>

#include <pf_q-skbuff.h>
#include <pf_q-macro.h>
#include <pf_q-skbuff.h>
#include <pf_q-skbuff-batch.h>

typedef struct gc_buff SkBuff;


#define for_each_gcbuff(batch, buff, n) \
        for(n = 0; (n != (batch)->len) && (buff = (batch)->queue[n]).skb; \
                __builtin_prefetch(((batch)->queue[n+1]).skb, 0, 1), n++)


#define for_each_gcbuff_bitmask(batch, mask, buff, n) \
        for(n = pfq_ctz(mask); mask && ((buff = (batch)->queue[n]), true); \
                mask ^=(1UL << n), n = pfq_ctz(mask))


#define SKBUFF_BATCH_ADDR(batch) \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(batch),struct pfq_skbuff_short_batch), (struct pfq_skbuff_batch *)&batch, \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(batch),struct pfq_skbuff_long_batch),  (struct pfq_skbuff_batch *)&batch, \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(batch),struct gc_queue_buff),  	 (struct pfq_skbuff_batch *)&batch, (void) 0)))


struct gc_log
{
	struct net_device * dev[Q_GC_LOG_QUEUE_LEN];
	size_t num_fwd;
	size_t to_kernel;
};


struct gc_queue_buff
{
        size_t len;
        struct gc_buff queue[Q_SKBUFF_LONG_BATCH];
};


struct gc_data
{
	struct gc_log   	log[Q_GC_POOL_QUEUE_LEN];
	struct gc_queue_buff 	pool;
};


extern void   gc_reset(struct gc_data *gc);
extern struct gc_buff gc_make_buff(struct gc_data *gc, struct sk_buff *skb);
extern struct gc_buff gc_alloc_buff(struct gc_data *gc, size_t size);
extern struct gc_buff gc_copy_buff(struct gc_data *gc, struct gc_buff orig);

extern struct gc_buff pfq_make_buff(struct sk_buff *skb);
extern struct gc_buff pfq_alloc_buff(size_t size);
extern struct gc_buff pfq_copy_buff(struct gc_buff buff);


static inline
void gc_data_init(struct gc_data *gc)
{
	memset(gc, 0, sizeof(struct gc_data));
}


static inline
void gc_log_init(struct gc_log *log)
{
	log->num_fwd   = 0;
	log->to_kernel = 0;
}


static inline
size_t gc_size(struct gc_data *gc)
{
	return gc->pool.len;
}


#endif /* _PF_Q_GC_H_ */
