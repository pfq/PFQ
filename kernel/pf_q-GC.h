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

#ifndef _PF_Q_GC_SKBUFF_H_
#define _PF_Q_GC_SKBUFF_H_

#include <linux/string.h>
#include <linux/skbuff.h>

#include <pf_q-skbuff.h>
#include <pf_q-macro.h>
#include <pf_q-bounded-queue.h>


#define GC_queue_for_each_skb(pool, skb, n) \
        for(n = 0; (n != (pool)->len) && (skb = (pool)->queue[n].skb); \
                __builtin_prefetch((pool)->queue[n+1].skb, 0, 1), n++)

#define GC_queue_for_each_buff(pool, buff, n) \
        for(n = 0; (n != (pool)->len) && (buff = (pool)->queue[n]).skb; n++)

#define GC_queue_for_each_skb_bitmask(pool, skb, mask, n) \
        for(n = pfq_ctz(mask); mask && ((skb = (pool)->queue[n].skb), true); \
                mask ^=(1UL << n), n = pfq_ctz(mask))

#define GC_queue_for_each_buff_bitmask(pool, buff, mask, n) \
        for(n = pfq_ctz(mask); mask && ((buff = (pool)->queue[n]), true); \
                mask ^=(1UL << n), n = pfq_ctz(mask))


struct gc_buff
{
 	struct sk_buff *skb;
};

typedef struct gc_buff SkBuff;


struct gc_log
{
	struct net_device * dev[Q_GC_LOG_QUEUE_LEN];
	size_t num_fwd;
	bool   to_kernel;
};


struct gc_queue_buff
{
        size_t len;
        struct gc_buff queue[Q_GC_POOL_QUEUE_LEN];
};


struct gc_data
{
	struct gc_log   	log[Q_GC_POOL_QUEUE_LEN];
	struct gc_queue_buff 	pool;
};


static inline
void gc_data_init(struct gc_data *gc)
{
	memset(gc, 0, sizeof(struct gc_data));
}


static inline
void gc_log_init(struct gc_log *log)
{
	log->num_fwd = 0;
	log->to_kernel = false;
}


static inline
void gc_reset(struct gc_data *gc)
{
	size_t n;
	for(n = 0; n < gc->pool.len; ++n)
	{
		gc_log_init(&gc->log[n]);
	}
	gc->pool.len = 0;
}


static inline
struct gc_buff
make_buff(struct gc_data *gc, struct sk_buff *skb)
{
	struct gc_buff ret;

	if (gc->pool.len >= Q_GC_POOL_QUEUE_LEN) {
		ret.skb = NULL;
	}
	else {
		struct pfq_cb *cb = (struct pfq_cb *)skb->cb;
                cb->log = &gc->log[gc->pool.len];
		gc->pool.queue[gc->pool.len++].skb = skb;
		ret.skb = skb;
	}

	return ret;
}


static inline
size_t gc_size(struct gc_data *gc)
{
	return gc->pool.len;
}

static inline
struct gc_buff
gc_alloc_buff(struct gc_data *gc, size_t size)
{
	struct sk_buff *skb;
	struct gc_buff ret;

	if (gc->pool.len >= Q_GC_POOL_QUEUE_LEN) {
		ret.skb = NULL;
		return ret;
	}

	skb = alloc_skb(size, GFP_ATOMIC);
	if (skb == NULL) {
		ret.skb = NULL;
		return ret;
	}

	return make_buff(gc, skb);
}


static inline
struct gc_buff
gc_copy(struct gc_data *gc, struct gc_buff orig)
{
	struct sk_buff *skb;
	struct gc_buff ret;

	if (gc->pool.len >= Q_GC_POOL_QUEUE_LEN) {
		ret.skb = NULL;
		return ret;
	}

	skb = skb_copy(orig.skb, GFP_ATOMIC);
	if (skb == NULL) {
		ret.skb = NULL;
		return ret;
	}

	ret = make_buff(gc, skb);
	if (ret.skb) {

		PFQ_CB(ret.skb)->group_mask = PFQ_CB(orig.skb)->group_mask;
		PFQ_CB(ret.skb)->direct     = PFQ_CB(orig.skb)->direct;
		PFQ_CB(ret.skb)->monad      = PFQ_CB(orig.skb)->monad;
	}

	return ret;
}


#endif /* _PF_Q_GC_H_ */
