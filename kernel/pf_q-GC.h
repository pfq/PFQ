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

#ifndef PF_Q_GC_H
#define PF_Q_GC_H

#include <pragma/diagnostic_push>
#include <linux/string.h>
#include <linux/skbuff.h>
#include <pragma/diagnostic_pop>

#include <pf_q-skbuff.h>
#include <pf_q-macro.h>
#include <pf_q-skbuff.h>
#include <pf_q-skbuff-batch.h>


typedef skbuff_t SkBuff;


struct GC_log
{
	struct net_device * dev[Q_GC_LOG_QUEUE_LEN];
	size_t num_devs;
	size_t to_kernel;
	size_t xmit_todo;
};


struct GC_queue_buff
{
        size_t len;
        skbuff_t queue[Q_SKBUFF_LONG_BATCH];
};


struct GC_data
{
	struct GC_log			log[Q_GC_POOL_QUEUE_LEN];
	struct pfq_skbuff_long_batch	pool;
};


extern void   GC_reset(struct GC_data *gc);

extern skbuff_t GC_make_buff(struct GC_data *gc, struct sk_buff *skb);
extern skbuff_t GC_alloc_buff(struct GC_data *gc, size_t size);
extern skbuff_t GC_copy_buff(struct GC_data *gc, skbuff_t orig);

skbuff_t pfq_make_buff(struct sk_buff *skb);
skbuff_t pfq_alloc_buff(size_t size);
skbuff_t pfq_copy_buff(skbuff_t skb);


struct skb_lazy_targets;

extern void  GC_get_lazy_targets(struct GC_data *gc, struct skb_lazy_targets *ts);


static inline size_t
GC_count_dev_in_log(struct net_device *dev, struct GC_log *log)
{
	size_t n, ret = 0;
	for(n = 0; n < log->num_devs; n++)
	{
		if (dev == log->dev[n])
			ret++;
	}
	return ret;
}


static inline
void GC_data_init(struct GC_data *gc)
{
	memset(gc, 0, sizeof(struct GC_data));
}


static inline
void GC_log_init(struct GC_log *log)
{
	log->to_kernel = 0;
	log->xmit_todo = 0;
	log->num_devs  = 0;
}


static inline
size_t GC_size(struct GC_data *gc)
{
	return gc->pool.len;
}


#endif /* PF_Q_GC_H */
