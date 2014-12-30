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


#include <pf_q-percpu.h>
#include <pf_q-global.h>
#include <pf_q-GC.h>

void gc_reset(struct gc_data *gc)
{
	size_t n;
	for(n = 0; n < gc->pool.len; ++n)
	{
		gc_log_init(&gc->log[n]);
	}
	gc->pool.len = 0;
}


struct gc_buff
gc_make_buff(struct gc_data *gc, struct sk_buff *skb)
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


struct gc_buff
gc_alloc_buff(struct gc_data *gc, size_t size)
{
	struct sk_buff *skb;
	struct gc_buff ret;

	if (gc->pool.len >= Q_GC_POOL_QUEUE_LEN) {
		printk(KERN_INFO "[PFQ] GC: pool exhausted!\n");
		ret.skb = NULL;
		return ret;
	}

	skb = alloc_skb(size, GFP_ATOMIC);
	if (skb == NULL) {
		printk(KERN_INFO "[PFQ] GC: out of memory!\n");
		ret.skb = NULL;
		return ret;
	}

	/* gc_make_buff can't fail now */

	return gc_make_buff(gc, skb);
}


struct gc_buff
gc_copy_buff(struct gc_data *gc, struct gc_buff orig)
{
	struct sk_buff *skb;
	struct gc_buff ret;

	if (gc->pool.len >= Q_GC_POOL_QUEUE_LEN) {
		printk(KERN_INFO "[PFQ] GC: pool exhausted!\n");
		ret.skb = NULL;
		return ret;
	}

	skb = skb_copy(orig.skb, GFP_ATOMIC);
	if (skb == NULL) {
		printk(KERN_INFO "[PFQ] GC: out of memory!\n");
		ret.skb = NULL;
		return ret;
	}

	skb->mac_len = orig.skb->mac_len;

	/* gc_make_buff can't fail now */

	ret = gc_make_buff(gc, skb);

	PFQ_CB(ret.skb)->group_mask = PFQ_CB(orig.skb)->group_mask;
	PFQ_CB(ret.skb)->direct     = PFQ_CB(orig.skb)->direct;
	PFQ_CB(ret.skb)->monad      = PFQ_CB(orig.skb)->monad;

	return ret;
}


inline void
__gc_add_dev_to_targets(struct net_device *dev, struct gc_fwd_targets *ts)
{
	size_t n = 0;

	for(; n < ts->num; ++n)
	{
        	if (dev == ts->dev[n]) {
        		ts->cnt[n]++;
        		ts->cnt_total++;
        		return;
		}
	}

	if (n < Q_GC_LOG_QUEUE_LEN) {
		ts->dev[n] = dev;
		ts->cnt[n] = 1;
		ts->cnt_total++;
		ts->num++;
	}
	else {
		printk(KERN_INFO "[PFQ] GC: forward pool exhausted!\n");
	}
}


void
gc_get_fwd_targets(struct gc_data *gc, struct gc_fwd_targets *ts)
{
	size_t n, i;

	ts->num = 0;
        ts->cnt_total = 0;

	for(n = 0; n < gc->pool.len; ++n)
	{
		for(i = 0; i < gc->log[n].num_fwd; i++)
		{
         		__gc_add_dev_to_targets(gc->log[n].dev[i], ts);
		}
	}
}


struct gc_buff pfq_make_buff(struct sk_buff *skb)
{
	struct local_data *local = this_cpu_ptr(cpu_data);
	return gc_make_buff(&local->gc, skb);
}

struct gc_buff pfq_alloc_buff(size_t size)
{
	struct local_data *local = this_cpu_ptr(cpu_data);
	return gc_alloc_buff(&local->gc, size);
}

struct gc_buff pfq_copy_buff(struct gc_buff buff)
{
	struct local_data *local = this_cpu_ptr(cpu_data);
	return gc_copy_buff(&local->gc, buff);
}

