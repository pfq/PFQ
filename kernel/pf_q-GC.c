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
#include <pf_q-transmit.h>

void GC_reset(struct GC_data *gc)
{
	size_t n;
	for(n = 0; n < gc->pool.len; ++n)
	{
		GC_log_init(&gc->log[n]);
	}
	gc->pool.len = 0;
}


struct sk_buff __GC *
GC_make_buff(struct GC_data *gc, struct sk_buff *skb)
{
	struct sk_buff __GC * ret;

	if (gc->pool.len >= Q_GC_POOL_QUEUE_LEN) {
		ret = NULL;
	}
	else {
                PFQ_CB(skb)->log = &gc->log[gc->pool.len];
		ret = gc->pool.queue[gc->pool.len++] = (struct sk_buff __force __GC *) skb;
	}

	return ret;
}


struct sk_buff __GC *
GC_alloc_buff(struct GC_data *gc, size_t size)
{
	struct sk_buff *skb;
	struct sk_buff __GC * ret;

	if (gc->pool.len >= Q_GC_POOL_QUEUE_LEN) {
		pr_devel("[PFQ] GC: pool exhausted!\n");
		ret = NULL;
		return ret;
	}

	skb = alloc_skb(size, GFP_ATOMIC);
	if (skb == NULL) {
		pr_devel("[PFQ] GC: out of memory!\n");
		ret = NULL;
		return ret;
	}

	/* GC_make_buff can't fail now */

	return GC_make_buff(gc, skb);
}


struct sk_buff __GC *
GC_copy_buff(struct GC_data *gc, struct sk_buff __GC * orig)
{
	struct sk_buff *skb;
	struct sk_buff __GC * ret;

	if (gc->pool.len >= Q_GC_POOL_QUEUE_LEN) {
		pr_devel("[PFQ] GC: pool exhausted!\n");
		ret = NULL;
		return ret;
	}

	skb = skb_copy(PFQ_SKB(orig), GFP_ATOMIC);
	if (skb == NULL) {
		pr_devel("[PFQ] GC: out of memory!\n");
		ret = NULL;
		return ret;
	}

	skb->mac_len = orig->mac_len;

	/* GC_make_buff can't fail now */

	ret = GC_make_buff(gc, skb);

	PFQ_CB(ret)->group_mask = PFQ_CB(orig)->group_mask;
	PFQ_CB(ret)->direct     = PFQ_CB(orig)->direct;
	PFQ_CB(ret)->monad      = PFQ_CB(orig)->monad;

	return ret;
}


static inline void
add_dev_to_targets(struct net_device *dev, struct skb_lazy_targets *ts)
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
	else
		pr_devel("[PFQ] GC: forward pool exhausted!\n");
}


void
GC_get_lazy_targets(struct GC_data *gc, struct skb_lazy_targets *ts)
{
	size_t n, i;

	ts->num = 0;
        ts->cnt_total = 0;

	for(n = 0; n < gc->pool.len; ++n)
	{
		for(i = 0; i < gc->log[n].num_devs; i++)
		{
			add_dev_to_targets(gc->log[n].dev[i], ts);
		}
	}
}


struct sk_buff __GC *
pfq_make_buff(struct sk_buff *skb)
{
	struct pfq_percpu_data *data = this_cpu_ptr(percpu_data);
	return GC_make_buff(data->GC, skb);
}

struct sk_buff __GC *
pfq_alloc_buff(size_t size)
{
	struct pfq_percpu_data *data = this_cpu_ptr(percpu_data);
	return GC_alloc_buff(data->GC, size);
}

struct sk_buff __GC *
pfq_copy_buff(struct sk_buff __GC * skb)
{
	struct pfq_percpu_data *data = this_cpu_ptr(percpu_data);
	return GC_copy_buff(data->GC, skb);
}

