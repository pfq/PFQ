/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
 * 	           Loris Gazzarrini <loris.gazzarrini@iet.unipi.it>
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

#include <pfq/global.h>
#include <pfq/memory.h>

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/skbuff.h>


/* exported symbols */

struct sk_buff *
__pfq_alloc_skb(unsigned int size, gfp_t priority, int fclone, int node)
{
#ifdef PFQ_USE_SKB_POOL
        if (atomic_read(&global->pool_enabled)) {
		struct pfq_percpu_pool *cpu_pool = this_cpu_ptr(global->percpu_pool);
		struct pfq_spsc_fifo *fifo = cpu_pool->rx.fifo;
                return ____pfq_alloc_skb_pool(size, priority, fclone, node, 0, fifo);
	}
#endif
        return __alloc_skb(size, priority, fclone, node);
}


struct sk_buff *
pfq_dev_alloc_skb(unsigned int length)
{
        struct sk_buff *skb = pfq_alloc_skb(length + NET_SKB_PAD, GFP_ATOMIC);
        if (likely(skb))
                skb_reserve(skb, NET_SKB_PAD);
        return skb;
}


struct sk_buff *
__pfq_netdev_alloc_skb(struct net_device *dev, unsigned int length, gfp_t gfp)
{
        struct sk_buff *skb = __pfq_alloc_skb(length + NET_SKB_PAD, gfp, 0, NUMA_NO_NODE);
        if (likely(skb)) {
                skb_reserve(skb, NET_SKB_PAD);
                skb->dev = dev;
        }
        return skb;
}


EXPORT_SYMBOL_GPL(__pfq_alloc_skb);
EXPORT_SYMBOL_GPL(__pfq_netdev_alloc_skb);
EXPORT_SYMBOL_GPL(pfq_dev_alloc_skb);

