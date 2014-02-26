/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/skbuff.h>

#include <pf_q-non-intrusive.h>
#include <pf_q-memory.h>


sparse_counter_t os_alloc;
sparse_counter_t os_free;
sparse_counter_t rc_alloc;
sparse_counter_t rc_free;
sparse_counter_t rc_error;


struct pfq_recycle_stat
pfq_get_recycle_stats(void)
{
        struct pfq_recycle_stat ret = { sparse_read(&os_alloc),
                                        sparse_read(&os_free),
                                        sparse_read(&rc_alloc),
                                        sparse_read(&rc_free),
                                        sparse_read(&rc_error)};
        return ret;
}


void
pfq_reset_recycle_stats(void)
{
        sparse_set(&os_alloc, 0);
        sparse_set(&os_free, 0);
        sparse_set(&rc_alloc, 0);
        sparse_set(&rc_free, 0);
        sparse_set(&rc_error, 0);
}


/* exported symbols */

struct sk_buff *
__pfq_alloc_skb(unsigned int size, gfp_t priority, int fclone, int node)
{
#ifdef PFQ_USE_SKB_RECYCLE
        struct local_data *this_cpu = __this_cpu_ptr(cpu_data);

        if (atomic_read(&this_cpu->enable_recycle))
        {
                return ____pfq_alloc_skb_recycle(size, priority, fclone, node, &this_cpu->rx_recycle_list);
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

