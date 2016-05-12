/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#include <engine/io.h>
#include <engine/endpoint.h>
#include <engine/queue.h>
#include <engine/bitops.h>

#include <pfq/sparse.h>


void
add_dev_to_endpoints(struct net_device *dev, struct pfq_endpoint_info *ts)
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


static
size_t copy_to_user_skbs(struct pfq_sock *so, struct pfq_skbuff_GC_queue *skbs,
			 unsigned long long mask, int cpu, pfq_gid_t gid)
{
        unsigned int len = pfq_popcount(mask);
        size_t cpy = 0;

        if (likely(pfq_get_rx_queue(&so->opt))) {

		smp_rmb();

                cpy = pfq_sk_rx_queue_recv(&so->opt, skbs, mask, len, gid);

		__sparse_add(so->stats, recv, cpy, cpu);

		if (len > cpy)
			__sparse_add(so->stats, drop, len - cpy, cpu);

		return cpy;
        }
	else
		__sparse_add(so->stats, lost, len, cpu);

        return cpy;
}


static
size_t copy_to_dev_skbs(struct pfq_sock *so, struct pfq_skbuff_GC_queue *skbs,
			 unsigned long long mask, int cpu, pfq_gid_t gid)
{
	struct net_device *dev;
	int sent;

	if (so->egress_index) {

		dev = dev_get_by_index(&init_net, so->egress_index);
		if (dev == NULL) {
			if (printk_ratelimit())
				printk(KERN_INFO "[PFQ] egress endpoint not existing (%d)\n",
				       so->egress_index);
                        return false;
		}

		sent = pfq_skb_queue_lazy_xmit(skbs, mask, dev, so->egress_queue);
                dev_put(dev);
		return sent;
	}

	return 0;
}


size_t copy_to_endpoint_skbs(struct pfq_sock *so, struct pfq_skbuff_GC_queue *pool,
			      unsigned long long mask, int cpu, pfq_gid_t gid)
{
	switch(so->egress_type)
	{
	case pfq_endpoint_socket:
		return copy_to_user_skbs(so, pool, mask, cpu, gid);

	case pfq_endpoint_device:
		return copy_to_dev_skbs(so, pool, mask, cpu, gid);
	}

	return false;
}

