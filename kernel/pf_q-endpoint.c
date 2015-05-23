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

#include <linux/kernel.h>
#include <linux/version.h>
#include <linux/module.h>
#include <linux/skbuff.h>

#include <pf_q-shared-queue.h>
#include <pf_q-sparse.h>
#include <pf_q-transmit.h>
#include <pf_q-endpoint.h>


static inline
size_t copy_to_user_skbs(struct pfq_rx_opt *ro, struct pfq_skbuff_batch *skbs,
			 unsigned long long mask, int cpu, pfq_gid_t gid)
{
        int len = pfq_popcount(mask);
        size_t cpy = 0;

        if (likely(pfq_get_rx_queue(ro))) {

		smp_rmb();

                cpy = pfq_mpsc_enqueue_batch(ro, skbs, mask, len, gid);

		__sparse_add(&ro->stats.recv, cpy, cpu);

		if (len > cpy)
			__sparse_add(&ro->stats.drop, len - cpy, cpu);

		return cpy;
        }
	else
		__sparse_add(&ro->stats.lost, len, cpu);

        return cpy;
}


static inline
size_t copy_to_dev_buffs(struct pfq_sock *so, struct gc_queue_buff *buffs,
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

		sent = pfq_batch_lazy_xmit_by_mask(buffs, mask, dev, so->egress_queue);

                dev_put(dev);
		return sent;
	}

	return 0;
}


size_t copy_to_endpoint_buffs(struct pfq_sock *so, struct gc_queue_buff *pool,
			      unsigned long long mask, int cpu, pfq_gid_t gid)
{
	switch(so->egress_type)
	{
	case pfq_endpoint_socket:
		return copy_to_user_skbs(&so->rx_opt, SKBUFF_BATCH_ADDR(*pool), mask, cpu, gid);

	case pfq_endpoint_device:
		return copy_to_dev_buffs(so, pool, mask, cpu, gid);
	}

	return false;
}

