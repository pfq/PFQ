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

#include <linux/kernel.h>
#include <linux/version.h>
#include <linux/module.h>
#include <linux/skbuff.h>

#include "pf_q-non-intrusive.h"
#include "pf_q-mpdb-queue.h"
#include "pf_q-sparse.h"
#include "pf_q-transmit.h"


static inline
bool copy_to_user_skbs(struct pfq_rx_opt *ro, struct pfq_non_intrusive_skb *skbs, unsigned long long skbs_mask, int cpu, int gid)
{
        /* enqueue the sk_buffs: it's wait-free. */

        int len = 0; size_t sent = 0;

        if (likely(ro->queue_ptr)) {

        	smp_rmb();

                len  = (int)pfq_popcount(skbs_mask);
                sent = pfq_mpdb_enqueue_batch(ro, skbs, skbs_mask, len, gid);

        	__sparse_add(&ro->stat.recv, sent, cpu);

		if (len > sent) {
			__sparse_add(&ro->stat.lost, len - sent, cpu);
			return false;
		}
        }
        return true;
}


bool copy_to_endpoint_skbs(struct pfq_sock *so, struct pfq_non_intrusive_skb *skbs, unsigned long long skbs_mask, int cpu, int gid)
{
	if (so->egress_index) {

		struct net_device *dev;
		struct sk_buff *skb;
		bool ret;
		int n;

                pfq_non_intrusive_for_each(skb, n, skbs)
		{
 			atomic_inc(&skb->users);
               	}

               	dev = dev_get_by_index(&init_net, so->egress_index);
               	if (dev == NULL)
		{
			if (printk_ratelimit()) {
                        	printk(KERN_INFO "[PFQ] egress endpoint index (%d)\n", so->egress_index);
                        	return false;
			}
		}

 		ret = pfq_queue_xmit_by_mask(skbs, skbs_mask, dev, so->egress_queue);
                dev_put(dev);
		return ret;
	}

	return copy_to_user_skbs(&so->rx_opt, skbs, skbs_mask, cpu, gid);
}


