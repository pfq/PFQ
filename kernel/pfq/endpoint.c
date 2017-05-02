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

#include <pfq/bitops.h>
#include <pfq/endpoint.h>
#include <pfq/io.h>
#include <pfq/kcompat.h>
#include <pfq/netdev.h>
#include <pfq/printk.h>
#include <pfq/queue.h>
#include <pfq/sparse.h>
#include <pfq/qbuff.h>


void
pfq_get_lazy_endpoints( struct pfq_qbuff_queue *qb
		      , struct pfq_endpoint_info *ts)
{
	size_t n, i;
	ts->num = 0;
        ts->cnt_total = 0;

	for(n = 0; n < qb->len; ++n)
	{
		for(i = 0; i < qb->queue[n].fwd_dev_num; i++)
		{
			pfq_add_dev_to_endpoints(qb->queue[n].fwd_dev[i], ts);
		}
	}
}


void
pfq_add_dev_to_endpoints( struct net_device *dev
			, struct pfq_endpoint_info *ts)
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

	if (n < Q_BUFF_LOG_LEN) {
		ts->dev[n] = dev;
		ts->cnt[n] = 1;
		ts->cnt_total++;
		ts->num++;
	}
	else
		pr_devel("[PFQ] GC: forward pool exhausted!\n");
}


static inline
size_t copy_to_user_qbuffs( struct pfq_sock *so
			  , struct pfq_qbuff_queue *buffs
			  , unsigned __int128 mask
			  , int cpu)
{
        size_t cpy, len = pfq_popcount(mask);

	__sparse_add(so->stats, recv, len, cpu);

        if (likely(pfq_sock_rx_shared_queue(so) != NULL)) {

		smp_rmb();

                cpy = pfq_sk_queue_recv(so, buffs, mask, (int)len);
		if (len > cpy)
			__sparse_add(so->stats, lost, len - cpy, cpu);

		return cpy;
        }
	else
		__sparse_add(so->stats, lost, len, cpu);

        return 0;
}


static inline
size_t copy_to_dev_qbuffs( struct pfq_sock *so
			 , struct pfq_qbuff_queue *buffs
			 , unsigned __int128 mask
			 , int cpu)
{
	struct net_device *dev;
	size_t sent;

	if (so->egress_index) {

		dev = pfq_dev_get_by_index(so->egress_index);
		if (dev == NULL) {
			if (printk_ratelimit())
				printk(KERN_INFO "[PFQ] egress endpoint not existing (%d)\n",
				       so->egress_index);
                        return false;
		}

		sent = (size_t)pfq_qbuff_queue_lazy_xmit(buffs, mask, dev, so->egress_queue);
                pfq_dev_put(dev);
		return sent;
	}

	return 0;
}


size_t
pfq_copy_to_endpoint_qbuffs( struct pfq_sock *so
			   , struct pfq_qbuff_queue *buffs
			   , unsigned __int128 mask
			   , int cpu)
{
	switch(so->egress_type)
	{
	case Q_ENDPOINT_SOCKET:
		return copy_to_user_qbuffs(so, buffs, mask, cpu);

	case Q_ENDPOINT_DEVICE:
		return copy_to_dev_qbuffs(so, buffs, mask, cpu);
	}

	return false;
}

