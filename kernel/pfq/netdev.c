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

#include <pfq/netdev.h>


struct net_dev_queue net_dev_queue_null = { PFQ_DEVQ_NULL, NULL, NULL, 0 };


int pfq_dev_refcnt_read_by_index(struct net *net, int ifindex)
{
	struct net_device *dev;
	int ref = -1;
	rcu_read_lock();
	dev = dev_get_by_index_rcu(net, ifindex);
	if (dev) {
		ref = netdev_refcnt_read(dev);
	}
	rcu_read_unlock();
	return ref;
}


int pfq_dev_queue_get(struct net *net, dev_queue_t id, struct net_dev_queue *dq)
{
	int ifindex = PFQ_DEVQ_IFINDEX(id);

	if ( ifindex != 0 &&
	     ifindex != PFQ_DEVQ_IFINDEX(dq->id))
	{
		struct net_device *dev = dev_get_by_index(net, ifindex);
		if (dev == NULL) {
			*dq = net_dev_queue_null;
			return -EFAULT;
		}
#ifdef PFQ_DEBUG
		printk(KERN_INFO "[PFQ] dev_queue_get: ifindex=%d, ref=%d\n", ifindex, pfq_dev_refcnt_read_by_index(net, ifindex));
#endif
		dq->id = id;
		dq->dev = dev;
		dq->queue_mapping = __pfq_dev_cap_txqueue(dev, PFQ_DEVQ_QUEUE(id));
		dq->queue = netdev_get_tx_queue(dev, dq->queue_mapping);
	}
	return 0;
}

