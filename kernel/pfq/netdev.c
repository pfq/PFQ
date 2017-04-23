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


int
pfq_dev_refcnt_read_by_index(struct net *net, int ifindex)
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



int pfq_dev_queue_get(struct net *net, int ifindex, int queue, struct pfq_dev_queue *dq)
{
	struct net_device *dev = dev_get_by_index(net, ifindex);
	if (dev == NULL) {
		*dq = (struct pfq_dev_queue){.dev = NULL, .queue = NULL, .mapping = 0};
		return -EFAULT;
	}

	dq->dev = dev;
	dq->mapping = __pfq_dev_cap_txqueue(dev, queue);
	dq->queue = netdev_get_tx_queue(dev, dq->mapping);
	return 0;
}


void pfq_dev_queue_put(struct pfq_dev_queue *dq)
{
	if(likely(dq->dev)) {
		dev_put(dq->dev);
		dq->dev = NULL;
	}
}
