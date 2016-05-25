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

#ifndef PFQ_NETDEV_H
#define PFQ_NETDEV_H


#include <pragma/diagnostic_push>
#include <linux/netdevice.h>
#include <pragma/diagnostic_pop>


typedef uint64_t dev_queue_t;


struct net_dev_queue
{
	dev_queue_t	     id;

	/* ---- the following parameters are set by dev_queue_get etc. */

	struct net_device   *dev;
	struct netdev_queue *queue;
	uint16_t		     queue_mapping;

};

extern struct net_dev_queue net_dev_queue_null;


#define PFQ_DEVQ_ID(ifindex, queue)	((uint64_t)ifindex << 32 | (uint16_t)queue)
#define PFQ_DEVQ_IFINDEX(id)		((int)(id >> 32))
#define PFQ_DEVQ_QUEUE(id)		((int)(id & 0xffffffff))

#define PFQ_DEVQ_NULL			PFQ_DEVQ_ID(-1,0)
#define PFQ_DEVQ_DEFAULT		PFQ_DEVQ_ID(0,0)

#define PFQ_DEVQ_IS_NULL(id)		(PFQ_DEVQ_IFINDEX(id) == -1)

#define PFQ_DEVQ_FMT			"(%d:%d)"
#define PFQ_DEVQ_ARG(id)		PFQ_DEVQ_IFINDEX(id), PFQ_DEVQ_QUEUE(id)


extern int pfq_dev_refcnt_read_by_index(struct net *net, int ifindex);
extern int pfq_dev_queue_get(struct net *net, dev_queue_t id, struct net_dev_queue *dq);

static inline int
__pfq_dev_cap_txqueue(struct net_device *dev, int queue)
{
	if (unlikely(queue >= dev->real_num_tx_queues))
		return queue % dev->real_num_tx_queues;
	return queue;
}

static inline
void pfq_hard_tx_lock(struct net_dev_queue *dq)
{
	if(likely(dq->dev && dq->queue))
		HARD_TX_LOCK(dq->dev, dq->queue, smp_processor_id());
}


static inline
void pfq_hard_tx_unlock(struct net_dev_queue *dq)
{
	if(likely(dq->dev && dq->queue))
		HARD_TX_UNLOCK(dq->dev, dq->queue);
}


static inline
void pfq_dev_queue_put(struct net *net, struct net_dev_queue *dq)
{
	if(likely(dq->dev)) {
#ifdef PFQ_DEBUG
		int ifindex = PFQ_DEVQ_IFINDEX(dq->id);
		printk(KERN_INFO "[PFQ] dev_queue_put: ifindex=%d, ref=%d\n", ifindex, pfq_dev_refcnt_read_by_index(net, ifindex));
#else
		(void)net;
#endif
		dev_put(dq->dev);
		*dq = net_dev_queue_null;
	}
}


static inline
int pfq_dev_put_by_index(struct net *net, int ifindex)
{
	struct net_device *dev;
	int err = -EPERM;
	rcu_read_lock();
	dev = dev_get_by_index_rcu(net, ifindex);
	if (dev) {
		dev_put(dev);
		err = 0;
	}
	rcu_read_unlock();
	return err;
}


static inline
bool pfq_dev_check_by_index(int index)
{
	struct net_device *dev;
	rcu_read_lock();
	dev = dev_get_by_index_rcu(&init_net, index);
	rcu_read_unlock();
	return dev != NULL;
}


static inline
struct net_device *pfq_dev_get_by_index(int index)
{
	return dev_get_by_index(&init_net, index);
}


static inline
struct net_device *pfq_dev_get_by_name(const char *name)
{
	return dev_get_by_name(&init_net, name);
}


static inline
void pfq_dev_put(struct net_device *dev)
{
	dev_put(dev);
}


static inline
const char *
pfq_dev_name(struct net_device *dev)
{
	return dev->name;
}


#endif /* PFQ_NETDEV_H */
