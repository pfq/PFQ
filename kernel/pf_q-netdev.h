/*
 * Copyright (c) 2014 Bonelli Nicola <nicola@pfq.io>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met: 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer. 2.
 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#ifndef PF_Q_NETDEV_H
#define PF_Q_NETDEV_H


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
	u16		     queue_mapping;

};

extern struct net_dev_queue net_dev_queue_null;


#define PFQ_DEVQ_ID(ifindex, queue)	((uint64_t)ifindex << 32 | (uint32_t)queue)
#define PFQ_DEVQ_IFINDEX(id)		((int)(id >> 32))
#define PFQ_DEVQ_QUEUE(id)		((int)(id & 0xffffffff))

#define PFQ_DEVQ_NULL			PFQ_DEVQ_ID(-1,-1)
#define PFQ_DEVQ_DEFAULT		PFQ_DEVQ_ID(0,0)

#define PFQ_DEVQ_FMT			"(%d:%d)"
#define PFQ_DEVQ_ARG(id)		PFQ_DEVQ_IFINDEX(id), PFQ_DEVQ_QUEUE(id)


extern int netdev_refcnt_read_by_index(struct net *net, int ifindex);
extern int dev_queue_get(struct net *net, dev_queue_t id, struct net_dev_queue *dq);

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
void dev_queue_put(struct net *net, struct net_dev_queue *dq)
{
	if(likely(dq->dev)) {
#ifdef PFQ_DEBUG
		int ifindex = PFQ_DEVQ_IFINDEX(dq->id);
		printk(KERN_INFO "[PFQ] dev_queue_put: ifindex=%d, ref=%d\n", ifindex, netdev_refcnt_read_by_index(net, ifindex));
#else
		(void)net;
#endif
		dev_put(dq->dev);
		*dq = net_dev_queue_null;
	}
}


static inline
int dev_put_by_index(struct net *net, int ifindex)
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


#endif /* PF_Q_NETDEV_H */
