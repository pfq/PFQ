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

#include <pf_q-netdev.h>

struct net_dev_queue net_dev_queue_null = { PFQ_DEVQ_NULL, NULL, NULL, 0 };


int dev_queue_get(struct net *net, dev_queue_t id, struct net_dev_queue *dq)
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

		dq->id = id;
		dq->dev = dev;
		dq->queue_mapping = __pfq_dev_cap_txqueue(dev, PFQ_DEVQ_QUEUE(id));
		dq->queue = netdev_get_tx_queue(dev, dq->queue_mapping);
	}
	return 0;
}

