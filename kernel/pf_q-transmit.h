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

#ifndef PF_Q_TRANSMIT_H
#define PF_Q_TRANSMIT_H

#include <pragma/diagnostic_push>
#include <linux/skbuff.h>
#include <linux/netdevice.h>
#include <pragma/diagnostic_pop>

#include <pf_q-skbuff.h>
#include <pf_q-sock.h>
#include <pf_q-netdev.h>

#include <lang/GC.h>
#include <lang/module.h>


typedef union
{
	uint64_t value;
	struct {
		uint32_t ok;
		uint32_t fail;
	};

} tx_ret;

struct pfq_mbuff_xmit_context
{
	struct pfq_skb_pool	       *skb_pool;
	struct net		       *net;
	ktime_t			        now;
	unsigned long			jiffies;
};


/* socket queues */

extern tx_ret pfq_sk_queue_xmit(struct pfq_sock *so, int qindex, int cpu, int node, atomic_t const *stop);

/* skb queues */

extern int pfq_xmit(struct sk_buff *skb, struct net_device *dev, int queue, int more);
extern tx_ret pfq_skb_queue_xmit(struct pfq_skbuff_queue *skbs, unsigned long long skbs_mask, struct net_device *dev, int queue_index);

/* skb lazy xmit */

extern int pfq_lazy_xmit(struct sk_buff __GC * skb, struct net_device *dev, int queue_index);
extern int pfq_skb_queue_lazy_xmit(struct pfq_skbuff_GC_queue *queue, unsigned long long mask, struct net_device *dev, int queue_index);
extern int pfq_skb_queue_lazy_xmit_run(struct pfq_skbuff_GC_queue *queue, struct pfq_endpoint_info const *info);

#endif /* PF_Q_TRANSMIT_H */
