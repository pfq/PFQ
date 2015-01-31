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

#include <linux/skbuff.h>
#include <linux/netdevice.h>

#include <pf_q-skbuff-batch.h>
#include <pf_q-sock.h>
#include <pf_q-module.h>
#include <pf_q-GC.h>


extern int __pfq_queue_flush(size_t index, struct pfq_tx_opt *to, struct net_device *dev, int cpu, int node);


static inline int
pfq_queue_flush(size_t index, struct pfq_tx_opt *to, struct net_device *dev)
{
	return __pfq_queue_flush(index, to, dev, Q_NO_KTHREAD, NUMA_NO_NODE);
}


extern int pfq_queue_flush_or_wakeup(struct pfq_sock *so, int index);


extern int pfq_queue_xmit(struct pfq_skbuff_batch *skbs, struct net_device *dev, int queue_index);
extern int pfq_queue_xmit_by_mask(struct pfq_skbuff_batch *skbs, unsigned long long skbs_mask, struct net_device *dev, int queue_index);
extern int pfq_xmit(struct sk_buff *skb, struct net_device *dev, int hw_queue, int more);

extern int pfq_lazy_xmit(struct gc_buff, struct net_device *dev, int queue_index);
extern int pfq_queue_lazy_xmit(struct gc_queue_buff *queue, struct net_device *dev, int queue_index);
extern int pfq_queue_lazy_xmit_by_mask(struct gc_queue_buff *queue, unsigned long long mask, struct net_device *dev, int queue_index);

extern size_t pfq_lazy_xmit_exec(struct gc_data *gc, struct gc_fwd_targets const *t);


#endif /* PF_Q_TRANSMIT_H */
