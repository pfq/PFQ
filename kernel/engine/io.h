/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
 *             Loris Gazzarrini <loris.gazzarrini@iet.unipi.it>
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

#ifndef Q_ENGINE_IO_H
#define Q_ENGINE_IO_H

#include <engine/lang/module.h>
#include <engine/lang/engine.h>
#include <engine/lang/symtable.h>
#include <engine/lang/GC.h>

#include <engine/percpu.h>
#include <engine/sock.h>

#include <pf_q-skbuff.h>
#include <pf_q-netdev.h>

struct napi_struct;


int
pfq_receive(struct napi_struct *napi, struct sk_buff * skb, int direct);


typedef union
{
	uint64_t value;
	struct {
		uint32_t ok;
		uint32_t fail;
	};

} tx_ret;



extern size_t pfq_sk_rx_queue_recv(struct pfq_sock_opt *opt,
		                   struct pfq_skbuff_GC_queue *skbs,
		                   unsigned long long skbs_mask,
		                   int burst_len,
		                   pfq_gid_t gid);

struct pfq_mbuff_xmit_context
{
	struct pfq_skb_pool	       *skb_pool;
	struct net		       *net;
	ktime_t			        now;
	unsigned long			jiffies;
	int				node;
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


#endif /* Q_ENGINE_IO_H */
