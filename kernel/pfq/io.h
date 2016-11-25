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

#ifndef PFQ_IO_H
#define PFQ_IO_H

#include <core/GC.h>
#include <core/sock.h>

#include <pfq/types.h>


struct napi_struct;
struct sk_buff;

struct napi_struct;


typedef union
{
	uint64_t value;
	struct {
		uint32_t ok;
		uint32_t fail;
	};

} tx_response_t;


extern size_t pfq_sk_queue_recv(struct core_sock_opt *opt,
		                struct core_qbuff_refs *buffs,
		                unsigned long long buffs_mask,
		                int burst_len,
		                pfq_gid_t gid);

struct pfq_mbuff_xmit_context
{
	pfq_skb_pool_t		       *skb_pool;
	struct net		       *net;
	ktime_t			        now;
	unsigned long			jiffies;
	int				node;
	int				copies;
	atomic_t const			*stop;
	bool				*intr;
	bool				xmit_more;
};


/* socket queues */

extern tx_response_t
pfq_sk_queue_xmit(struct core_sock *so, int qindex, int cpu, int node, atomic_t const *stop);


/* skb queues */

extern int pfq_xmit(struct qbuff *buff, struct net_device *dev, int queue, int more);

extern tx_response_t
pfq_qbuff_queue_xmit(struct core_qbuff_queue *buff, unsigned long long buffs_mask, struct net_device *dev, int queue_index);


/* skb lazy xmit */

extern int pfq_lazy_xmit(struct qbuff * buff, struct net_device *dev, int queue_index);
extern int pfq_qbuff_queue_lazy_xmit_run(struct core_qbuff_queue *queue, struct core_endpoint_info const *info);


#define pfq_qbuff_queue_lazy_xmit(buffs, mask, dev, queue_index) ({ \
		int check = STATIC_TYPE(unsigned long long, mask) && \
			    STATIC_TYPE(struct net_device *, dev) && \
			    STATIC_TYPE(int, queue_index); \
		struct qbuff * buff; \
		unsigned int i; int n = 0; \
		(void)check; \
                \
		for_each_qbuff_with_mask(mask, buffs, buff, i) \
		{   \
			if (pfq_lazy_xmit(buff, dev, queue_index)) \
				++n; \
		} \
		n; \
	})

/* receive */

extern int pfq_receive(struct napi_struct *napi, struct sk_buff * skb, int direct);


#endif /* PFQ_IO_H */


