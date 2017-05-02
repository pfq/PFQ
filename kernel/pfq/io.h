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

#include <pfq/sock.h>
#include <pfq/types.h>


#define pfq_qbuff_queue_lazy_xmit(buffs, mask, dev, queue_index) ({ \
		int check = STATIC_TYPE(unsigned __int128, mask) && \
			    STATIC_TYPE(struct net_device *, dev) && \
			    STATIC_TYPE(int, queue_index); \
		struct qbuff * buff; \
		unsigned int i; int n = 0; \
		(void)check; \
                \
		for_each_qbuff_with_mask(mask, buffs, buff, i) \
		{   \
			if (pfq_qbuff_lazy_xmit(buff, dev, queue_index)) \
				++n; \
		} \
		n; \
	})

typedef union
{
	uint64_t value;
	struct {
		uint32_t ok;
		uint32_t fail;
	};

} tx_response_t;


struct sk_buff;
struct napi_struct;
struct napi_struct;


extern size_t pfq_sk_queue_recv( struct pfq_sock *so
			       , struct pfq_qbuff_queue *buffs
			       , unsigned __int128 buffs_mask
			       , int burst_len
			       );


struct pfq_mbuff_xmit_context
{
	struct pfq_skb_pool	*tx;
	struct net		*net;
	ktime_t			now;
	unsigned long		jiffies;
	int			node;
	int			copies;
	bool			*intr;
	bool			xmit_more;
};


static inline size_t
pfq_count_fwd_devs(struct net_device *dev, struct net_device **devs, size_t num_devs)
{
	size_t n, ret = 0;
	for(n = 0; n < num_devs; n++)
	{
		if (dev == devs[n])
			ret++;
	}
	return ret;
}


/* socket queues */

extern tx_response_t
pfq_sk_queue_xmit(struct pfq_sock *so, int qindex, int cpu);


/* skb queues */

extern int pfq_xmit(struct qbuff *buff, struct net_device *dev, int queue, int more);

extern tx_response_t
pfq_qbuff_queue_xmit(struct pfq_qbuff_queue *buff, unsigned __int128 buffs_mask, struct net_device *dev, int queue_index);

/* skb lazy xmit */

extern int pfq_qbuff_lazy_xmit(struct qbuff * buff, struct net_device *dev, int queue_index);
extern int pfq_qbuff_lazy_xmit_run(struct pfq_qbuff_queue *queue, struct pfq_endpoint_info const *info);


/* receive */

extern int pfq_receive(struct napi_struct *napi, struct sk_buff * skb);
extern int pfq_receive_run( struct pfq_percpu_data *data , struct pfq_percpu_pool *pool , int cpu);

#endif /* PFQ_IO_H */


