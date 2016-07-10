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

#ifndef Q_CORE_SOCK_H
#define Q_CORE_SOCK_H

#include <core/endpoint.h>
#include <core/define.h>
#include <core/stats.h>

#include <pfq/kcompat.h>
#include <pfq/thread.h>
#include <pfq/shmem.h>
#include <pfq/types.h>
#include <pfq/atomic.h>
#include <pfq/sock.h>

#ifdef __KERNEL__
#include <net/sock.h>
#endif


#define for_each_sk_mbuff(hdr, end, fix) \
        for(; (hdr < (struct pfq_pkthdr *)end); \
               hdr = PFQ_SHARED_QUEUE_NEXT_PKTHDR(hdr, fix))


struct core_tx_info
{
	atomic_long_t		addr;			/* (pfq_tx_queue *) */
	void			*base_addr;
	int			def_ifindex;		/* default ifindex */
	int			def_queue;		/* default queue */
};


static inline
void core_tx_info_init(struct core_tx_info *info)
{
	atomic_long_set(&info->addr, 0);
	info->base_addr = NULL;
	info->def_ifindex = -1;
	info->def_queue = -1;
}


struct core_rx_info
{
	atomic_long_t		addr;		/* (pfq_rx_queue *) */
	void			*base_addr;
};


static inline
void core_rx_info_init(struct core_rx_info *info)
{
        atomic_long_set(&info->addr, 0);
        info->base_addr = NULL;
}


struct core_sock_opt
{
	int			tstamp;
	size_t			caplen;

	size_t			rx_queue_len;
	size_t			rx_slot_size;

	size_t			tx_queue_len;
	size_t			tx_slot_size;

	wait_queue_head_t	waitqueue;

        size_t			tx_num_async_queues;

	struct core_tx_info	txq_async[Q_MAX_TX_QUEUES];
	struct core_tx_info	txq;
	struct core_rx_info	rxq;

} ____cacheline_aligned_in_smp;



struct core_sock
{
        struct sock		sk;
        pfq_id_t		id;

	int			egress_type;
        int			egress_index;
        int			egress_queue;
	int			weight;

	struct pfq_shmem_descr  shmem;
        struct core_sock_opt	opt;

        core_sock_stats_t __percpu *stats;

} ____cacheline_aligned_in_smp;


/* queue info */

static inline
struct core_rx_info *
core_sock_get_rx_queue_info(struct core_sock_opt *that)
{
	return &that->rxq;
}

static inline
struct core_tx_info *
core_sock_get_tx_queue_info(struct core_sock_opt *that, int index)
{
	if (index == -1)
	    return &that->txq;
	return &that->txq_async[index];
}

/* queues */

static inline
struct pfq_rx_queue *
core_sock_get_rx_queue(struct core_sock_opt *that)
{
	return (struct pfq_rx_queue *)atomic_long_read(&that->rxq.addr);
}

static inline
struct pfq_tx_queue *
core_sock_get_tx_queue(struct core_sock_opt *that, int index)
{
	if (index == -1)
		return (struct pfq_tx_queue *)atomic_long_read(&that->txq.addr);
	return (struct pfq_tx_queue *)atomic_long_read(&that->txq_async[index].addr);
}

/* memory mapped queues */

static inline
struct pfq_shared_queue *
core_sock_get_shared_queue(struct core_sock *p)
{
        return (struct pfq_shared_queue *) p->shmem.addr;
}


static inline struct core_sock *
pfq_sk(struct sock *sk)
{
        return (struct core_sock *)(sk);
}


extern pfq_id_t core_sock_get_free_id(struct core_sock * so);
extern void	core_sock_opt_init(struct core_sock_opt *that, size_t caplen, size_t maxlen);
extern int	core_sock_init(struct core_sock *so, pfq_id_t id);
extern struct	core_sock * core_sock_get_by_id(pfq_id_t id);
extern int	core_sock_get_socket_count(void);
extern void	core_sock_release_id(pfq_id_t id);
extern int	core_sock_tx_bind(struct core_sock *so, int tid, int if_index, int queue);
extern int	core_sock_tx_unbind(struct core_sock *so);

#endif /* Q_CORE_SOCK_H */
