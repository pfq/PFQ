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

#ifndef Q_ENGINE_SOCK_H
#define Q_ENGINE_SOCK_H

#ifdef __KERNEL__
#include <net/sock.h>
#endif

#include <engine/endpoint.h>
#include <engine/stats.h>
#include <engine/define.h>

#include <pfq/kcompat.h>
#include <pfq/shmem.h>
#include <pfq/types.h>
#include <pfq/atomic.h>


#define for_each_sk_mbuff(hdr, end, fix) \
        for(; (hdr < (struct pfq_pkthdr *)end); \
               hdr = Q_SHARED_QUEUE_NEXT_PKTHDR(hdr, fix))


extern atomic_long_t pfq_sock_vector[Q_MAX_ID];


struct pfq_tx_info
{
	atomic_long_t		addr;			/* (pfq_tx_queue *) */
	void			*base_addr;
	int			def_ifindex;		/* default ifindex */
	int			def_queue;		/* default queue */
};


static inline
void pfq_tx_info_init(struct pfq_tx_info *info)
{
	atomic_long_set(&info->addr, 0);
	info->base_addr = NULL;
	info->def_ifindex = -1;
	info->def_queue = -1;
}


struct pfq_rx_info
{
	atomic_long_t		addr;		/* (pfq_rx_queue *) */
	void			*base_addr;
};

static inline
void pfq_rx_info_init(struct pfq_rx_info *info)
{
        atomic_long_set(&info->addr, 0);
        info->base_addr = NULL;
}


struct pfq_sock_opt
{
	int			tstamp;
	size_t			caplen;

	size_t			rx_queue_len;
	size_t			rx_slot_size;

	size_t			tx_queue_len;
	size_t			tx_slot_size;

#ifdef __KERNEL__
	wait_queue_head_t	waitqueue;
#endif

        size_t			tx_num_async_queues;

	struct pfq_tx_info	txq_async[Q_MAX_TX_QUEUES];
	struct pfq_tx_info	txq;
	struct pfq_rx_info	rxq;

} ____cacheline_aligned_in_smp;



struct pfq_sock
{
        struct sock		sk;
        pfq_id_t		id;

	int			egress_type;
        int			egress_index;
        int			egress_queue;

	int			weight;

	struct pfq_shmem_descr  shmem;
        struct pfq_sock_opt	opt;

        pfq_sock_stats_t __percpu *stats;

} ____cacheline_aligned_in_smp;


/* queue info */

static inline
struct pfq_rx_info *
pfq_get_rx_queue_info(struct pfq_sock_opt *that)
{
	return &that->rxq;
}

static inline
struct pfq_tx_info *
pfq_get_tx_queue_info(struct pfq_sock_opt *that, int index)
{
	if (index == -1)
	    return &that->txq;
	return &that->txq_async[index];
}

/* queues */

static inline
struct pfq_rx_queue *
pfq_get_rx_queue(struct pfq_sock_opt *that)
{
	return (struct pfq_rx_queue *)atomic_long_read(&that->rxq.addr);
}

static inline
struct pfq_tx_queue *
pfq_get_tx_queue(struct pfq_sock_opt *that, int index)
{
	if (index == -1)
		return (struct pfq_tx_queue *)atomic_long_read(&that->txq.addr);
	return (struct pfq_tx_queue *)atomic_long_read(&that->txq_async[index].addr);
}

/* memory mapped queues */

static inline
struct pfq_shared_queue *
pfq_get_shared_queue(struct pfq_sock *p)
{
        return (struct pfq_shared_queue *) p->shmem.addr;
}


static inline struct pfq_sock *
pfq_sk(struct sock *sk)
{
        return (struct pfq_sock *)(sk);
}

pfq_id_t
pfq_get_free_id(struct pfq_sock * so);

void	pfq_sock_opt_init(struct pfq_sock_opt *that, size_t caplen, size_t maxlen);

int	pfq_sock_init(struct pfq_sock *so, pfq_id_t id);
void	pfq_sock_destruct(struct sock *sk);

int     pfq_get_sock_count(void);
struct	pfq_sock * pfq_get_sock_by_id(pfq_id_t id);
void	pfq_release_sock_id(pfq_id_t id);

int	pfq_sock_tx_bind(struct pfq_sock *so, int tid, int if_index, int queue);
int	pfq_sock_tx_unbind(struct pfq_sock *so);

#endif /* Q_ENGINE_SOCK_H */
