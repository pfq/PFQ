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


struct core_txq_info
{
	atomic_long_t		addr;			/* (pfq_shared_tx_queue *) */
	void			*shmem_addr;
	int			def_ifindex;		/* default ifindex */
	int			def_queue;		/* default queue */
};


static inline
void core_txq_info_init(struct core_txq_info *info)
{
	atomic_long_set(&info->addr, 0);
	info->shmem_addr = NULL;
	info->def_ifindex = -1;
	info->def_queue = -1;
}


struct core_rxq_info
{
	atomic_long_t		addr;		/* (pfq_shared_rx_queue *) */
	void			*shmem_addr;
};


static inline
void core_rxq_info_init(struct core_rxq_info *info)
{
        atomic_long_set(&info->addr, 0);
        info->shmem_addr = NULL;
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

        size_t			txq_num_async;

	struct core_txq_info	txq_info_async[Q_MAX_TX_QUEUES];
	struct core_txq_info	txq_info;
	struct core_rxq_info	rxq_info;

} ____pfq_cacheline_aligned;



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

} ____pfq_cacheline_aligned;


/* queue info */

static inline
struct core_rxq_info *
core_sock_get_rx_queue_info(struct core_sock_opt *that)
{
	return &that->rxq_info;
}

static inline
struct core_txq_info *
core_sock_get_tx_queue_info(struct core_sock_opt *that, int index)
{
	if (index == -1)
	    return &that->txq_info;
	return &that->txq_info_async[index];
}

/* queues */

static inline
struct pfq_shared_rx_queue *
core_sock_shared_rx_queue(struct core_sock_opt *that)
{
	return (struct pfq_shared_rx_queue *)atomic_long_read(&that->rxq_info.addr);
}

static inline
struct pfq_shared_tx_queue *
core_sock_shared_tx_queue(struct core_sock_opt *that, int index)
{
	if (index == -1)
		return (struct pfq_shared_tx_queue *)atomic_long_read(&that->txq_info.addr);
	return (struct pfq_shared_tx_queue *)atomic_long_read(&that->txq_info_async[index].addr);
}

/* memory mapped queues */

static inline
struct pfq_shared_queue *
core_sock_shared_queue(struct core_sock *p)
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
extern int	core_sock_counter(void);
extern void	core_sock_release_id(pfq_id_t id);
extern int	core_sock_tx_bind(struct core_sock *so, int tid, int if_index, int queue);
extern int	core_sock_tx_unbind(struct core_sock *so);

extern int	core_sock_enable(struct core_sock *so, struct pfq_so_enable *mem);
extern int	core_sock_disable(struct core_sock *so);


#endif /* Q_CORE_SOCK_H */
