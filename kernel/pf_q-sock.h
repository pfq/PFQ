/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PF_Q_SOCK_H
#define PF_Q_SOCK_H

#include <pragma/diagnostic_push>
#include <linux/kernel.h>
#include <linux/poll.h>
#include <linux/pf_q.h>
#include <net/sock.h>
#include <pragma/diagnostic_pop>

#include <pf_q-define.h>
#include <pf_q-endpoint.h>
#include <pf_q-stats.h>
#include <pf_q-shmem.h>
#include <pf_q-types.h>


extern atomic_long_t pfq_sock_vector[Q_MAX_ID];


struct pfq_tx_qinfo
{
	atomic_long_t		queue_ptr;
	void			*base_addr;

	int			if_index;
	int			queue;
	int			cpu;
	struct task_struct	*task;
};


struct pfq_rx_qinfo
{
	atomic_long_t		queue_ptr;
	void			*base_addr;
};



struct pfq_sock_opt
{
	int			tstamp;
	size_t			caplen;

	size_t			rx_queue_size;
	size_t			rx_slot_size;

	size_t			tx_queue_size;
	size_t			tx_slot_size;
        size_t			tx_num_queues;

	wait_queue_head_t	waitqueue;

	struct pfq_tx_qinfo	tx_queue[Q_MAX_TX_QUEUES];
	struct pfq_rx_qinfo	rx_queue;

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

        struct pfq_sock_stats	stats;

} ____cacheline_aligned_in_smp;



static inline
struct pfq_rx_queue *
pfq_get_rx_queue(struct pfq_sock_opt *that)
{
	return (struct pfq_rx_queue *)atomic_long_read(&that->rx_queue.queue_ptr);
}


static inline
struct pfq_tx_queue *
pfq_get_tx_queue(struct pfq_sock_opt *that, int index)
{
	return (struct pfq_tx_queue *)atomic_long_read(&that->tx_queue[index].queue_ptr);
}


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
void	pfq_sock_init(struct pfq_sock *so, int id);
int     pfq_get_sock_count(void);
struct	pfq_sock * pfq_get_sock_by_id(pfq_id_t id);
void	pfq_release_sock_id(pfq_id_t id);


#endif /* PF_Q_SOCK_H */
