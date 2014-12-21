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

#ifndef _PF_Q_SOCK_H_
#define _PF_Q_SOCK_H_

#include <linux/kernel.h>
#include <linux/poll.h>
#include <linux/pf_q.h>

#include <net/sock.h>

#include <pf_q-macro.h>
#include <pf_q-stats.h>
#include <pf_q-shmem.h>


extern atomic_long_t pfq_sock_vector[Q_MAX_ID];


struct pfq_rx_opt
{
	atomic_long_t 		queue_hdr;
	void 		       *base_addr;

	int    			tstamp;

	size_t 			caplen;

	size_t 			queue_size;
	size_t 			slot_size;

	wait_queue_head_t 	waitqueue;

        struct pfq_socket_rx_stats stats;

} ____cacheline_aligned_in_smp;


static inline
struct pfq_rx_queue_hdr *
pfq_get_rx_queue_hdr(struct pfq_rx_opt *that)
{
	return (struct pfq_rx_queue_hdr *)atomic_long_read(&that->queue_hdr);
}


static inline
void pfq_rx_opt_init(struct pfq_rx_opt *that, size_t caplen)
{
        /* the queue is allocate later, when the socket is enabled */

        atomic_long_set(&that->queue_hdr, 0);

        that->base_addr = NULL;

        /* disable tiemstamping by default */
        that->tstamp = false;

        /* set q_slots and q_caplen default values */

        that->caplen = caplen;

        that->queue_size = 0;
        that->slot_size = 0;

        /* initialize waitqueue */

        init_waitqueue_head(&that->waitqueue);

        /* reset stats */
        sparse_set(&that->stats.recv, 0);
        sparse_set(&that->stats.lost, 0);
        sparse_set(&that->stats.drop, 0);

}


struct pfq_tx_queue_info
{
	atomic_long_t 		queue_hdr;
	void 		       *base_addr;

	int 			if_index;
	int 			hw_queue;
	int 			cpu;

	struct task_struct     *task;
};


struct pfq_tx_opt
{
	uint64_t 		counter;

	size_t  		maxlen;
	size_t  		queue_size;
	size_t  		slot_size;
        size_t 	       	 	num_queues;

	struct pfq_tx_queue_info queue[Q_MAX_TX_QUEUES];

	struct pfq_socket_tx_stats stats;

} ____cacheline_aligned_in_smp;


static inline
struct pfq_tx_queue_hdr *
pfq_get_tx_queue_hdr(struct pfq_tx_opt *that, int index)
{
	return (struct pfq_tx_queue_hdr *)atomic_long_read(&that->queue[index].queue_hdr);
}


static inline
void pfq_tx_opt_init(struct pfq_tx_opt *that, size_t maxlen)
{
        /* the queue is allocate later, when the socket is enabled */
        int n;

        that->counter = 0;

        that->maxlen = maxlen;
        that->queue_size = 0;
        that->slot_size  = 0;
	that->num_queues = 0;

	for(n = 0; n < Q_MAX_TX_QUEUES; ++n)
	{
		atomic_long_set(&that->queue[n].queue_hdr, 0);

        	that->queue[n].base_addr = NULL;
		that->queue[n].if_index  = -1;
		that->queue[n].hw_queue  = -1;
		that->queue[n].cpu       = -1;
		that->queue[n].task 	 = NULL;
       	}

        sparse_set(&that->stats.sent, 0);
        sparse_set(&that->stats.disc, 0);
}


struct pfq_sock
{
        struct sock 		sk;

        int                 	id;

	int		    	egress_type;
        int 		    	egress_index;
        int 		    	egress_queue;

        void *              	shmem_addr;         	/* memory mapped area */
        size_t              	shmem_size;         	/* memory mapped size */
	enum pfq_shmem_kind 	shmem_kind;

	struct page ** 		shmem_hugepages;
	size_t 			shmem_npages;

        struct pfq_rx_opt   	rx_opt;
        struct pfq_tx_opt   	tx_opt;

} ____cacheline_aligned_in_smp;


static inline
struct pfq_queue_hdr *
pfq_get_queue_hdr(struct pfq_sock *p)
{
        return (struct pfq_queue_hdr *) p->shmem_addr;
}


static inline struct pfq_sock *
pfq_sk(struct sock *sk)
{
        return (struct pfq_sock *)(sk);
}


int    pfq_get_sock_count(void);
int    pfq_get_free_sock_id(struct pfq_sock * so);
struct pfq_sock * pfq_get_sock_by_id(size_t id);
void   pfq_release_sock_id(int id);


#endif /* _PF_COMMON_H_ */
