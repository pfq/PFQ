/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
 *             Andrea Di Pietro <andrea.dipietro@for.unipi.it>
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

#ifndef _PF_COMMON_H_
#define _PF_COMMON_H_

#include <linux/kernel.h>
#include <linux/poll.h>

#include <net/sock.h>

#include <pf_q-sparse-counter.h>


#define GRACE_PERIOD 10     /* msec */

#define PFQ_TX_RING_SIZE  (8192)
#define PFQ_TX_RING_MASK  (PFQ_TX_RING_SIZE-1)


/* sparse_counter_t stats */

typedef struct pfq_kstats
{
    sparse_counter_t  recv;    // received by the queue
    sparse_counter_t  lost;    // queue is full, packet is lost
    sparse_counter_t  drop;    // filter

} pfq_kstat_t;


struct pfq_rx_opt
{
        int                 id;
        int                 tstamp;

        void *              addr;
        size_t              queue_mem;  /* > sizeof(pfq_queue_descr) + q_slots * sizeof(slots) * 2 */

        size_t              slots;      /* number of slots per queue */
        size_t              caplen;
        size_t              offset;
        size_t              slot_size;

        wait_queue_head_t   waitqueue;
        pfq_kstat_t         stat;

        int                 active;

} __attribute__((aligned(128)));


struct pfq_tx_opt
{
        struct sk_buff     *skb_slot[PFQ_TX_RING_SIZE];  /* preallocated skbuff ring */

        unsigned int        skb_index;
        uint64_t            counter;

        struct net_device   *dev;
        struct netdev_queue *txq;

        int                 hardware_queue;
        int                 cpu_index;

        void *              q_mem;
        size_t              q_tot_mem;

        struct task_struct  *thread;
        bool                thread_running;

} __attribute__((aligned(128)));


struct pfq_sock
{
        struct sock sk;
        struct pfq_rx_opt *rx_opt;
        struct pfq_tx_opt *tx_opt;
};


#endif /* _PF_COMMON_H_ */
