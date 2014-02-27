/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#include <pf_q-common.h>


extern atomic_long_t pfq_sock_vector[Q_MAX_ID];


struct pfq_rx_opt
{
        struct pfq_rx_queue_hdr *queue_info;
        void *                  base_addr;

        int                     tstamp;

        size_t                  caplen;
        size_t                  offset;

        size_t                  size;
        size_t                  slot_size;

        wait_queue_head_t       waitqueue;
        pfq_rx_stat_t           stat;

} __attribute__((aligned(64)));

static inline
void pfq_rx_opt_init(struct pfq_rx_opt *that, size_t caplen)
{
        /* the queue is allocate later, when the socket is enabled */

        that->queue_info = NULL;
        that->base_addr  = NULL;

        /* disable tiemstamping by default */
        that->tstamp    = false;

        /* set q_slots and q_caplen default values */

        that->caplen    = caplen;
        that->offset    = 0;

        that->size      = 0;
        that->slot_size = 0;

        /* initialize waitqueue */

        init_waitqueue_head(&that->waitqueue);

        /* reset stats */
        sparse_set(&that->stat.recv, 0);
        sparse_set(&that->stat.lost, 0);
        sparse_set(&that->stat.drop, 0);

}


struct pfq_tx_opt
{
        struct pfq_tx_queue_hdr *queue_info;
        void *                  base_addr;

        uint64_t                counter;

        size_t                  maxlen;

        size_t                  size;
        size_t                  slot_size;

        struct net_device       *dev;
        struct netdev_queue     *txq;

        int                     if_index;
        int                     hw_queue;
        int                     cpu;

        struct task_struct      *thread;

        pfq_tx_stat_t           stat;

} __attribute__((aligned(64)));

static inline
void pfq_tx_opt_init(struct pfq_tx_opt *that, size_t maxlen)
{
        /* the queue is allocate later, when the socket is enabled */

        that->queue_info        = NULL;
        that->base_addr         = NULL;

        that->counter           = 0;

        that->maxlen            = maxlen;
        that->size              = 0;
        that->slot_size         = 0;

        that->dev               = NULL;
        that->txq               = NULL;

        that->if_index          = -1;
        that->hw_queue          = -1;
        that->cpu               = -1;

        that->thread            = NULL;

        sparse_set(&that->stat.sent, 0);
        sparse_set(&that->stat.disc, 0);
}


struct pfq_sock
{
        struct sock sk;

        int                 id;

        void *              mem_addr;         /* global memory mapped area */
        size_t              mem_size;         /* global memory mapped size */

        struct pfq_rx_opt   rx_opt;
        struct pfq_tx_opt   tx_opt;
};

static inline
struct pfq_queue_hdr *
get_pfq_queue_hdr(struct pfq_sock *p)
{
        return (struct pfq_queue_hdr *) p->mem_addr;
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
