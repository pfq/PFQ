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

#include <pragma/diagnostic_push>

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/version.h>
#include <linux/types.h>
#include <pragma/diagnostic_pop>

#include <pf_q-thread.h>
#include <pf_q-dev.h>
#include <pf_q-sock.h>
#include <pf_q-memory.h>

/* vector of pointers to pfq_sock */

static atomic_t      pfq_sock_count;

atomic_long_t pfq_sock_vector[Q_MAX_ID];


static void
pfq_sock_init_once(void)
{
#ifdef PFQ_USE_SKB_POOL
	pfq_skb_pool_enable(true);
#endif
}


static void
pfq_sock_finish_once(void)
{
#ifdef PFQ_USE_SKB_POOL
	pfq_skb_pool_enable(false);
#endif
}


pfq_id_t
pfq_get_free_id(struct pfq_sock * so)
{
        int n = 0;

        for(; n < (__force int)Q_MAX_ID; n++)
        {
                if (!atomic_long_cmpxchg(pfq_sock_vector + n, 0, (long)so)) {
			if(atomic_inc_return(&pfq_sock_count) == 1)
				pfq_sock_init_once();
			return (__force pfq_id_t)n;
                }
        }
        return (__force pfq_id_t)-ENOMEM;
}


int pfq_get_sock_count(void)
{
        return atomic_read(&pfq_sock_count);
}


struct pfq_sock *
pfq_get_sock_by_id(pfq_id_t id)
{
        struct pfq_sock *so;
        if (unlikely((__force int)id >= Q_MAX_ID)) {
                pr_devel("[PFQ] pfq_get_sock_by_id: bad id=%d!\n", id);
                return NULL;
        }
	so = (struct pfq_sock *)atomic_long_read(&pfq_sock_vector[(__force int)id]);
	smp_read_barrier_depends();
	return so;
}


void pfq_release_sock_id(pfq_id_t id)
{
        if (unlikely((__force int)id >= Q_MAX_ID ||
		     (__force int)id < 0)) {
                pr_devel("[PFQ] pfq_release_sock_by_id: bad id=%d!\n", id);
                return;
        }

        atomic_long_set(pfq_sock_vector + (__force int)id, 0);
        if (atomic_dec_return(&pfq_sock_count) == 0)
		pfq_sock_finish_once();
}


void pfq_sock_opt_init(struct pfq_sock_opt *that, size_t caplen, size_t maxlen)
{
        int n;

        /* disable tiemstamping by default */

        that->tstamp = false;

        /* initialize waitqueue */

        init_waitqueue_head(&that->waitqueue);

	/* Rx queue setup */

	pfq_rx_info_init(&that->rxq);

        that->caplen = caplen;
        that->rx_queue_len = 0;
        that->rx_slot_size = 0;

	/* Tx queues setup */

	pfq_tx_info_init(&that->txq);

        that->tx_queue_len  = 0;
        that->tx_slot_size  = Q_QUEUE_SLOT_SIZE(maxlen);
	that->tx_num_async_queues = 0;

	/* Tx async queues setup */

	for(n = 0; n < Q_MAX_TX_QUEUES; ++n)
	{
		pfq_tx_info_init(&that->txq_async[n]);
	}
}


void pfq_sock_init(struct pfq_sock *so, int id)
{
	so->id = id;

        /* memory mapped queues are allocated later, when the socket is enabled */

	so->egress_type   = pfq_endpoint_socket;
	so->egress_index  = 0;
	so->egress_queue  = 0;

	/* default weight */

	so->weight = 1;

        so->shmem.addr = NULL;
        so->shmem.size = 0;
        so->shmem.kind = 0;
        so->shmem.hugepages = NULL;
        so->shmem.npages = 0;

        /* reset stats */

        sparse_set(&so->stats.recv, 0);
        sparse_set(&so->stats.lost, 0);
        sparse_set(&so->stats.drop, 0);
        sparse_set(&so->stats.sent, 0);
        sparse_set(&so->stats.disc, 0);
}


int
pfq_sock_tx_bind(struct pfq_sock *so, int tid, int if_index, int queue, struct
		 net_device *dev)
{
	size_t i = so->opt.tx_num_async_queues;

	if (i >= Q_MAX_TX_QUEUES)
		return -EPERM;

	so->opt.txq_async[i].def_ifindex = if_index;
	so->opt.txq_async[i].def_queue = queue;
	so->opt.txq_async[i].def_dev = dev;
	so->opt.tx_num_async_queues++;

	smp_wmb();

	if (pfq_bind_tx_thread(tid, so, queue) < 0)
	{
		so->opt.txq_async[i].def_ifindex = -1;
		so->opt.txq_async[i].def_queue = -1;
		so->opt.txq_async[i].def_dev = NULL;
		so->opt.tx_num_async_queues--;

		printk(KERN_INFO "[PFQ|%d] could not bind Tx[%d] thread: resource busy!\n", so->id, tid);
		return -EBUSY;
	}

	return 0;
}


int
pfq_sock_tx_unbind(struct pfq_sock *so)
{
	size_t n;

	/* unbind sync Tx queue */

	if (so->opt.txq.def_ifindex != -1) {
		dev_put_by_index(sock_net(&so->sk), so->opt.txq.def_ifindex);
	}

	so->opt.txq.def_ifindex = -1;
	so->opt.txq.def_queue = -1;
	so->opt.txq.def_dev = NULL;

	/* unbind async Tx queue */

	if (pfq_unbind_tx_thread(so) < 0)
		return -EPERM;

	for(n = 0; n < Q_MAX_TX_QUEUES; ++n)
	{
		if (so->opt.txq_async[n].def_ifindex != -1)
			dev_put_by_index(sock_net(&so->sk), so->opt.txq_async[n].def_ifindex);

		so->opt.txq_async[n].def_ifindex = -1;
		so->opt.txq_async[n].def_queue = -1;
		so->opt.txq_async[n].def_dev = NULL;
	}

	return 0;
}

