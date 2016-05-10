/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
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

#include <engine/sock.h>

#include <pf_q-thread.h>
#include <pf_q-pool.h>


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
        that->tx_slot_size  = Q_SHARED_QUEUE_SLOT_SIZE(maxlen);
	that->tx_num_async_queues = 0;

	/* Tx async queues setup */

	for(n = 0; n < Q_MAX_TX_QUEUES; ++n)
	{
		pfq_tx_info_init(&that->txq_async[n]);
	}
}


int pfq_sock_init(struct pfq_sock *so, int id)
{
	int i;

	/* setup stats */

	so->stats = alloc_percpu(pfq_sock_stats_t);
	if (!so->stats)
		return -ENOMEM;

	for_each_possible_cpu(i)
	{
		pfq_sock_stats_t * stat = per_cpu_ptr(so->stats, i);

		local_set(&stat->recv, 0);
		local_set(&stat->lost, 0);
		local_set(&stat->drop, 0);
		local_set(&stat->sent, 0);
		local_set(&stat->disc, 0);
		local_set(&stat->fail, 0);
		local_set(&stat->frwd, 0);
		local_set(&stat->kern, 0);
	}

	/* setup id */

	so->id = (__force pfq_id_t)id;

        /* memory mapped queues are allocated later, when the socket is enabled */

	so->egress_type = pfq_endpoint_socket;
	so->egress_index = 0;
	so->egress_queue = 0;

	/* default weight */

	so->weight = 1;

        so->shmem.addr = NULL;
        so->shmem.size = 0;
        so->shmem.kind = 0;
        so->shmem.hugepages = NULL;
        so->shmem.npages = 0;

        return 0;
}


void pfq_sock_destruct(struct sock *sk)
{
	struct pfq_sock *so = pfq_sk(sk);

	free_percpu(so->stats);
        so->stats = NULL;

        skb_queue_purge(&sk->sk_error_queue);

        WARN_ON(atomic_read(&sk->sk_rmem_alloc));
        WARN_ON(atomic_read(&sk->sk_wmem_alloc));

        sk_refcnt_debug_dec(sk);
}


int
pfq_sock_tx_bind(struct pfq_sock *so, int tid, int ifindex, int qindex)
{
	size_t queue = so->opt.tx_num_async_queues;
	int err = 0;

	if (queue >= Q_MAX_TX_QUEUES) {
		printk(KERN_INFO "[PFQ|%d] could not bind Tx[%d] thread to queue %zu (out of range)!\n", so->id, tid, queue);
		return -EPERM;
	}

	so->opt.txq_async[queue].def_ifindex = ifindex;
	so->opt.txq_async[queue].def_queue = qindex;
	so->opt.tx_num_async_queues++;

	smp_wmb();

	if ((err = pfq_bind_tx_thread(tid, so, queue)) < 0)
	{
		so->opt.txq_async[queue].def_ifindex = -1;
		so->opt.txq_async[queue].def_queue = -1;
		so->opt.tx_num_async_queues--;
		return err;
	}

	return 0;
}


int
pfq_sock_tx_unbind(struct pfq_sock *so)
{
	size_t n;

	so->opt.txq.def_ifindex = -1;
	so->opt.txq.def_queue = -1;

	/* unbind async Tx queue */

	if (pfq_unbind_tx_thread(so) < 0)
		return -EPERM;

	for(n = 0; n < Q_MAX_TX_QUEUES; ++n)
	{
		so->opt.txq_async[n].def_ifindex = -1;
		so->opt.txq_async[n].def_queue = -1;
	}

	return 0;
}

