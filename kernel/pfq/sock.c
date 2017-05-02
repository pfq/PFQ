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

#include <pfq/atomic.h>
#include <pfq/global.h>
#include <pfq/kcompat.h>
#include <pfq/pool.h>
#include <pfq/printk.h>
#include <pfq/queue.h>
#include <pfq/sock.h>
#include <pfq/sock.h>
#include <pfq/thread.h>

#include <linux/pf_q.h>

void
pfq_sock_init_once(void)
{
#ifdef PFQ_USE_SKB_POOL
	atomic_set(&global->pool_enabled, 1);
#endif
}


void
pfq_sock_fini_once(void)
{
#ifdef PFQ_USE_SKB_POOL
	atomic_set(&global->pool_enabled, 0);
#endif
}


void pfq_sock_init_waitqueue_head(wait_queue_head_t *queue)
{
	init_waitqueue_head(queue);
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

/* vector of pointers to pfq_sock */

pfq_id_t
pfq_sock_get_free_id(struct pfq_sock * so)
{
        int n = 0;

        for(; n < (__force int)Q_MAX_ID; n++)
        {
                if (atomic_long_cmpxchg(&global->socket_ptr[n], (long)0, (long)so) == 0) {
			if(atomic_inc_return(&global->socket_count) == 1)
				pfq_sock_init_once();
			return (__force pfq_id_t)n;
                }
        }
        return (__force pfq_id_t)-ENOMEM;
}


int pfq_sock_counter(void)
{
        return atomic_read(&global->socket_count);
}


struct pfq_sock *
pfq_sock_get_by_id(pfq_id_t id)
{
        if ((__force int)id >= Q_MAX_ID ||
            (__force int)id < 0) {
                pr_devel("[PFQ] pfq_get_sock_by_id: bad id=%d!\n", id);
                return NULL;
        }
	return (struct pfq_sock *)atomic_long_read(&global->socket_ptr[(__force int)id]);
}


void pfq_sock_release_id(pfq_id_t id)
{
        if ((__force int)id >= Q_MAX_ID ||
	    (__force int)id < 0) {
                pr_devel("[PFQ] pfq_release_sock_by_id: bad id=%d!\n", id);
                return;
        }

        atomic_long_set(global->socket_ptr + (__force int)id, 0);

        if (atomic_dec_return(&global->socket_count) == 0) {
		pr_devel("[PFQ] calling sock_fini_once...\n");
		msleep(Q_GRACE_PERIOD);
		pfq_sock_fini_once();
	}
}


int pfq_sock_init(struct pfq_sock *so, pfq_id_t id, size_t caplen, size_t maxlen)
{
	int i;

	/* setup stats */

	so->stats = alloc_percpu(pfq_sock_stats_t);
	if (!so->stats)
		return -ENOMEM;

	for_each_present_cpu(i)
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

	so->id = id;

        /* memory mapped queues are allocated later, when the socket is enabled */

	so->egress_type = Q_ENDPOINT_SOCKET;
	so->egress_index = 0;
	so->egress_queue = 0;

	/* default weight */

	so->weight = 1;

        so->shmem.addr = NULL;
        so->shmem.size = 0;
        so->shmem.kind = 0;
        so->shmem.hugepages_descr = NULL;


        /* disable tiemstamping by default */

        so->tstamp = false;

        /* initialize waitqueue */

        pfq_sock_init_waitqueue_head(&so->waitqueue);

	/* Rx queue setup */

	pfq_rxq_info_init(&so->rx);

        so->caplen = caplen;
        so->rx_queue_len = 0;
        so->rx_slot_size = 0;

	/* Tx queues setup */

	pfq_txq_info_init(&so->tx);

        so->tx_queue_len  = 0;
        so->tx_slot_size  = PFQ_SHARED_QUEUE_SLOT_SIZE(maxlen);
	so->txq_num_async = 0;

	/* Tx async queues setup */

	for(i = 0; i < Q_MAX_TX_QUEUES; ++i)
	{
		pfq_txq_info_init(&so->tx_async[i]);
	}
        return 0;
}


int
pfq_sock_tx_bind(struct pfq_sock *so, int tid, int ifindex, int qindex)
{
	int queue = (int)so->txq_num_async;
	int err = 0;

	if (queue >= Q_MAX_TX_QUEUES) {
		printk(KERN_INFO "[PFQ|%d] could not bind Tx[%d] thread to queue %d (out of range)!\n", so->id, tid, queue);
		return -EPERM;
	}

	so->tx_async[queue].ifindex = ifindex;
	so->tx_async[queue].queue = qindex;
	so->txq_num_async++;

	smp_wmb();

	if ((err = pfq_bind_tx_thread(tid, so, queue)) < 0)
	{
		so->tx_async[queue].ifindex = -1;
		so->tx_async[queue].queue = -1;
		so->txq_num_async--;
		return err;
	}

	return 0;
}


int
pfq_sock_tx_unbind(struct pfq_sock *so)
{
	size_t n;

	so->tx.ifindex = -1;
	so->tx.queue = -1;

	/* unbind async Tx queue */

	if (pfq_unbind_tx_thread(so) < 0)
		return -EPERM;

	for(n = 0; n < Q_MAX_TX_QUEUES; ++n)
	{
		so->tx_async[n].ifindex = -1;
		so->tx_async[n].queue = -1;
	}

	return 0;
}


int
pfq_sock_enable(struct pfq_sock *so, struct pfq_so_enable *mem)
{
        int err;

	printk(KERN_INFO "[PFQ|%d] enable: mapping user_addr=%p user_size=%zu hugepage_size=%zu...\n", so->id,
		(void *)mem->user_addr, mem->user_size, mem->hugepage_size);

        err = pfq_shared_queue_enable(so, mem->user_addr, mem->user_size, mem->hugepage_size);
        if (err < 0) {
                printk(KERN_INFO "[PFQ|%d] enable error!\n", so->id);
                return err;
        }

	if (mem->hugepage_size) {
		if (!so->shmem.hugepages_descr) {
			printk(KERN_INFO "[PFQ|%d] enable error (null HugePages descriptor)!\n", so->id);
			return -EFAULT;
		}
	}

	return 0;
}


int
pfq_sock_disable(struct pfq_sock *so)
{
	if (so->shmem.addr) {

		/* unbind Tx threads */

		pr_devel("[PFQ|%d] unbinding Tx threads...\n", so->id);
		pfq_sock_tx_unbind(so);

		pr_devel("[PFQ|%d] leaving all groups...\n", so->id);
		pfq_group_leave_all(so->id);

		pr_devel("[PFQ|%d] unlinking shared queue...\n", so->id);
		pfq_shared_queue_unlink(so);

		msleep(Q_GRACE_PERIOD * 4);

		pr_devel("[PFQ|%d] unmapping shared queue...\n", so->id);
		pfq_shared_queue_unmap(so);
	}
	else {
		pr_devel("[PFQ|%d] socket (already) disabled.\n", so->id);
	}

	return 0;
}



