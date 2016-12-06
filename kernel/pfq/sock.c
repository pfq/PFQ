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

#include <pfq/sock.h>
#include <pfq/pool.h>

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
	struct core_sock *so = pfq_sk(sk);

	free_percpu(so->stats);
        so->stats = NULL;

        skb_queue_purge(&sk->sk_error_queue);

        WARN_ON(atomic_read(&sk->sk_rmem_alloc));
        WARN_ON(atomic_read(&sk->sk_wmem_alloc));

        sk_refcnt_debug_dec(sk);
}


