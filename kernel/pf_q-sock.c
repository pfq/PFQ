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

#include <linux/kernel.h>
#include <linux/module.h>

#include <linux/version.h>
#include <linux/types.h>

#include <pf_q-sock.h>
#include <pf_q-memory.h>

/* vector of pointers to pfq_sock */

static atomic_t      pfq_sock_count;

atomic_long_t pfq_sock_vector[Q_MAX_ID];


static void
pfq_sock_init(void)
{
#ifdef PFQ_USE_SKB_POOL
	pfq_skb_pool_enable(true);
#endif
}


static void
pfq_sock_finish(void)
{
#ifdef PFQ_USE_SKB_POOL
	pfq_skb_pool_enable(false);
#endif
}


pfq_id_t
pfq_get_free_id(struct pfq_sock * so)
{
        int n = 0;
        pfq_id_t id;

        for(; n < Q_MAX_ID; n++)
        {
                if (!atomic_long_cmpxchg(pfq_sock_vector + n, 0, (long)so)) {
			if(atomic_inc_return(&pfq_sock_count) == 1)
				pfq_sock_init();
			id.value = n;
                        return id;
                }
        }
        id.value = -ENOMEM;
        return id;
}


int pfq_get_sock_count(void)
{
        return atomic_read(&pfq_sock_count);
}


struct pfq_sock *
pfq_get_sock_by_id(pfq_id_t id)
{
        struct pfq_sock *so;
        if (unlikely(id.value >= Q_MAX_ID)) {
                pr_devel("[PFQ] pfq_get_sock_by_id: bad id=%d!\n", id.value);
                return NULL;
        }
	so = (struct pfq_sock *)atomic_long_read(&pfq_sock_vector[id.value]);
	smp_read_barrier_depends();
	return so;
}


void pfq_release_sock_id(pfq_id_t id)
{
        if (unlikely(id.value >= Q_MAX_ID || id.value < 0)) {
                pr_devel("[PFQ] pfq_release_sock_by_id: bad id=%d!\n", id.value);
                return;
        }

        atomic_long_set(pfq_sock_vector + id.value, 0);
        if (atomic_dec_return(&pfq_sock_count) == 0)
        	pfq_sock_finish();
}


