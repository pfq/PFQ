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

#ifndef PF_Q_STATS_H
#define PF_Q_STATS_H

#include <pragma/diagnostic_push>
#include <linux/kernel.h>
#include <linux/pf_q.h>
#include <pragma/diagnostic_pop>

#include <pf_q-sparse.h>


/* sparse_counter_t stats */


struct pfq_socket_rx_stats
{
        sparse_counter_t  recv;         /* received by the queue */
        sparse_counter_t  lost;         /* packets lost due to socket queue congestion */
        sparse_counter_t  drop;         /* dropped by filters */
};


struct pfq_socket_tx_stats
{
        sparse_counter_t  sent;         /* sent by the driver */
        sparse_counter_t  disc;         /* discarded by the driver */
};


struct pfq_group_stats
{
        sparse_counter_t recv;		/* received by the group/computation */
        sparse_counter_t drop;          /* drop by computation: fanout monad */
        sparse_counter_t frwd;          /* forwarded to devices */
        sparse_counter_t kern;          /* passed to kernel */
        sparse_counter_t disc;          /* discarded due to driver congestion */
        sparse_counter_t abrt;          /* aborted (e.g. memory problems) */
};

static inline
void pfq_group_stats_reset(struct pfq_group_stats *stats)
{
        sparse_set(&stats->recv, 0);
        sparse_set(&stats->drop, 0);
        sparse_set(&stats->frwd, 0);
        sparse_set(&stats->kern, 0);
        sparse_set(&stats->disc, 0);
        sparse_set(&stats->abrt, 0);
}

struct pfq_global_stats
{
	sparse_counter_t recv;		/* received by PFQ */
	sparse_counter_t lost;		/* lost during capture, due to PFQ problem (e.g. memory problem) */
        sparse_counter_t sent;		/* transmitted from user-space */
        sparse_counter_t frwd;		/* forwarded to devices */
        sparse_counter_t kern;		/* passed to kernel */
        sparse_counter_t disc;		/* discarded due to driver congestion */
        sparse_counter_t abrt;		/* aborted (e.g. memory problems) */

        sparse_counter_t poll;		/* number of poll */
        sparse_counter_t wake;		/* number of wakeup */
};

static inline
void pfq_global_stats_reset(struct pfq_global_stats *stats)
{
	sparse_set(&stats->recv, 0);
	sparse_set(&stats->lost, 0);
	sparse_set(&stats->sent, 0);
	sparse_set(&stats->frwd, 0);
	sparse_set(&stats->kern, 0);
	sparse_set(&stats->disc, 0);
	sparse_set(&stats->abrt, 0);
	sparse_set(&stats->poll, 0);
	sparse_set(&stats->wake, 0);
}


struct pfq_memory_stats
{
	sparse_counter_t os_alloc;
	sparse_counter_t os_free;

	sparse_counter_t pool_alloc;
	sparse_counter_t pool_free;
	sparse_counter_t pool_push;
	sparse_counter_t pool_pop;

	sparse_counter_t err_norecyl;
	sparse_counter_t err_pop;
	sparse_counter_t err_push;
	sparse_counter_t err_intdis;
	sparse_counter_t err_shared;
	sparse_counter_t err_cloned;
	sparse_counter_t err_memory;
};


static inline
void pfq_memory_stats_reset(struct pfq_memory_stats *stats)
{
        sparse_set(&stats->os_alloc,   0);
        sparse_set(&stats->os_free,    0);

        sparse_set(&stats->pool_alloc, 0);
        sparse_set(&stats->pool_free,  0);
        sparse_set(&stats->pool_push,  0);
        sparse_set(&stats->pool_pop,   0);

        sparse_set(&stats->err_norecyl,0);
        sparse_set(&stats->err_pop,    0);
        sparse_set(&stats->err_push,   0);
        sparse_set(&stats->err_intdis, 0);
        sparse_set(&stats->err_shared, 0);
        sparse_set(&stats->err_cloned, 0);
        sparse_set(&stats->err_memory, 0);
}


struct pfq_pool_stat
{
	uint64_t        os_alloc;
	uint64_t        os_free;

	uint64_t        pool_alloc;
	uint64_t        pool_free;
	uint64_t        pool_push;
	uint64_t        pool_pop;

	uint64_t        err_norecyl;
	uint64_t        err_pop;
	uint64_t        err_push;
	uint64_t        err_intdis;
	uint64_t        err_shared;
	uint64_t        err_cloned;
	uint64_t        err_memory;
};


#endif /* PF_Q_STATS_H */
