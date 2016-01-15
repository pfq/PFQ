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

#ifndef PF_Q_STATS_H
#define PF_Q_STATS_H

#include <pragma/diagnostic_push>
#include <linux/kernel.h>
#include <linux/cpumask.h>
#include <linux/percpu-defs.h>
#include <linux/pf_q.h>
#include <pragma/diagnostic_pop>

#include <pf_q-sparse.h>


struct pfq_kernel_stats
{
        local_t recv;		/* received by the queue/group/computation */
        local_t lost;		/* packets lost due to socket queue congestion */
        local_t drop;		/* dropped by filters or computation */
        local_t sent;		/* sent by the driver */
        local_t disc;		/* discarded due to driver congestion */
        local_t fail;		/* tx failed due to driver congestion */
        local_t frwd;		/* forwarded to devices */
        local_t kern;		/* passed to kernel */
};


typedef struct pfq_kernel_stats	pfq_sock_stats_t;
typedef struct pfq_kernel_stats	pfq_group_stats_t;
typedef struct pfq_kernel_stats	pfq_global_stats_t;


struct pfq_group_counters
{
	local_t	value[Q_MAX_COUNTERS];
};


struct pfq_memory_stats
{
	local_t os_alloc;
	local_t os_free;
	local_t pool_alloc;
	local_t pool_free;
	local_t pool_push;
	local_t pool_pop;
	local_t err_norecyl;
	local_t err_pop;
	local_t err_push;
	local_t err_intdis;
	local_t err_shared;
	local_t err_cloned;
	local_t err_memory;
};


struct pfq_pool_stat
{
	uint64_t os_alloc;
	uint64_t os_free;

	uint64_t pool_alloc;
	uint64_t pool_free;
	uint64_t pool_push;
	uint64_t pool_pop;

	uint64_t err_norecyl;
	uint64_t err_pop;
	uint64_t err_push;
	uint64_t err_intdis;
	uint64_t err_shared;
	uint64_t err_cloned;
	uint64_t err_memory;
};


extern void pfq_kernel_stats_read(struct pfq_kernel_stats __percpu *kstats, struct pfq_stats *stats);

extern void pfq_kernel_stats_reset(struct pfq_kernel_stats __percpu *stats);
extern void pfq_group_counters_reset(struct pfq_group_counters __percpu *counters);
extern void pfq_memory_stats_reset(struct pfq_memory_stats __percpu *stats);

static inline void pfq_global_stats_reset(struct pfq_kernel_stats __percpu *stats)
{
	pfq_kernel_stats_reset(stats);
}

static inline void pfq_group_stats_reset(struct pfq_kernel_stats __percpu *stats)
{
	pfq_kernel_stats_reset(stats);
}

static inline void pfq_sock_stats_reset(struct pfq_kernel_stats __percpu *stats)
{
	pfq_kernel_stats_reset(stats);
}


#endif /* PF_Q_STATS_H */
