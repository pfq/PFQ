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

#ifndef Q_CORE_STATS_H
#define Q_CORE_STATS_H

#include <core/define.h>

#include <pfq/kcompat.h>
#include <pfq/atomic.h>
#include <pfq/sparse.h>

#include <linux/pf_q.h>



struct core_kernel_stats
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


typedef struct core_kernel_stats	core_sock_stats_t;
typedef struct core_kernel_stats	core_group_stats_t;
typedef struct core_kernel_stats	core_global_stats_t;


struct core_group_counters
{
	local_t	value[Q_MAX_COUNTERS];
};


struct core_memory_stats
{
	local_t os_alloc;
	local_t os_free;

	local_t pool_push;
	local_t pool_pop;
	local_t pool_empty;
	local_t pool_norecycl;

	local_t err_shared;
	local_t err_cloned;
	local_t err_memory;
	local_t err_irqdis;
	local_t err_nolinr;
};


struct core_pool_stat
{
	uint64_t os_alloc;
	uint64_t os_free;

	uint64_t pool_push;
	uint64_t pool_pop;
	uint64_t pool_empty;
	uint64_t pool_norecycl;

	uint64_t err_shared;
	uint64_t err_cloned;
	uint64_t err_memory;
	uint64_t err_irqdis;
	uint64_t err_nolinr;
};


extern void core_kernel_stats_read(struct core_kernel_stats __percpu *kstats, struct pfq_stats *stats);
extern void core_kernel_stats_reset(struct core_kernel_stats __percpu *stats);
extern void core_group_counters_reset(struct core_group_counters __percpu *counters);
extern void core_memory_stats_reset(struct core_memory_stats __percpu *stats);

static inline void core_global_stats_reset(struct core_kernel_stats __percpu *stats)
{
	core_kernel_stats_reset(stats);
}

static inline void core_group_stats_reset(struct core_kernel_stats __percpu *stats)
{
	core_kernel_stats_reset(stats);
}

static inline void core_sock_stats_reset(struct core_kernel_stats __percpu *stats)
{
	core_kernel_stats_reset(stats);
}


#endif /* Q_CORE_STATS_H */
