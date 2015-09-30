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
#include <pragma/diagnostic_pop>

#include <pf_q-stats.h>


void pfq_sock_stats_reset(struct pfq_sock_stats __percpu *stats)
{
	int i;
	for_each_possible_cpu(i)
	{
		struct pfq_sock_stats * stat = per_cpu_ptr(stats, i);

		local_set(&stat->recv, 0);
		local_set(&stat->lost, 0);
		local_set(&stat->drop, 0);
		local_set(&stat->sent, 0);
		local_set(&stat->disc, 0);
	}
}


void pfq_group_stats_reset(struct pfq_group_stats __percpu *stats)
{
	int i;
	for_each_possible_cpu(i)
	{
		struct pfq_group_stats * stat = per_cpu_ptr(stats, i);

		local_set(&stat->recv, 0);
		local_set(&stat->drop, 0);
		local_set(&stat->frwd, 0);
		local_set(&stat->kern, 0);
		local_set(&stat->disc, 0);
		local_set(&stat->abrt, 0);
	}
}


void pfq_group_counters_reset(struct pfq_group_counters __percpu *counters)
{

	int i, n;
	for_each_possible_cpu(i)
	{
		struct pfq_group_counters * ctr = per_cpu_ptr(counters, i);
		for(n = 0; n < Q_MAX_COUNTERS; n++)
			local_set(&ctr->value[n], 0);
	}
}


void pfq_global_stats_reset(struct pfq_global_stats __percpu *stats)
{
	int i;
	for_each_possible_cpu(i)
	{
		struct pfq_global_stats * stat = per_cpu_ptr(stats, i);

		local_set(&stat->recv, 0);
		local_set(&stat->lost, 0);
		local_set(&stat->sent, 0);
		local_set(&stat->frwd, 0);
		local_set(&stat->kern, 0);
		local_set(&stat->disc, 0);
		local_set(&stat->abrt, 0);
		local_set(&stat->poll, 0);
		local_set(&stat->wake, 0);
	}
}


void pfq_memory_stats_reset(struct pfq_memory_stats __percpu *stats)
{
	int i;
	for_each_possible_cpu(i)
	{
		struct pfq_memory_stats * stat = per_cpu_ptr(stats, i);

		local_set(&stat->os_alloc,   0);
		local_set(&stat->os_free,    0);
		local_set(&stat->pool_alloc, 0);
		local_set(&stat->pool_free,  0);
		local_set(&stat->pool_push,  0);
		local_set(&stat->pool_pop,   0);
		local_set(&stat->err_norecyl,0);
		local_set(&stat->err_pop,    0);
		local_set(&stat->err_push,   0);
		local_set(&stat->err_intdis, 0);
		local_set(&stat->err_shared, 0);
		local_set(&stat->err_cloned, 0);
		local_set(&stat->err_memory, 0);
	}
}

