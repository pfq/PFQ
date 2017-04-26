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

#ifndef Q_CORE_PERCPU_H
#define Q_CORE_PERCPU_H

#include <core/spsc_fifo.h>
#include <core/global.h>
#include <core/define.h>
#include <core/GC.h>

#include <pfq/kcompat.h>
#include <pfq/timer.h>
#include <pfq/printk.h>
#include <pfq/pool.h>


int  core_percpu_alloc(void);
void core_percpu_free(void);


struct core_percpu_data
{
	struct GC_data		*GC;

	ktime_t			last_rx;
	struct timer_list	timer;
	uint32_t		counter;

        unsigned long		sock_eligible_mask;
	unsigned long           sock_mask[Q_CORE_MAX_SOCK_MASK];
        int                     sock_cnt;

} ____pfq_cacheline_aligned;


static inline void
core_invalidate_percpu_eligible_mask(pfq_id_t id)
{
	int cpu;

	(void)id;
	for_each_present_cpu(cpu)
	{
		struct core_percpu_data * this_data = per_cpu_ptr(global->percpu_data, cpu);
		this_data->sock_eligible_mask = 0;
		this_data->sock_cnt = 0;
	}
}


#endif /* Q_CORE_PERCPU_H */
