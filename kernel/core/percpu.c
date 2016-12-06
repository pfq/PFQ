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

#include <pfq/kcompat.h>
#include <pfq/timer.h>
#include <pfq/percpu.h>

#include <core/percpu.h>
#include <core/stats.h>
#include <core/global.h>
#include <core/GC.h>


int core_percpu_alloc(void)
{
	global->percpu_data = alloc_percpu(struct core_percpu_data);
	if (!global->percpu_data) {
                printk(KERN_ERR "[PFQ] could not allocate percpu data!\n");
		return -ENOMEM;
        }

	global->percpu_sock = alloc_percpu(struct core_percpu_sock);
	if (!global->percpu_sock) {
                printk(KERN_ERR "[PFQ] could not allocate percpu sock!\n");
                goto err0;
        }

	global->percpu_pool = alloc_percpu(struct pfq_percpu_pool);
	if (!global->percpu_pool) {
                printk(KERN_ERR "[PFQ] could not allocate percpu pool!\n");
                goto err1;
        }

	global->percpu_stats = alloc_percpu(core_global_stats_t);
	if (!global->percpu_stats) {
                printk(KERN_ERR "[PFQ] could not allocate percpu stats!\n");
                goto err2;
        }

	global->percpu_mem_stats = alloc_percpu(struct core_memory_stats);
	if (!global->percpu_mem_stats) {
                printk(KERN_ERR "[PFQ] could not allocate percpu memory stats!\n");
                goto err3;
        }

	printk(KERN_INFO "[PFQ] number of online cpus %d\n", num_online_cpus());
        return 0;

	free_percpu(global->percpu_mem_stats);
err3:   free_percpu(global->percpu_stats);
err2:   free_percpu(global->percpu_pool);
err1:	free_percpu(global->percpu_sock);
err0:	free_percpu(global->percpu_data);
	return -ENOMEM;
}


void core_percpu_free(void)
{
	int cpu;

	for_each_present_cpu(cpu) {

		struct core_percpu_data *data = per_cpu_ptr(global->percpu_data, cpu);

		kfree(data->GC);
		kfree(data->rx_fifo);
		kfree(data->rx_free);
	}


	free_percpu(global->percpu_stats);
	free_percpu(global->percpu_mem_stats);

	free_percpu(global->percpu_data);
	free_percpu(global->percpu_sock);
	free_percpu(global->percpu_pool);
	free_percpu(global->percpu_queue);
}


