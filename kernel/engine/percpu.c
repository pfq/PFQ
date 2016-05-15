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
#include <pfq/GC.h>

#include <engine/percpu.h>
#include <engine/stats.h>
#include <engine/global.h>


pfq_global_stats_t	__percpu   * global_stats;
struct pfq_memory_stats __percpu   * memory_stats;

struct pfq_percpu_data __percpu    * percpu_data;
struct pfq_percpu_sock __percpu    * percpu_sock;
struct pfq_percpu_pool __percpu    * percpu_pool;


int pfq_percpu_alloc(void)
{
	percpu_data = alloc_percpu(struct pfq_percpu_data);
	if (!percpu_data) {
                printk(KERN_ERR "[PFQ] could not allocate percpu data!\n");
		return -ENOMEM;
        }

	percpu_sock = alloc_percpu(struct pfq_percpu_sock);
	if (!percpu_sock) {
                printk(KERN_ERR "[PFQ] could not allocate percpu sock!\n");
                goto err0;
        }

	percpu_pool = alloc_percpu(struct pfq_percpu_pool);
	if (!percpu_pool) {
                printk(KERN_ERR "[PFQ] could not allocate percpu pool!\n");
                goto err1;
        }

	global_stats = alloc_percpu(pfq_global_stats_t);
	if (!global_stats) {
                printk(KERN_ERR "[PFQ] could not allocate percpu pool!\n");
                goto err2;
        }

	memory_stats = alloc_percpu(struct pfq_memory_stats);
	if (!memory_stats) {
                printk(KERN_ERR "[PFQ] could not allocate percpu pool!\n");
                goto err3;
        }

	printk(KERN_INFO "[PFQ] number of online cpus %d\n", num_online_cpus());
        return 0;


err3:   free_percpu(global_stats);
err2:   free_percpu(percpu_pool);
err1:	free_percpu(percpu_sock);
err0:	free_percpu(percpu_data);

	return -ENOMEM;
}


void pfq_percpu_free(void)
{
        struct pfq_percpu_data *data;
	int cpu;

	for_each_possible_cpu(cpu) {
                data = per_cpu_ptr(percpu_data, cpu);
		kfree(data->GC);
	}

	free_percpu(global_stats);
	free_percpu(memory_stats);

	free_percpu(percpu_data);
	free_percpu(percpu_sock);
	free_percpu(percpu_pool);
}


