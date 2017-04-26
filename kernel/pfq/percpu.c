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

#include <pfq/percpu.h>
#include <pfq/qbuff.h>
#include <pfq/percpu.h>
#include <pfq/qbuff.h>
#include <pfq/memory.h>

int pfq_percpu_alloc(void)
{
	global->percpu_data = alloc_percpu(struct pfq_percpu_data);
	if (!global->percpu_data) {
                printk(KERN_ERR "[PFQ] could not allocate percpu data!\n");
		return -ENOMEM;
        }

	global->percpu_pool = alloc_percpu(struct pfq_percpu_pool);
	if (!global->percpu_pool) {
                printk(KERN_ERR "[PFQ] could not allocate percpu pool!\n");
                goto err1;
        }

	global->percpu_stats = alloc_percpu(pfq_global_stats_t);
	if (!global->percpu_stats) {
                printk(KERN_ERR "[PFQ] could not allocate percpu stats!\n");
                goto err2;
        }

	global->percpu_memory = alloc_percpu(struct pfq_memory_stats);
	if (!global->percpu_memory) {
                printk(KERN_ERR "[PFQ] could not allocate percpu memory stats!\n");
                goto err3;
        }

	printk(KERN_INFO "[PFQ] number of online cpus %d\n", num_online_cpus());
        return 0;

	free_percpu(global->percpu_memory);
err3:   free_percpu(global->percpu_stats);
err2:   free_percpu(global->percpu_pool);
err1:	free_percpu(global->percpu_data);
	return -ENOMEM;
}


void pfq_percpu_free(void)
{
	int cpu;

	for_each_present_cpu(cpu) {

		struct pfq_percpu_data *data = per_cpu_ptr(global->percpu_data, cpu);

		kfree(data->GC);
	}

	free_percpu(global->percpu_stats);
	free_percpu(global->percpu_memory);
	free_percpu(global->percpu_data);
	free_percpu(global->percpu_pool);
}


int pfq_percpu_init(void)
{
	struct GC_data **GCptrs;
	int cpu, i;

	GCptrs = (struct GC_data **)kzalloc(sizeof(struct GC_data *) * Q_MAX_CPU, GFP_KERNEL);
	if (!GCptrs) {
		printk(KERN_ERR "[PFQ] percpu: out of memory!\n");
		return -ENOMEM;
	}

	for_each_present_cpu(cpu)
	{
		if (cpu >= Q_MAX_CPU) {
			printk(KERN_ERR "[PFQ] percpu: maximum number of cpu reached (%d)!\n", Q_MAX_CPU);
			goto err;
		}

		GCptrs[cpu] = (struct GC_data *)kmalloc_node(sizeof(struct GC_data), GFP_KERNEL, cpu_to_node(cpu));
		if (!GCptrs[cpu]) {
			printk(KERN_ERR "[PFQ] percpu: could not allocate GC[%d]!\n", cpu);
			goto err;
		}
	}

        for_each_present_cpu(cpu)
        {
                struct pfq_percpu_data *data;

		memset(per_cpu_ptr(global->percpu_stats, cpu), 0, sizeof(pfq_global_stats_t));
		memset(per_cpu_ptr(global->percpu_memory, cpu), 0, sizeof(struct pfq_memory_stats));

		preempt_disable();

                data = per_cpu_ptr(global->percpu_data, cpu);

		data->counter = 0;
		data->GC = GCptrs[cpu];

		GC_data_init(data->GC);

		preempt_enable();
	}

	kfree(GCptrs);
	return 0;

err:
	for(i = 0; i < Q_MAX_CPU; i++)
		kfree(GCptrs[i]);

	kfree(GCptrs);
	return -ENOMEM;
}



int pfq_percpu_GC_reset(void)
{
        int cpu;
        int total = 0;

        /* destroy prefetch queues (of each cpu) */

        for_each_present_cpu(cpu) {

		struct pfq_percpu_data *data;
		struct pfq_percpu_pool *pool;
		struct qbuff *buff;
		size_t n;

		preempt_disable();

                data = per_cpu_ptr(global->percpu_data, cpu);
                pool = per_cpu_ptr(global->percpu_pool, cpu);

		for_each_qbuff(&data->GC->pool, buff, n)
		{
			pfq_kfree_skb_pool(QBUFF_SKB(buff), &pool->rx);
		}

                total += data->GC->pool.len;

		GC_reset(data->GC);

		preempt_enable();
        }

	sparse_add(global->percpu_stats, lost, total);
	return total;
}


int pfq_percpu_destruct(void)
{
        int cpu;
        int total = 0;

        /* destroy prefetch queues (of each cpu) */

        for_each_present_cpu(cpu) {

		struct pfq_percpu_data *data;

		preempt_disable();

                data = per_cpu_ptr(global->percpu_data, cpu);

		// for_each_qbuff(&data->GC->pool, buff, n)
		// {
		// 	sparse_inc(global->percpu_memory, os_free);
		// 	kfree_skb(QBUFF_SKB(buff));
		// }

                total += data->GC->pool.len;

		GC_reset(data->GC);

		preempt_enable();
        }

	sparse_add(global->percpu_stats, lost, total);
        return total;
}


