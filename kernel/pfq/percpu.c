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

#include <core/percpu.h>
#include <core/qbuff.h>

#include <pfq/percpu.h>
#include <pfq/qbuff.h>


int pfq_percpu_init(void)
{
	struct GC_data **GCs;
	int cpu, i, n = 0;

	GCs = (struct GC_data **)kzalloc(sizeof(struct GC_data *) * Q_CORE_MAX_CPU, GFP_KERNEL);
	if (!GCs) {
		printk(KERN_ERR "[PFQ] percpu: out of memory!\n");
		return -ENOMEM;
	}

	for_each_possible_cpu(cpu)
	{
		if (n == Q_CORE_MAX_CPU) {
			printk(KERN_ERR "[PFQ] percpu: maximum number of cpu reached (%d)!\n", Q_CORE_MAX_CPU);
			goto err;
		}

		GCs[n] = (struct GC_data *)kmalloc(sizeof(struct GC_data), GFP_KERNEL);
		if (!GCs[n]) {
			printk(KERN_ERR "[PFQ] percpu: could not allocate GC[%d]!\n", n);
			goto err;
		}
		n++;
	}


	n = 0;
        for_each_possible_cpu(cpu)
        {
                struct core_percpu_data *data;

		memset(per_cpu_ptr(global->percpu_stats, cpu), 0, sizeof(core_global_stats_t));
		memset(per_cpu_ptr(global->percpu_mem_stats, cpu), 0, sizeof(struct core_memory_stats));

		preempt_disable();

                data = per_cpu_ptr(global->percpu_data, cpu);
		pfq_setup_timer(&data->timer, cpu);
		data->counter = 0;
		data->GC = GCs[n++];
		GC_data_init(data->GC);

		preempt_enable();
	}

	kfree(GCs);
	return 0;
err:
	for(i = 0; i < n; i++)
		kfree(GCs[i]);

	kfree(GCs);
	return -ENOMEM;
}


int pfq_percpu_destruct(void)
{
        int cpu;
        int total = 0;

        /* destroy prefetch queues (of each cpu) */

        for_each_possible_cpu(cpu) {

		struct core_percpu_data *data;
		struct qbuff *buff;
		size_t n;

		preempt_disable();

                data = per_cpu_ptr(global->percpu_data, cpu);

		for_each_qbuff(&data->GC->pool, buff, n)
		{
			sparse_inc(global->percpu_mem_stats, os_free);
			kfree_skb(QBUFF_SKB(buff));
		}

                total += data->GC->pool.len;

		GC_reset(data->GC);

		pfq_del_timer(&data->timer);

		preempt_enable();
        }

	sparse_add(global->percpu_stats, lost, total);
        return total;
}




