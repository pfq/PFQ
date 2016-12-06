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
	struct GC_data **GCptrs;
	int cpu, i;

	GCptrs = (struct GC_data **)kzalloc(sizeof(struct GC_data *) * Q_CORE_MAX_CPU, GFP_KERNEL);
	if (!GCptrs) {
		printk(KERN_ERR "[PFQ] percpu: out of memory!\n");
		return -ENOMEM;
	}

	for_each_present_cpu(cpu)
	{
		if (cpu >= Q_CORE_MAX_CPU) {
			printk(KERN_ERR "[PFQ] percpu: maximum number of cpu reached (%d)!\n", Q_CORE_MAX_CPU);
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
                struct core_percpu_data *data;

		memset(per_cpu_ptr(global->percpu_stats, cpu), 0, sizeof(core_global_stats_t));
		memset(per_cpu_ptr(global->percpu_mem_stats, cpu), 0, sizeof(struct core_memory_stats));

		preempt_disable();

                data = per_cpu_ptr(global->percpu_data, cpu);

		data->counter = 0;
		data->GC = GCptrs[cpu];

		data->rx_fifo = core_spsc_init(global->skb_pool_size, cpu);
		data->rx_free = core_spsc_init(global->skb_pool_size, cpu);
                data->rx_napi = true;

		GC_data_init(data->GC);

		preempt_enable();
	}

	kfree(GCptrs);
	return 0;

err:
	for(i = 0; i < Q_CORE_MAX_CPU; i++)
		kfree(GCptrs[i]);

	kfree(GCptrs);
	return -ENOMEM;
}


int pfq_percpu_destruct(void)
{
        int cpu;
        int total = 0;

        /* destroy prefetch queues (of each cpu) */

        for_each_present_cpu(cpu) {

		struct core_percpu_data *data;
		struct sk_buff *skb;
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

		while ((skb = core_spsc_pop(data->rx_fifo)))
		{
			sparse_inc(global->percpu_mem_stats, os_free);
			kfree_skb(skb);
			total++;
		}
		while ((skb = core_spsc_pop(data->rx_free)))
		{
			sparse_inc(global->percpu_mem_stats, os_free);
			kfree_skb(skb);
			total++;
		}

		preempt_enable();
        }

	sparse_add(global->percpu_stats, lost, total);
        return total;
}


