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

#include <engine/percpu.h>
#include <engine/stats.h>
#include <engine/global.h>
#include <engine/lang/GC.h>


DEFINE_PER_CPU(pfq_global_stats_t, global_stats);
DEFINE_PER_CPU(struct pfq_memory_stats, memory_stats);

struct pfq_percpu_data __percpu    * percpu_data;
struct pfq_percpu_sock __percpu    * percpu_sock;
struct pfq_percpu_pool __percpu    * percpu_pool;

extern void pfq_timer (unsigned long);


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

	printk(KERN_INFO "[PFQ] number of online cpus %d\n", num_online_cpus());
        return 0;

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

	free_percpu(percpu_data);
	free_percpu(percpu_sock);
	free_percpu(percpu_pool);
}


int pfq_percpu_init(void)
{
	struct GC_data **GCs;
	int cpu, i, n = 0;

	GCs = (struct GC_data **)kzalloc(sizeof(struct GC_data *) * Q_MAX_CPU, GFP_KERNEL);
	if (!GCs) {
		printk(KERN_ERR "[PFQ] percpu: out of memory!\n");
		return -ENOMEM;
	}

	for_each_possible_cpu(cpu) {

		if (n == Q_MAX_CPU) {
			printk(KERN_ERR "[PFQ] percpu: maximum number of cpu reached (%d)!\n", Q_MAX_CPU);
			goto err;
		}

		GCs[n] = (struct GC_data *)kmalloc(sizeof(struct GC_data), GFP_KERNEL);
		if (!GCs[n]) {
			printk(KERN_ERR "[PFQ] percpu: could not allocate GC[%d]!\n", n);
			goto err;
		}
		n++;
	}

	/* allocate GCs */

	n = 0;
        for_each_possible_cpu(cpu) {

                struct pfq_percpu_data *data;

		preempt_disable();

                data = per_cpu_ptr(percpu_data, cpu);

		init_timer_deferrable(&data->timer);

		data->counter = 0;

		data->timer.function = pfq_timer;
		data->timer.data = (unsigned long)cpu;
		data->timer.expires = jiffies + msecs_to_jiffies(100);

		add_timer_on(&data->timer, cpu);

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

		struct pfq_percpu_data *data;
	        struct sk_buff *skb;
		int n = 0;

		preempt_disable();

                data = per_cpu_ptr(percpu_data, cpu);

		for_each_skbuff(SKBUFF_QUEUE_ADDR(data->GC->pool), skb, n)
		{
			sparse_inc(&memory_stats, os_free);
			kfree_skb(skb);
		}

                total += data->GC->pool.len;

		GC_reset(data->GC);
		del_timer(&data->timer);

		preempt_enable();
        }

	sparse_add(&global_stats, lost, total);
        return total;
}


