/***************************************************************
 *
 * (C) 2014 Nicola Bonelli <nicola@pfq.io>
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
#include <linux/module.h>
#include <linux/cpumask.h>
#include <linux/timer.h>
#include <linux/pf_q.h>

#include <pragma/diagnostic_pop>

#include <pf_q-global.h>
#include <pf_q-memory.h>
#include <pf_q-module.h>
#include <pf_q-GC.h>


struct pfq_percpu_data __percpu    * percpu_data;

extern void pfq_timer (unsigned long);


int pfq_percpu_alloc(void)
{
	percpu_data = alloc_percpu(struct pfq_percpu_data);
	if (!percpu_data) {
                printk(KERN_ERR "[PFQ] could not allocate percpu data!\n");
		return -ENOMEM;
        }

	printk(KERN_INFO "[PFQ] number of online cpus %d\n", num_online_cpus());
        return 0;
}


void pfq_percpu_free(void)
{
        struct pfq_percpu_data *local;
	int cpu;

	for_each_online_cpu(cpu) {
                local = per_cpu_ptr(percpu_data, cpu);
		kfree(local->GC);
	}

	free_percpu(percpu_data);
}


int pfq_percpu_init(void)
{
	struct GC_data *GCs[Q_MAX_CPU];
	int cpu, i, n = 0;

	for_each_online_cpu(cpu) {

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
        for_each_online_cpu(cpu) {

                struct pfq_percpu_data *local;

		preempt_disable();

                local = per_cpu_ptr(percpu_data, cpu);

		init_timer_deferrable(&local->timer);

		local->timer.function = pfq_timer;
		local->timer.data = (unsigned long)cpu;
		local->timer.expires = jiffies + msecs_to_jiffies(100);

		add_timer_on(&local->timer, cpu);

		local->GC = GCs[n++];

		GC_data_init(local->GC);

		preempt_enable();
	}

	return 0;
err:
	for(i = 0; i < n; i++)
		kfree(GCs[i]);

	return -ENOMEM;
}


int pfq_percpu_fini(void)
{
        int cpu;
        int total = 0;

        /* destroy prefetch queues (of each cpu) */

        for_each_online_cpu(cpu) {

		struct pfq_percpu_data *local;
	        struct sk_buff *skb;
		int n = 0;

		preempt_disable();

                local = per_cpu_ptr(percpu_data, cpu);

		for_each_skbuff(SKBUFF_QUEUE(local->GC->pool), skb, n)
		{
			SPARSE_INC(&memory_stats.os_free);
			kfree_skb(skb);
		}

                total += local->GC->pool.len;

		GC_reset(local->GC);
		del_timer(&local->timer);

		preempt_enable();
        }

	sparse_add(&global_stats.lost, total);
        return total;
}


