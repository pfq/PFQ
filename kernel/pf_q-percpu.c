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

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/cpumask.h>
#include <linux/pf_q.h>

#include <pf_q-global.h>
#include <pf_q-memory.h>
#include <pf_q-module.h>
#include <pf_q-GC.h>


int pfq_percpu_init(void)
{
	int cpu;

	/* create a per-cpu context */

	cpu_data = alloc_percpu(struct local_data);
	if (!cpu_data) {
                printk(KERN_WARNING "[PFQ] out of memory!\n");
		return -ENOMEM;
        }

        for_each_possible_cpu(cpu) {

                struct local_data *local = per_cpu_ptr(cpu_data, cpu);

		gc_data_init(&local->gc);
	}

	return 0;
}


int pfq_percpu_flush(void)
{
        int cpu;
        int total = 0;

        /* destroy prefetch queues (of each cpu) */

        for_each_possible_cpu(cpu) {

                struct local_data *local = per_cpu_ptr(cpu_data, cpu);
	        struct sk_buff *skb;
		int n = 0;

		for_each_skbuff(&local->gc.pool, skb, n)
		{
                 	kfree_skb(skb);
		}

                total += local->gc.pool.len;

		gc_reset(&local->gc);
        }

        return total;
}


