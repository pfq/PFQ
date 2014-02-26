/***************************************************************
 *
 * (C) 2014 Nicola Bonelli <nicola.bonelli@cnit.it>
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
#include <linux/pf_q-fun.h>

#include <pf_q-memory.h>

int pfq_prefetch_purge_all(void)
{
        int cpu;
        int total = 0;

        /* destroy prefetch queues (of each cpu) */

        for_each_possible_cpu(cpu) {

                struct local_data *local = per_cpu_ptr(cpu_data, cpu);
                struct pfq_non_intrusive_skb *this_queue = &local->prefetch_queue;
                struct sk_buff *skb;
		int n = 0;

		pfq_non_intrusive_for_each(skb, n, this_queue)
		{
                        struct pfq_annotation *cb = pfq_skb_annotation(skb);
                        if (unlikely(cb->stolen_skb))
                                continue;
                 	kfree_skb(skb);
		}

                total += pfq_non_intrusive_len(this_queue);

       		pfq_non_intrusive_flush(this_queue);
        }

        return total;
}


