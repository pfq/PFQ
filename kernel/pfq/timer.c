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

#include <pfq/timer.h>
#include <pfq/io.h>


static void pfq_timer(unsigned long cpu)
{
	struct core_percpu_data *data;

	pfq_receive(NULL, NULL, 0);
	data = per_cpu_ptr(global->percpu_data, cpu);

#if LINUX_VERSION_CODE < KERNEL_VERSION(2, 6, 31) || LINUX_VERSION_CODE >= KERNEL_VERSION(4, 8, 0)
	mod_timer(&data->timer, jiffies + msecs_to_jiffies(100));
#else
	mod_timer_pinned(&data->timer, jiffies + msecs_to_jiffies(100));
#endif
}


static
void pfq_setup_timer(struct timer_list *timer, unsigned long cpu)
{
	init_timer_deferrable(timer);

	timer->function = pfq_timer;
	timer->data = (unsigned long)cpu;
	timer->expires = jiffies + msecs_to_jiffies(100);

	add_timer_on(timer, cpu);
}



void pfq_timer_init(void)
{
	int cpu;
	for_each_present_cpu(cpu)
	{
                struct core_percpu_data *data;
		preempt_disable();
		data = per_cpu_ptr(global->percpu_data, cpu);
        	pfq_setup_timer(&data->timer, cpu);
		preempt_enable();
	}
}


void pfq_timer_fini(void)
{
	int cpu;
	for_each_present_cpu(cpu)
	{
                struct core_percpu_data *data;
		preempt_disable();
		data = per_cpu_ptr(global->percpu_data, cpu);
        	del_timer(&data->timer);
		preempt_enable();
	}
}


