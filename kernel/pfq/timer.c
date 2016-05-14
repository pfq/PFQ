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

#ifndef PF_Q_NET_HEADERS_H
#define PF_Q_NET_HEADERS_H

#include <engine/percpu.h>
#include <pfq/timer.h>
#include <pfq/io.h>


void pfq_timer(unsigned long cpu)
{
	struct pfq_percpu_data *data;

	pfq_receive(NULL, NULL, 0);
	data = per_cpu_ptr(percpu_data, cpu);
	mod_timer_pinned(&data->timer, jiffies + msecs_to_jiffies(100));
}


void pfq_setup_timer(struct timer_list *timer, unsigned long cpu)
{
	init_timer_deferrable(timer);

	timer->function = pfq_timer;
	timer->data = (unsigned long)cpu;
	timer->expires = jiffies + msecs_to_jiffies(100);

	add_timer_on(timer, cpu);
}


void pfq_del_timer(struct timer_list *timer)
{
	del_timer(timer);
}


#endif /* PF_Q_NET_HEADERS_H */

