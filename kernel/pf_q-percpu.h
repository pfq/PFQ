/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PF_Q_PERCPU_H
#define PF_Q_PERCPU_H

#include <pragma/diagnostic_push>

#include <linux/percpu.h>
#include <linux/ktime.h>
#include <linux/timer.h>
#include <linux/printk.h>

#include <pragma/diagnostic_pop>

#include <pf_q-pool.h>
#include <pf_q-define.h>

#include <lang/GC.h>


int pfq_percpu_init(void);
int pfq_percpu_destruct(void);
int pfq_percpu_alloc(void);
void pfq_percpu_free(void);


struct pfq_percpu_sock
{
        unsigned long           eligible_mask;
	unsigned long           mask[Q_MAX_SOCK_MASK];
        int                     cnt;

} ____cacheline_aligned;


struct pfq_percpu_pool
{
        atomic_t                enable;

	struct pfq_skb_pool	tx_pool;
        struct pfq_skb_pool	rx_pool;

} ____cacheline_aligned;


struct pfq_percpu_data
{
	struct GC_data		*GC;
	ktime_t			last_rx;
	struct timer_list	timer;
	uint32_t		counter;

} ____cacheline_aligned;


extern struct pfq_percpu_data __percpu * percpu_data;
extern struct pfq_percpu_sock __percpu * percpu_sock;
extern struct pfq_percpu_pool __percpu * percpu_pool;


static inline void
pfq_invalidate_percpu_eligible_mask(pfq_id_t id)
{
	int cpu;

	for_each_online_cpu(cpu)
	{
		struct pfq_percpu_sock * sock = per_cpu_ptr(percpu_sock, cpu);
		sock->eligible_mask = 0; /* TODO should be atomic */
		sock->cnt = 0;
	}
}


#endif /* PF_Q_PERCPU_H */
