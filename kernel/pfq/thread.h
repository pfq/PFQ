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

#ifndef PFQ_THREAD_H
#define PFQ_THREAD_H

#include <pfq/sock.h>
#include <pfq/define.h>

#include <linux/kthread.h>
#include <linux/mutex.h>
#include <linux/rwsem.h>


struct pfq_sock;

extern struct task_struct *kthread_tx_pool [Q_MAX_CPU];

extern int  pfq_start_tx_threads(void);
extern void pfq_stop_tx_threads(void);
extern int  pfq_bind_tx_thread(int tx_index, struct pfq_sock *sock, int sock_queue);
extern int  pfq_unbind_tx_thread(struct pfq_sock *sock);

extern int pfq_check_threads_affinity(void);
extern int pfq_check_napi_contexts(void);

struct pfq_thread_data
{
	int			id;
	int			cpu;
	struct task_struct *	task;
};


struct pfq_thread_tx_data
{
	int			id;
	int			cpu;
	struct task_struct *	task;

	/* specific for Tx data */

	struct pfq_sock *	sock[Q_MAX_TX_QUEUES];
	atomic_t		sock_queue[Q_MAX_TX_QUEUES];

} ____pfq_cacheline_aligned;



static inline
void pfq_relax(void)
{
	if (need_resched())
		schedule();
	else
		cpu_relax();
}


static inline
bool is_kthread_should_stop(void)
{
	return (current->flags & PF_KTHREAD) && kthread_should_stop();
}


#endif /* PFQ_THREAD_H */
