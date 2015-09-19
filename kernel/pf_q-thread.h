/*
 * Copyright (c) 2014 Bonelli Nicola <nicola@pfq.io>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met: 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer. 2.
 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#ifndef PF_Q_THREAD_H
#define PF_Q_THREAD_H

#include <pragma/diagnostic_push>
#include <linux/kthread.h>
#include <linux/mutex.h>
#include <pragma/diagnostic_pop>

#include <pf_q-sock.h>
#include <pf_q-define.h>

extern struct mutex kthread_tx_pool_lock;
extern struct task_struct *kthread_tx_pool [Q_MAX_CPU];

extern int pfq_start_all_tx_threads(void);
extern void pfq_stop_all_tx_threads(void);

extern int pfq_bind_tx_thread(int tx_index, struct pfq_sock *sock, int sock_queue);
extern int pfq_unbind_tx_thread(struct pfq_sock *sock);


struct pfq_thread_data
{
	struct pfq_sock *so;
	size_t		id;
};


static inline
void pfq_relax(void)
{
	if (need_resched())
		schedule();
	else
		cpu_relax();
}


struct pfq_thread_tx_data
{
	int			id;
	int			cpu;
	int			node;
	struct task_struct *	task;
	struct pfq_sock *	sock[Q_MAX_TX_QUEUES];
	atomic_t		sock_queue[Q_MAX_TX_QUEUES];
};


static inline
bool is_kthread_should_stop(void)
{
	return (current->flags & PF_KTHREAD) && kthread_should_stop();
}


#endif /* PF_Q_THREAD_H */
