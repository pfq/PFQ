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

#include <pragma/diagnostic_push>

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/version.h>
#include <linux/kthread.h>
#include <linux/mutex.h>

#include <pragma/diagnostic_pop>

#include <pf_q-macro.h>
#include <pf_q-thread.h>
#include <pf_q-memory.h>
#include <pf_q-sock.h>
#include <pf_q-transmit.h>


DEFINE_MUTEX(kthread_tx_pool_lock);

struct task_struct *kthread_tx_pool [Q_MAX_CPU] = { [0 ... 255] = NULL };


int
pfq_tx_wakeup(struct pfq_sock *so, int index)
{
	if (so->tx_opt.queue[index].task) {
		wake_up_process(so->tx_opt.queue[index].task);
		return 0;
	}

	return -EPERM;
}


int
pfq_tx_thread(void *_data)
{
        struct pfq_thread_data *data = (struct pfq_thread_data *)_data;
        struct net_device *dev;
	int cpu;

	if (data == NULL) {
		printk(KERN_INFO "[PFQ] Tx thread data error!\n");
		return -EPERM;
	}

	cpu = smp_processor_id();
        dev = dev_get_by_index(sock_net(&data->so->sk), data->so->tx_opt.queue[data->id].if_index);

	printk(KERN_INFO "[PFQ] Tx[%zu] thread started on cpu %d.\n", data->id, cpu);

	__set_current_state(TASK_RUNNING);

        for(;;)
        {
                __pfq_queue_xmit(data->id, &data->so->tx_opt, dev, cpu, cpu_to_node(cpu));

                if (kthread_should_stop())
                        break;

		pfq_relax();
        }

        dev_put(dev);

        printk(KERN_INFO "[PFQ] Tx[%zu] thread stopped on cpu %d.\n", data->id, cpu);

        kfree(data);
        return 0;
}


void
pfq_stop_all_tx_threads(struct pfq_sock *so)
{
	int n = 0;

	mutex_lock(&kthread_tx_pool_lock);

	for(n = 0; n < so->tx_opt.num_queues; n++)
	{
		if (so->tx_opt.queue[n].task) {

			pr_devel("[PFQ|%d] stopping Tx thread@%p\n", so->id.value, so->tx_opt.queue[n].task);

			if (so->tx_opt.queue[n].cpu != -1)
				BUG_ON(kthread_tx_pool[so->tx_opt.queue[n].cpu % Q_MAX_CPU] != so->tx_opt.queue[n].task);

			kthread_stop(so->tx_opt.queue[n].task);
			kthread_tx_pool[so->tx_opt.queue[n].cpu % Q_MAX_CPU] = NULL;
			so->tx_opt.queue[n].task = NULL;
			so->tx_opt.queue[n].cpu = -1;
		}
	}

	mutex_unlock(&kthread_tx_pool_lock);
}


