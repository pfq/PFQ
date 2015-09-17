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

#include <pf_q-define.h>
#include <pf_q-thread.h>
#include <pf_q-memory.h>
#include <pf_q-sock.h>
#include <pf_q-transmit.h>


DEFINE_MUTEX(kthread_tx_pool_lock);


struct task_struct *kthread_tx_pool [Q_MAX_CPU] = { [0 ... 255] = NULL };


static struct pfq_thread_tx_data kthread_tx_pool_NG[Q_MAX_CPU] =
{
	[0 ... Q_MAX_CPU-1] = {
		.id	= -1,
		.cpu    = -1,
		.task	= NULL,
		.sock   = {NULL},
		.qindex = {{-1}}
	}
};


static int
pfq_tx_thread_NG(void *_data)
{
	struct pfq_thread_tx_data *data = (struct pfq_thread_tx_data *)_data;

	if (data == NULL) {
		printk(KERN_INFO "[PFQ] Tx thread data error!\n");
		return -EPERM;
	}

	printk(KERN_INFO "[PFQ] Tx[%d] thread-NG started on cpu %d.\n", data->id, data->cpu);

	__set_current_state(TASK_RUNNING);
        for(;;)
	{
		int n;

		/* transmit the registered socket's queues */

		for(n = 0; n < Q_MAX_TX_QUEUES; n++)
		{
			/* TODO: check for memory barriers */

			int qindex = atomic_read(&data->qindex[n]);
			if (qindex != -1 && data->sock != NULL)
				pfq_sk_queue_xmit_NG(data->sock[n], qindex, data->cpu, data->node);
		}

                if (kthread_should_stop())
                        break;

		pfq_relax();
	}

        printk(KERN_INFO "[PFQ] Tx[%d] thread-NG stopped on cpu %d.\n", data->id, data->cpu);
        return 0;
}


int
pfq_start_all_tx_threads_NG(void)
{
	int err = 0;

	if (async_tx_nr)
	{
		int n;
		printk(KERN_INFO "[PFQ] starting %d Tx thread(s)...\n", async_tx_nr);

		for(n = 0; n < async_tx_nr; n++)
		{
			struct pfq_thread_tx_data *data = &kthread_tx_pool_NG[n];

			data->id  = n;
			data->cpu = async_tx[n];
			data->node = cpu_online(async_tx[n]) ? cpu_to_node(async_tx[n]) : NUMA_NO_NODE;

			data->task = kthread_create_on_node(pfq_tx_thread_NG,
							    data, data->node,
							    "kpfq/%d:%d", n, data->cpu);
			if (IS_ERR(data->task)) {
				printk(KERN_INFO "[PFQ] kernel_thread: create failed on cpu %d!\n",
				       data->cpu);
				err = PTR_ERR(data->task);
				data->task = NULL;
				return err;
			}

			kthread_bind(data->task, data->cpu);
			pr_devel("[PFQ] created Tx[%d] kthread on cpu %d...\n", data->id, data->cpu);
		}
	}

	return err;
}


void
pfq_stop_all_tx_threads_NG(void)
{
	if (async_tx_nr)
	{
		int n;

		printk(KERN_INFO "[PFQ] stopping %d Tx thread(s)...\n", async_tx_nr);

		for(n = 0; n < async_tx_nr; n++)
		{
			struct pfq_thread_tx_data *data = &kthread_tx_pool_NG[n];

			if (data->task)
			{
				int i;
				pr_devel("[PFQ stopping Tx[%d] thread@%p\n", data->id, data->task);
				kthread_stop(data->task);
				data->id   = -1;
				data->cpu  = -1;
				data->task = NULL;
				for(i=0; i < Q_MAX_TX_QUEUES; ++i)
				{
					data->sock[i] = NULL;
					atomic_set(&data->qindex[i], -1);
				}
			}
		}
	}
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
        dev = dev_get_by_index(sock_net(&data->so->sk), data->so->opt.txq[data->id].if_index);

	printk(KERN_INFO "[PFQ] Tx[%zu] thread started on cpu %d.\n", data->id, cpu);

	__set_current_state(TASK_RUNNING);

        for(;;)
        {
                __pfq_sk_tx_queue_xmit(data->so, dev, data->id, cpu, cpu_to_node(cpu));

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

	for(n = 0; n < so->opt.tx_num_queues; n++)
	{
		if (so->opt.txq[n].task) {

			pr_devel("[PFQ|%d] stopping Tx thread@%p\n", so->id, so->opt.txq[n].task);

			if (so->opt.txq[n].cpu != -1)
				BUG_ON(kthread_tx_pool[so->opt.txq[n].cpu % Q_MAX_CPU] != so->opt.txq[n].task);

			kthread_stop(so->opt.txq[n].task);
			kthread_tx_pool[so->opt.txq[n].cpu % Q_MAX_CPU] = NULL;

			so->opt.txq[n].task = NULL;
			so->opt.txq[n].cpu = -1;
		}
	}

	mutex_unlock(&kthread_tx_pool_lock);
}


