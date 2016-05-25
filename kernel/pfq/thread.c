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

#include <pragma/diagnostic_push>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/version.h>
#include <linux/kthread.h>
#include <linux/mutex.h>
#include <linux/jiffies.h>
#include <pragma/diagnostic_pop>

#include <core/define.h>
#include <core/sock.h>

#include <pfq/thread.h>
#include <pfq/memory.h>
#include <pfq/io.h>


static DEFINE_MUTEX(pfq_thread_tx_pool_lock);


static struct pfq_thread_tx_data pfq_thread_tx_pool[Q_CORE_MAX_CPU] =
{
	[0 ... Q_CORE_MAX_CPU-1] = {
		.id	= -1,
		.cpu    = -1,
		.node   = -1,
		.task	= NULL,
		.sock   = {NULL, NULL, NULL, NULL},
		.sock_queue = {{-1}, {-1}, {-1}, {-1}}
	}
};



#ifdef PFQ_DEBUG
static void
pfq_tx_thread_dump(struct pfq_thread_tx_data const *data)
{
	char msg[256];
	int n, off = 0;
	off += sprintf(msg + off, "Tx[%d] cpu=%d node=%d ", data->id, data->cpu, data->node);
	for(n = 0; n < Q_CORE_MAX_TX_QUEUES; ++n)
	{
		int sock_queue = atomic_read(&data->sock_queue[n]);
		if (sock_queue != -1)
			off += sprintf(msg + off, "(sock:%d,queue:%d) ", data->sock[n]->id, sock_queue);
	}

	printk(KERN_INFO "[PFQ] %s...(PING!)\n", msg);
}
#endif

static int
pfq_tx_thread(void *_data)
{
	struct pfq_thread_tx_data *data = (struct pfq_thread_tx_data *)_data;

#ifdef PFQ_DEBUG
        int now = 0;
#endif

	if (data == NULL) {
		printk(KERN_INFO "[PFQ] Tx thread data error!\n");
		return -EPERM;
	}

	printk(KERN_INFO "[PFQ] Tx[%d] thread started on cpu %d.\n", data->id, data->cpu);

	__set_current_state(TASK_RUNNING);

        for(;;)
	{
		/* transmit the registered socket's queues */
		bool reg = false;
		int total_sent = 0, n;

		for(n = 0; n < Q_MAX_TX_QUEUES; n++)
		{
			struct core_sock *sock;
			int sock_queue;
			tx_res_t tx;

			sock_queue = atomic_read(&data->sock_queue[n]);
			smp_rmb();
			sock = data->sock[n];
			if (sock_queue != -1 && sock != NULL) {
				reg = true;
				tx = pfq_sk_queue_xmit(sock, sock_queue, data->cpu, data->node, &data->sock_queue[n]);
				total_sent += tx.ok;

				sparse_add(sock->stats,	  sent, tx.ok);
				sparse_add(sock->stats,   fail, tx.fail);
				sparse_add(global->percpu_stats,  sent, tx.ok);
				sparse_add(global->percpu_stats,  fail, tx.fail);
			}
		}

                if (kthread_should_stop())
                        break;

		pfq_relax();

#ifdef PFQ_DEBUG
		if (now != jiffies/(HZ*30)) {
			now = jiffies/(HZ*30);
			pfq_tx_thread_dump(data);
		}
#endif

		if (total_sent == 0)
			schedule();

		if (!reg)
			msleep(1);
	}

        printk(KERN_INFO "[PFQ] Tx[%d] thread stopped on cpu %d.\n", data->id, data->cpu);
	data->task = NULL;
        return 0;
}


int
pfq_bind_tx_thread(int tid, struct core_sock *sock, int sock_queue)
{
	struct pfq_thread_tx_data *thread_data;
	int n;

	if (tid >= global->tx_thread_nr) {
		printk(KERN_INFO "[PFQ] Tx[%d] thread not available (%d Tx threads running)!\n", tid, global->tx_thread_nr);
		return -ESRCH;
	}

	thread_data = &pfq_thread_tx_pool[tid];

	mutex_lock(&pfq_thread_tx_pool_lock);

	for(n = 0; n < Q_MAX_TX_QUEUES; n++)
	{
		if (atomic_read(&thread_data->sock_queue[n]) == -1)
			break;
	}

	if (n == Q_MAX_TX_QUEUES) {
		mutex_unlock(&pfq_thread_tx_pool_lock);
		printk(KERN_INFO "[PFQ] Tx[%d] thread busy (no queue available)!\n", tid);
		return -EBUSY;
	}

	thread_data->sock[n] = sock;
	smp_wmb();
	atomic_set(&thread_data->sock_queue[n], sock_queue);

        mutex_unlock(&pfq_thread_tx_pool_lock);
        printk(KERN_INFO "[PFQ] Tx[%d] thread bound to sock_id = %d, queue = %d...\n", tid, sock->id, sock_queue);
        return 0;
}


int
pfq_unbind_tx_thread(struct core_sock *sock)
{
	int n, i;
	mutex_lock(&pfq_thread_tx_pool_lock);

	for(n = 0; n < global->tx_thread_nr; n++)
	{
		struct pfq_thread_tx_data *data = &pfq_thread_tx_pool[n];

		for(i = 0; i < Q_MAX_TX_QUEUES; i++)
		{
			if (atomic_read(&data->sock_queue[i]) != -1)
			{
				if (data->sock[i] == sock) {
					atomic_set(&data->sock_queue[i], -1);
					smp_wmb();
					msleep(Q_CORE_GRACE_PERIOD);
					data->sock[i] = NULL;
				}
			}
		}
	}

        mutex_unlock(&pfq_thread_tx_pool_lock);
        return 0;
}


int
pfq_start_all_tx_threads(void)
{
	int err = 0;

	if (global->tx_thread_nr)
	{
		int n;
		printk(KERN_INFO "[PFQ] starting %d Tx thread(s)...\n", global->tx_thread_nr);

		for(n = 0; n < global->tx_thread_nr; n++)
		{
			struct pfq_thread_tx_data *data = &pfq_thread_tx_pool[n];

			data->id = n;
			data->cpu = global->tx_affinity[n];
			data->node = cpu_online(global->tx_affinity[n]) ? cpu_to_node(global->tx_affinity[n]) : NUMA_NO_NODE;
			data->task = kthread_create_on_node(pfq_tx_thread,
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

			wake_up_process(data->task);
		}
	}

	return err;
}


void
pfq_stop_all_tx_threads(void)
{
	if (global->tx_thread_nr)
	{
		int n;

		printk(KERN_INFO "[PFQ] stopping %d Tx thread(s)...\n", global->tx_thread_nr);

		for(n = 0; n < global->tx_thread_nr; n++)
		{
			struct pfq_thread_tx_data *data = &pfq_thread_tx_pool[n];

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
					atomic_set(&data->sock_queue[i], -1);
					data->sock[i] = NULL;
				}
			}
		}
	}
}

