/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
 *             Andrea Di Pietro <andrea.dipietro@for.unipi.it>
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

#include <pfq/global.h>
#include <pfq/kcompat.h>
#include <pfq/printk.h>
#include <pfq/memory.h>
#include <pfq/shmem.h>
#include <pfq/queue.h>


int
pfq_shared_queue_enable(struct pfq_sock *so, unsigned long user_addr, size_t user_size, size_t hugepage_size)
{
	if (!atomic_long_read(&so->shmem.addr)) {

		struct pfq_shared_queue * mapped_queue;
                unsigned int i; size_t n;
                void *addr;

		/* alloc queue memory */

		if (pfq_shared_memory_alloc(so->id, &so->shmem, user_addr, user_size, hugepage_size, pfq_total_queue_mem_aligned(so)) < 0)
		{
			return -ENOMEM;
		}

		addr = (void *)atomic_long_read(&so->shmem.addr);

		/* initialize queues headers */

		mapped_queue = (struct pfq_shared_queue *)addr;

		/* initialize Rx queue */

		mapped_queue->rx.shinfo    = 0;
		mapped_queue->rx.len       = (unsigned int)so->rx_queue_len;
		mapped_queue->rx.size      = (unsigned int)pfq_mpsc_queue_mem(so)/2;
		mapped_queue->rx.slot_size = (unsigned int)so->rx_slot_size;

		so->rx.shmem_addr = addr + sizeof(struct pfq_shared_queue);

		/* reset Rx slots */

		for(i = 0; i < 2; i++)
		{
			char * raw = addr + sizeof(struct pfq_shared_queue) + i * mapped_queue->rx.size;
			char * end = raw + mapped_queue->rx.size;
			const int rst = !i;
			for(;raw < end; raw += mapped_queue->rx.slot_size)
				((struct pfq_pkthdr *)raw)->info.commit = (uint16_t)rst;
		}

		/* initialize TX queues */

		mapped_queue->tx.size  = pfq_spsc_queue_mem(so)/2;

		mapped_queue->tx.prod.index = 0;
		mapped_queue->tx.prod.off0  = 0;
		mapped_queue->tx.prod.off1  = 0;
		mapped_queue->tx.cons.index = 0;
		mapped_queue->tx.cons.off   = 0;

		so->tx.shmem_addr = addr + sizeof(struct pfq_shared_queue) + pfq_mpsc_queue_mem(so);


		/* initialize TX async queues */

		for(n = 0; n < Q_MAX_TX_QUEUES; n++)
		{
			mapped_queue->tx_async[n].size  = pfq_spsc_queue_mem(so)/2;

			mapped_queue->tx_async[n].prod.index = 0;
			mapped_queue->tx_async[n].prod.off0  = 0;
			mapped_queue->tx_async[n].prod.off1  = 0;
			mapped_queue->tx_async[n].cons.index = 0;
			mapped_queue->tx_async[n].cons.off   = 0;

			so->tx_async[n].shmem_addr = addr + sizeof(struct pfq_shared_queue)
				+ pfq_mpsc_queue_mem(so)
				+ pfq_spsc_queue_mem(so) * (1 + n);
		}

		/* commit queues */

		smp_wmb();

		atomic_long_set(&so->rx.addr, (long)&mapped_queue->rx);
		atomic_long_set(&so->tx.addr, (long)&mapped_queue->tx);

		for(n = 0; n < Q_MAX_TX_QUEUES; n++)
		{
			atomic_long_set(&so->tx_async[n].addr, (long)&mapped_queue->tx_async[n]);
		}

		pr_devel("[PFQ|%d] Rx queue: len=%zu slot_size=%zu caplen=%zu, mem=%zu bytes\n",
			 so->id,
			 so->rx_queue_len,
			 so->rx_slot_size,
			 so->caplen,
			 pfq_mpsc_queue_mem(so));

		pr_devel("[PFQ|%d] Tx queue: len=%zu slot_size=%zu maxlen=%d, mem=%zu bytes\n",
			 so->id,
			 so->tx_queue_len,
			 so->tx_slot_size,
			 global->xmit_slot_size,
			 pfq_spsc_queue_mem(so));

		pr_devel("[PFQ|%d] Tx async queues: len=%zu slot_size=%zu maxlen=%d, mem=%zu bytes (%d queues)\n",
			 so->id,
			 so->tx_queue_len,
			 so->tx_slot_size,
			 global->xmit_slot_size,
			 pfq_spsc_queue_mem(so) * Q_MAX_TX_QUEUES, Q_MAX_TX_QUEUES);
	}

	return 0;
}


int
pfq_shared_queue_unmap(struct pfq_sock *so)
{
	if (atomic_long_read(&so->shmem.addr)) {
		atomic_long_set(&so->shmem.addr,0);
		// FIXME
		pfq_shared_memory_free(&so->shmem);
	}

	pr_devel("[PFQ|%d] Rx/Tx shared queues unmapped.\n", so->id);
	return 0;
}
