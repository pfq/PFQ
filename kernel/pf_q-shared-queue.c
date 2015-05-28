/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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

#include <pragma/diagnostic_push>

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/vmalloc.h>
#include <linux/printk.h>
#include <linux/kthread.h>
#include <linux/mm.h>
#include <linux/pf_q.h>

#include <pragma/diagnostic_pop>

#include <pf_q-shared-queue.h>
#include <pf_q-shmem.h>
#include <pf_q-bitops.h>
#include <pf_q-module.h>
#include <pf_q-sock.h>
#include <pf_q-global.h>
#include <pf_q-memory.h>
#include <pf_q-GC.h>


static inline
void *pfq_skb_copy_from_linear_data(const struct sk_buff *skb, void *to, size_t len)
{
	if (len < 64 && (len + skb_tailroom(skb) >= 64))
		return memcpy(to, skb->data, 64);
	return memcpy(to, skb->data, len);
}


static inline
char *mpsc_slot_ptr(struct pfq_rx_opt *ro, struct pfq_rx_queue *qd, size_t qindex, size_t slot)
{
	return (char *)(ro->base_addr) + (ro->queue_size * (qindex & 1) + slot) * ro->slot_size;
}


size_t pfq_mpsc_enqueue_batch(struct pfq_rx_opt *ro,
			      struct pfq_skbuff_batch *skbs,
			      unsigned long long mask,
			      int burst_len,
			      pfq_gid_t gid)
{
	struct pfq_rx_queue *rx_queue = pfq_get_rx_queue(ro);
	int data, qlen, qindex;
	struct sk_buff *skb;

	size_t n, sent = 0;
	char *this_slot;

	if (unlikely(rx_queue == NULL))
		return 0;

	data = atomic_read((atomic_t *)&rx_queue->data);

	if (Q_SHARED_QUEUE_LEN(data) > ro->queue_size)
		return 0;

	data = atomic_add_return(burst_len, (atomic_t *)&rx_queue->data);

	qlen      = Q_SHARED_QUEUE_LEN(data) - burst_len;
	qindex    = Q_SHARED_QUEUE_INDEX(data);
	this_slot = mpsc_slot_ptr(ro, rx_queue, qindex, qlen);

	for_each_skbuff_bitmask(skbs, mask, skb, n)
	{
		volatile struct pfq_pkthdr *hdr;
		size_t bytes, slot_index;
		char *pkt;

		bytes = min_t(size_t, skb->len, ro->caplen);

		slot_index = qlen + sent;

		hdr = (struct pfq_pkthdr *)this_slot;
		pkt = (char *)(hdr+1);

		if (slot_index > ro->queue_size) {

			if (waitqueue_active(&ro->waitqueue)) {
#ifdef PFQ_USE_EXTENDED_PROC
				sparse_inc(&global_stats.wake);
#endif
				wake_up_interruptible(&ro->waitqueue);
			}

			return sent;
		}

		/* copy bytes of packet */

#ifdef PFQ_USE_SKB_LINEARIZE
		if (unlikely(skb_is_nonlinear(skb)))
#else
			if (skb_is_nonlinear(skb))
#endif
			{
				if (skb_copy_bits(skb, 0, pkt, bytes) != 0) {
					printk(KERN_WARNING "[PFQ] BUG! skb_copy_bits failed (bytes=%zu, skb_len=%d mac_len=%d)!\n",
					       bytes, skb->len, skb->mac_len);
					return 0;
				}
			}
			else {
				pfq_skb_copy_from_linear_data(skb, pkt, bytes);
			}

		/* copy state from pfq_cb annotation */

		hdr->data = PFQ_CB(skb)->monad->state;

		/* setup the header */

		if (ro->tstamp != 0) {
			struct timespec ts;
			skb_get_timestampns(skb, &ts);
			hdr->tstamp.tv.sec  = (uint32_t)ts.tv_sec;
			hdr->tstamp.tv.nsec = (uint32_t)ts.tv_nsec;
		}

		hdr->if_index = skb->dev->ifindex & 0xff;
		hdr->gid      = gid.value;
		hdr->len      = (uint16_t)skb->len;
		hdr->caplen   = (uint16_t)bytes;
		hdr->vlan.tci = skb->vlan_tci & ~VLAN_TAG_PRESENT;
		hdr->hw_queue = (uint8_t)(skb_get_rx_queue(skb) & 0xff);

		/* commit the slot (release semantic) */

		smp_wmb();

		hdr->commit = (uint8_t)qindex;

		if ((slot_index & 8191) == 0 &&
		    waitqueue_active(&ro->waitqueue)) {
#ifdef PFQ_USE_EXTENDED_PROC
			sparse_inc(&global_stats.wake);
#endif
			wake_up_interruptible(&ro->waitqueue);
		}

		sent++;

		this_slot += ro->slot_size;
	}

	return sent;
}


int
pfq_shared_queue_enable(struct pfq_sock *so, unsigned long user_addr)
{
	if (!so->shmem.addr) {

		struct pfq_shared_queue * queue;
		size_t n;

		/* alloc queue memory */

		if (user_addr) {
			if (pfq_hugepage_map(&so->shmem, user_addr, pfq_shared_memory_size(so)) < 0)
				return -ENOMEM;
		}
		else {
			if (pfq_shared_memory_alloc(&so->shmem, pfq_shared_memory_size(so)) < 0)
				return -ENOMEM;
		}

		/* so->mem_addr and so->mem_size are set now */

		/* initialize queues headers */

		queue = (struct pfq_shared_queue *)so->shmem.addr;

		/* initialize rx queue header */

		queue->rx.data      = (1L << 24);
		queue->rx.size      = so->rx_opt.queue_size;
		queue->rx.slot_size = so->rx_opt.slot_size;

		for(n = 0; n < Q_MAX_TX_QUEUES; n++)
		{
			queue->tx[n].prod      = 0;
			queue->tx[n].cons      = 0;
			queue->tx[n].size      = pfq_queue_spsc_mem(so)/2;
			queue->tx[n].ptr       = NULL;
			queue->tx[n].index     = -1;

			so->tx_opt.queue[n].base_addr = so->shmem.addr + sizeof(struct pfq_shared_queue)
				+ pfq_queue_mpsc_mem(so) + pfq_queue_spsc_mem(so) * n;
		}

		/* update the queues base_addr */

		so->rx_opt.base_addr = so->shmem.addr + sizeof(struct pfq_shared_queue);

		/* commit both the queues */

		smp_wmb();

		atomic_long_set(&so->rx_opt.queue_hdr, (long)&queue->rx);

		for(n = 0; n < Q_MAX_TX_QUEUES; n++)
		{
			atomic_long_set(&so->tx_opt.queue[n].queue_hdr, (long)&queue->tx[n]);
		}

		pr_devel("[PFQ|%d] Rx queue: len=%zu slot_size=%zu caplen=%zu, mem=%zu bytes\n",
			 so->id.value,
			 so->rx_opt.queue_size,
			 so->rx_opt.slot_size,
			 so->rx_opt.caplen,
			 pfq_queue_mpsc_mem(so));

		pr_devel("[PFQ|%d] Tx queue: len=%zu slot_size=%zu maxlen=%d, mem=%zu bytes (%d queues)\n",
			 so->id.value,
			 so->tx_opt.queue_size,
			 so->tx_opt.slot_size,
			 max_len,
			 pfq_queue_spsc_mem(so) * Q_MAX_TX_QUEUES, Q_MAX_TX_QUEUES);
	}

	return 0;
}


int
pfq_shared_queue_disable(struct pfq_sock *so)
{
	size_t n;

	if (so->shmem.addr) {

		atomic_long_set(&so->rx_opt.queue_hdr, 0);

		for(n = 0; n < Q_MAX_TX_QUEUES; n++)
		{
			atomic_long_set(&so->tx_opt.queue[n].queue_hdr, 0);
		}

		msleep(Q_GRACE_PERIOD);

		pfq_shared_memory_free(&so->shmem);

		so->shmem.addr = NULL;
		so->shmem.size = 0;

		pr_devel("[PFQ|%d] Tx/Rx queues disabled.\n", so->id.value);
	}

	return 0;
}
