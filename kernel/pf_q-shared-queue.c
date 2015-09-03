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
char *mpsc_slot_ptr(struct pfq_sock_opt *opt, struct pfq_rx_queue *qd, size_t qindex, size_t slot)
{
	return (char *)(opt->rxq.base_addr) + (opt->rx_queue_size * (qindex & 1) + slot) * opt->rx_slot_size;
}


size_t pfq_mpsc_enqueue_batch(struct pfq_sock_opt *opt,
			      struct pfq_skbuff_queue __GC *skbs,
			      unsigned long long mask,
			      int burst_len,
			      pfq_gid_t gid)
{
	struct pfq_rx_queue *rx_queue = pfq_get_rx_queue(opt);
	int data, qlen, qindex;
	struct sk_buff __GC *skb;

	size_t n, sent = 0;
	char *this_slot;

	if (unlikely(rx_queue == NULL))
		return 0;

	data = atomic_read((atomic_t *)&rx_queue->data);

	if (Q_SHARED_QUEUE_LEN(data) > opt->rx_queue_size)
		return 0;

	data = atomic_add_return(burst_len, (atomic_t *)&rx_queue->data);

	qlen      = Q_SHARED_QUEUE_LEN(data) - burst_len;
	qindex    = Q_SHARED_QUEUE_INDEX(data);
	this_slot = mpsc_slot_ptr(opt, rx_queue, qindex, qlen);

	for_each_skbuff_bitmask((struct pfq_skbuff_queue_GC __force *)skbs, mask, skb, n)
	{
		volatile struct pfq_pkthdr *hdr;
		size_t bytes, slot_index;
		char *pkt;

		bytes = min_t(size_t, skb->len, opt->caplen);

		slot_index = qlen + sent;

		hdr = (struct pfq_pkthdr *)this_slot;
		pkt = (char *)(hdr+1);

		if (slot_index > opt->rx_queue_size) {

			if (waitqueue_active(&opt->waitqueue)) {
				SPARSE_INC(&global_stats.wake);
				wake_up_interruptible(&opt->waitqueue);
			}

			return sent;
		}

		/* copy bytes of packet */

#ifdef PFQ_USE_SKB_LINEARIZE
		if (unlikely(skb_is_nonlinear(PFQ_SKB(skb))))
#else
		if (skb_is_nonlinear(PFQ_SKB(skb)))
#endif
		{
			if (skb_copy_bits(PFQ_SKB(skb), 0, pkt, bytes) != 0) {
				printk(KERN_WARNING "[PFQ] BUG! skb_copy_bits failed (bytes=%zu, skb_len=%d mac_len=%d)!\n",
				       bytes, skb->len, skb->mac_len);
				return 0;
			}
		}
		else {
			pfq_skb_copy_from_linear_data(PFQ_SKB(skb), pkt, bytes);
		}

		/* copy state from pfq_cb annotation */

		hdr->data = PFQ_CB(skb)->monad->state;

		/* setup the header */

		if (opt->tstamp != 0) {
			struct timespec ts;
			skb_get_timestampns(PFQ_SKB(skb), &ts);
			hdr->tstamp.tv.sec  = (uint32_t)ts.tv_sec;
			hdr->tstamp.tv.nsec = (uint32_t)ts.tv_nsec;
		}

		hdr->if_index = skb->dev->ifindex;
		hdr->gid      = (__force int)gid;
		hdr->len      = (uint16_t)skb->len;
		hdr->caplen   = (uint16_t)bytes;
		hdr->vlan.tci = skb->vlan_tci & ~VLAN_TAG_PRESENT;
		hdr->queue    = skb_rx_queue_recorded(PFQ_SKB(skb)) ? (uint8_t)(skb_get_rx_queue(PFQ_SKB(skb)) & 0xff) : 0;

		/* commit the slot (release semantic) */

		smp_wmb();

		hdr->commit = (uint8_t)qindex;

		if ((slot_index & 8191) == 0 &&
		    waitqueue_active(&opt->waitqueue)) {
			SPARSE_INC(&global_stats.wake);
			wake_up_interruptible(&opt->waitqueue);
		}

		sent++;

		this_slot += opt->rx_slot_size;
	}

	return sent;
}


int
pfq_shared_queue_enable(struct pfq_sock *so, unsigned long user_addr)
{
	if (!so->shmem.addr) {

		struct pfq_shared_queue * queue;
		size_t n;
                int i;

		/* alloc queue memory */

		if (user_addr) {
			if (pfq_hugepage_map(&so->shmem, user_addr, pfq_shared_memory_size(so)) < 0)
				return -ENOMEM;
		}
		else {
			if (pfq_shared_memory_alloc(&so->shmem, pfq_shared_memory_size(so)) < 0)
				return -ENOMEM;
		}

		/* initialize queues headers */

		queue = (struct pfq_shared_queue *)so->shmem.addr;

		/* initialize Rx queues */

		queue->rx.data      = 0;
		queue->rx.size      = so->opt.rx_queue_size;
		queue->rx.slot_size = so->opt.rx_slot_size;

		/* reset Rx slots */

		for(i = 0; i < 2; i++)
		{
			char * raw = so->shmem.addr + sizeof(struct pfq_shared_queue) + i * queue->rx.size;
			char * end = raw + queue->rx.size;
			const int rst = !i;

			for(;raw < end; raw += queue->rx.slot_size)
				((struct pfq_pkthdr *)raw)->commit = rst;
		}

		/* initialize TX queues */

		for(n = 0; n < Q_MAX_TX_QUEUES; n++)
		{
			queue->tx[n].prod      = 0;
			queue->tx[n].cons      = 0;
			queue->tx[n].size      = pfq_queue_spsc_mem(so)/2;
			queue->tx[n].ptr       = NULL;
			queue->tx[n].index     = -1;

			so->opt.txq[n].base_addr = so->shmem.addr + sizeof(struct pfq_shared_queue)
				+ pfq_queue_mpsc_mem(so)
				+ pfq_queue_spsc_mem(so) * n;
		}

		/* update the queues base_addr */

		so->opt.rxq.base_addr = so->shmem.addr + sizeof(struct pfq_shared_queue);

		/* commit both the queues */

		smp_wmb();

		atomic_long_set(&so->opt.rxq.addr, (long)&queue->rx);

		for(n = 0; n < Q_MAX_TX_QUEUES; n++)
		{
			atomic_long_set(&so->opt.txq[n].addr, (long)&queue->tx[n]);
		}

		pr_devel("[PFQ|%d] Rx queue: len=%zu slot_size=%zu caplen=%zu, mem=%zu bytes\n",
			 so->id,
			 so->opt.rx_queue_size,
			 so->opt.rx_slot_size,
			 so->opt.caplen,
			 pfq_queue_mpsc_mem(so));

		pr_devel("[PFQ|%d] Tx queue: len=%zu slot_size=%zu maxlen=%d, mem=%zu bytes (%d queues)\n",
			 so->id,
			 so->opt.tx_queue_size,
			 so->opt.tx_slot_size,
			 xmit_slot_size,
			 pfq_queue_spsc_mem(so) * Q_MAX_TX_QUEUES, Q_MAX_TX_QUEUES);
	}

	return 0;
}


int
pfq_shared_queue_disable(struct pfq_sock *so)
{
	size_t n;

	if (so->shmem.addr) {

		atomic_long_set(&so->opt.rxq.addr, 0);

		for(n = 0; n < Q_MAX_TX_QUEUES; n++)
		{
			atomic_long_set(&so->opt.txq[n].addr, 0);
		}

		msleep(Q_GRACE_PERIOD);

		pfq_shared_memory_free(&so->shmem);

		so->shmem.addr = NULL;
		so->shmem.size = 0;

		pr_devel("[PFQ|%d] Tx/Rx queues disabled.\n", so->id);
	}

	return 0;
}
