/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#include <linux/kernel.h>
#include <linux/module.h>

#include <linux/vmalloc.h>
#include <linux/printk.h>
#include <linux/mm.h>
#include <linux/pf_q.h>

#include <pf_q-bounded-queue.h>
#include <pf_q-mpdb-queue.h>
#include <pf_q-module.h>
#include <pf_q-sock.h>
#include <pf_q-global.h>
#include <pf_q-memory.h>
#include <pf_q-GC.h>

static inline
void *pfq_memcpy(void *to, const void *from, size_t len)
{
	switch(len)
	{
		case 64 : return __builtin_memcpy(to, from, 64);
		case 128: return __builtin_memcpy(to, from, 128);
		case 256: return __builtin_memcpy(to, from, 256);
		case 512: return __builtin_memcpy(to, from, 512);
		default:  return memcpy(to, from, len);
	}
}


static inline
char *mpdb_slot_ptr(struct pfq_rx_opt *ro, struct pfq_rx_queue_hdr *qd, int index, int slot)
{
	return (char *)(ro->base_addr) + ( (index&1 ? ro->size : 0 ) + slot) * ro->slot_size;
}


size_t pfq_mpdb_enqueue_batch(struct pfq_rx_opt *ro,
		              struct gc_queue_buff *queue,
		              unsigned long long skbs_mask,
		              int burst_len,
		              int gid)
{
	struct pfq_rx_queue_hdr *rx = ro->queue_ptr;

	int data, q_len, q_index;
	struct sk_buff *skb;
	size_t sent = 0;
	unsigned int n;
	char *this_slot;

	data = atomic_read((atomic_t *)&rx->data);

        if (unlikely(MPDB_QUEUE_LEN(data) > ro->size))
		return 0;

	data = atomic_add_return(burst_len, (atomic_t *)&rx->data);

	q_len     = MPDB_QUEUE_LEN(data) - burst_len;
	q_index   = MPDB_QUEUE_INDEX(data);
        this_slot = mpdb_slot_ptr(ro, rx, q_index, q_len);

	GC_queue_for_each_skb_bitmask(queue, skb, skbs_mask, n)
	{
		unsigned int bytes = min((int)skb->len, (int)ro->caplen);

		size_t slot_index = q_len + sent;

		volatile struct pfq_pkt_hdr *hdr = (struct pfq_pkt_hdr *)this_slot;
		char                    *pkt = (char *)(hdr+1);

		struct timespec ts;

		if (unlikely(slot_index > ro->size))
		{
			if ( rx->poll_wait ) {
				wake_up_interruptible(&ro->waitqueue);
			}
			return sent;
		}

		/* copy bytes of packet */

		if (likely(bytes))
		{
			/* packets might still come from a regular sniffer */

			if (
#ifdef PFQ_USE_SKB_LINEARIZE
			   	unlikely(skb_is_nonlinear(skb))
#else
		           	skb_is_nonlinear(skb)
#endif
			   )
		      	{
				if (skb_copy_bits(skb, 0, pkt, bytes) != 0)
				{
					printk(KERN_WARNING "[PFQ] BUG! skb_copy_bits failed (bytes=%u, skb_len=%d mac_len=%d)!\n",
							    bytes, skb->len, skb->mac_len);
					return 0;
				}
			}
			else
			{
				pfq_memcpy(pkt, skb->data, bytes);
			}
		}

                /* copy state from pfq_cb (annotation) */

                hdr->data = PFQ_CB(skb)->state;

		/* setup the header */

		if (ro->tstamp != 0)
		{
			skb_get_timestampns(skb, &ts);
			hdr->tstamp.tv.sec  = (uint32_t)ts.tv_sec;
			hdr->tstamp.tv.nsec = (uint32_t)ts.tv_nsec;
		}

		hdr->if_index    = skb->dev->ifindex & 0xff;
		hdr->gid         = gid;

		hdr->len         = (uint16_t)skb->len;
		hdr->caplen 	 = (uint16_t)bytes;
		hdr->un.vlan_tci = skb->vlan_tci & ~VLAN_TAG_PRESENT;
		hdr->hw_queue    = (uint8_t)(skb_get_rx_queue(skb) & 0xff);

		/* commit the slot (release semantic) */

		smp_wmb();

		hdr->commit = (uint8_t)q_index;

		if (unlikely((slot_index & 16383) == 0) &&
			     (slot_index >= (ro->size >> 1)) &&
			     rx->poll_wait)
		{
		        wake_up_interruptible(&ro->waitqueue);
		}

		sent++;

		this_slot += ro->slot_size;
	}

	return sent;
}


int pfq_mpdb_shared_queue_alloc(struct pfq_sock *so, size_t queue_mem)
{
        /* calculate the size of the buffer */

	size_t tm = PAGE_ALIGN(queue_mem);
        size_t tot_mem;

	/* align bufflen to page size */

	size_t num_pages = tm / PAGE_SIZE; void *addr;

	num_pages += (num_pages + (PAGE_SIZE-1)) & (PAGE_SIZE-1);
	tot_mem = num_pages*PAGE_SIZE;

	/* Memory is already zeroed */

        addr = vmalloc_user(tot_mem);
	if (addr == NULL)
	{
		printk(KERN_WARNING "[PFQ|%d] pfq_queue_alloc: out of memory (vmalloc %zu bytes)!", so->id, tot_mem);
		return -ENOMEM;
	}

        so->mem_addr = addr;
        so->mem_size = tot_mem;

	pr_devel("[PFQ|%d] pfq_queue_alloc: caplen:%zu maxlen:%zu memory:%zu bytes.\n", so->id, so->rx_opt.caplen, so->tx_opt.maxlen, tot_mem);
	return 0;
}


void pfq_mpdb_shared_queue_free(struct pfq_sock *so)
{
	if (so->mem_addr) {

		vfree(so->mem_addr);

		so->mem_addr = NULL;
		so->mem_size = 0;

		pr_devel("[PFQ|%d] queue freed.\n", so->id);
	}
}

