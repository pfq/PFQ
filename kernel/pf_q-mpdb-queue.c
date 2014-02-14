/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#include <pf_q-mpdb-queue.h>

#include <linux/pf_q-fun.h>

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


inline
char *mpdb_slot_ptr(struct pfq_rx_opt *ro, struct pfq_rx_queue_hdr *qd, int index, int slot)
{
	return (char *)(ro->base_addr) + ( (index&1 ? ro->size : 0 ) + slot) * ro->slot_size;
}


size_t mpdb_enqueue_batch(struct pfq_rx_opt *ro, unsigned long bitqueue, int burst_len, struct pfq_queue_skb *skbs, int gid)
{
	struct pfq_rx_queue_hdr *rx = ro->queue_info;
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

	queue_for_each_bitmask(skb, bitqueue, n, skbs)
	{
		unsigned int bytes = likely (skb->len > (int)ro->offset) ? min((int)skb->len - (int)ro->offset, (int)ro->caplen) : 0;

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
				if (skb_copy_bits(skb, (int)ro->offset, pkt, bytes) != 0)
				{
					printk(KERN_WARNING "[PFQ] BUG! skb_copy_bits failed (bytes=%u, skb_len=%d mac_len=%d q_offset=%zu)!\n",
							    bytes, skb->len, skb->mac_len, ro->offset);
					return 0;
				}
			}
			else
			{
				pfq_memcpy(pkt, skb->data + ro->offset, bytes);
			}
		}

                /* copy state from pfq_annotation */

                hdr->data = pfq_skb_annotation(skb)->state;

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

