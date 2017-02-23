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

#ifndef PFQ_SKBUFF_H
#define PFQ_SKBUFF_H

#include <linux/skbuff.h>
#include <linux/kernel.h>


#define PFQ_CB(addr)    ((struct pfq_cb *)(((struct sk_buff *)(addr))->cb))

struct pfq_cb
{
	char pad[FIELD_SIZEOF(struct sk_buff, cb) - sizeof(void *)*3];
	void *	 head;
	void *   skb_orig;
	uint32_t id;
	u8	 pool;
};


static inline
void pfq_printk_skb(const char *msg, const struct sk_buff *skb)
{
	struct skb_shared_info *shinfo =  skb_shinfo(skb);

	printk(KERN_INFO "%s: skb@%p -> peeked:%d [pool=%d id=%u head=%p skb_orig=%p] len=%d data_len=%d mac_len=%d hdr_len=%d truesize=%d {head=%p data=%p tail=%u end=%u mac_h=%d net_h=%d trans_h=%d users=%d} >> [nfrags=%d tx_flags=%x gso_size=%d data_ref=%d darg=%p frag_list=%p]\n"
			, msg
			, (void *)skb
			, skb->peeked
			, skb->peeked ? PFQ_CB(skb)->pool	: -1
			, skb->peeked ? PFQ_CB(skb)->id		: 0
			, skb->peeked ? PFQ_CB(skb)->head	: 0
			, skb->peeked ? PFQ_CB(skb)->skb_orig	: 0
			, skb->len
			, skb->data_len
			, skb->mac_len
			, skb->hdr_len
			, skb->truesize
			, skb->head
			, skb->data
			, skb->tail
			, skb->end
			, skb->mac_header
			, skb->network_header
			, skb->transport_header
			, atomic_read(&skb->users)
			, shinfo->nr_frags
			, shinfo->tx_flags
			, shinfo->gso_size
			, atomic_read(&shinfo->dataref)
			, shinfo->destructor_arg
			, shinfo->frag_list
			);
}


#ifdef NET_SKBUFF_DATA_USES_OFFSET
static inline
unsigned int pfq_skb_end_offset(const struct sk_buff *skb)
{
	return skb->end;
}
#else
static inline
unsigned int pfq_skb_end_offset(const struct sk_buff *skb)
{
	return skb->end - skb->head;
}
#endif



static inline
struct sk_buff *
skb_copy_for_kernel(struct sk_buff *skb, gfp_t pri)
{
	if (skb->nf_trace) {
		return skb_copy(skb, pri);
	}
	return skb;
}


static inline
struct sk_buff *
skb_clone_for_tx(struct sk_buff *skb, struct net_device *dev, gfp_t pri)
{
	if (likely(dev->priv_flags & IFF_TX_SKB_SHARING))
		return skb_get(skb);

	return skb_clone(skb, pri);
}


#endif /* PFQ_SKBUFF_H */

