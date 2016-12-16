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


/* Make sure a field is enclosed inside headers_start/headers_end section */
#define CHECK_SKB_FIELD(field) \
        BUILD_BUG_ON(offsetof(struct sk_buff, field) <          \
                     offsetof(struct sk_buff, headers_start));  \
        BUILD_BUG_ON(offsetof(struct sk_buff, field) >          \
                     offsetof(struct sk_buff, headers_end));    \


static void __copy_skb_header(struct sk_buff *new, const struct sk_buff *old)
{
        new->tstamp             = old->tstamp;
        /* We do not copy old->sk */
        new->dev                = old->dev;
        memcpy(new->cb, old->cb, sizeof(old->cb));
        skb_dst_copy(new, old);

#if 0
#ifdef CONFIG_XFRM
        new->sp                 = secpath_get(old->sp);
#endif
#endif
	// __nf_copy(new, old, false);

        /* Note : this field could be in headers_start/headers_end section
         * It is not yet because we do not want to have a 16 bit hole
         */
        new->queue_mapping = old->queue_mapping;

        memcpy(&new->headers_start, &old->headers_start,
               offsetof(struct sk_buff, headers_end) -
               offsetof(struct sk_buff, headers_start));

        CHECK_SKB_FIELD(protocol);
        CHECK_SKB_FIELD(csum);
        CHECK_SKB_FIELD(hash);
        CHECK_SKB_FIELD(priority);
        CHECK_SKB_FIELD(skb_iif);
        CHECK_SKB_FIELD(vlan_proto);
        CHECK_SKB_FIELD(vlan_tci);
        CHECK_SKB_FIELD(transport_header);
        CHECK_SKB_FIELD(network_header);
        CHECK_SKB_FIELD(mac_header);
        CHECK_SKB_FIELD(inner_protocol);
        CHECK_SKB_FIELD(inner_transport_header);
        CHECK_SKB_FIELD(inner_network_header);
        CHECK_SKB_FIELD(inner_mac_header);
        CHECK_SKB_FIELD(mark);
#ifdef CONFIG_NETWORK_SECMARK
        CHECK_SKB_FIELD(secmark);
#endif
#ifdef CONFIG_NET_RX_BUSY_POLL
        CHECK_SKB_FIELD(napi_id);
#endif
#ifdef CONFIG_XPS
        CHECK_SKB_FIELD(sender_cpu);
#endif
#ifdef CONFIG_NET_SCHED
        CHECK_SKB_FIELD(tc_index);
#ifdef CONFIG_NET_CLS_ACT
        CHECK_SKB_FIELD(tc_verd);
#endif
#endif

}

static void copy_skb_header(struct sk_buff *new, const struct sk_buff *old)
{
        __copy_skb_header(new, old);

        skb_shinfo(new)->gso_size = skb_shinfo(old)->gso_size;
        skb_shinfo(new)->gso_segs = skb_shinfo(old)->gso_segs;
        skb_shinfo(new)->gso_type = skb_shinfo(old)->gso_type;
}

static inline int skb_alloc_rx_flag(const struct sk_buff *skb)
{
        if (skb_pfmemalloc(skb))
                return SKB_ALLOC_RX;
        return 0;
}


static struct sk_buff *
pfq_skb_copy(const struct sk_buff *skb, gfp_t gfp_mask)
{
	int headerlen = skb_headroom(skb);
	unsigned int size = skb_end_offset(skb) + skb->data_len;
	struct sk_buff *nskb = __alloc_skb(size, gfp_mask, skb_alloc_rx_flag(skb), NUMA_NO_NODE);
	if (!nskb)
		return NULL;

	/* Set the data pointer */
	skb_reserve(nskb, headerlen);

	/* Set the tail pointer and length */
	skb_put(nskb, skb->len);

	if (skb_copy_bits(skb, -headerlen, nskb->head, headerlen + skb->len)) {
		printk(KERN_INFO "[PFQ] BUG: skb_copy_bits failed!\n");
		kfree(nskb);
		return NULL;
	}

	copy_skb_header(nskb, skb);
	return nskb;
}


static inline
struct sk_buff *
skb_copy_for_kernel(struct sk_buff *skb, gfp_t pri)
{
	if (skb->nf_trace)
		return pfq_skb_copy(skb, pri);
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

