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


#define PFQ_CB(addr)    ((struct pfq_cb *)(((struct sk_buff *)(addr))->cb))


struct pfq_cb
{
	bool	direct;
	u8	pool;
};


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

