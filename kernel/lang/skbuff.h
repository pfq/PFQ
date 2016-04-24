/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PFQ_LANG_SKBUFF_H
#define PFQ_LANG_SKBUFF_H

#include <linux/ip.h>
#include <linux/ipv6.h>
#include <linux/skbuff.h>
#include <lang/monad.h>



static inline int
next_ip_offset(struct sk_buff const *skb, int offset, int tproto, int *proto)
{
	(void)skb;

	switch(tproto)
	{
	case IPPROTO_IPIP: {
		*proto = IPPROTO_IP;
		return offset;
	}
	case IPPROTO_IPV6: {
		*proto = IPPROTO_IPV6;
		return offset;
	}
	}

	return -1;
}


static inline int
skb_next_ip_offset(struct sk_buff *skb, int offset, int *proto)
{
	switch(*proto)
	{
	case IPPROTO_NONE: {

		if (eth_hdr(PFQ_SKB(skb))->h_proto == __constant_htons(ETH_P_IP))
		{
			*proto = IPPROTO_IP;
			return skb->mac_len;
		}

		if (eth_hdr(PFQ_SKB(skb))->h_proto == __constant_htons(ETH_P_IPV6))
		{
			*proto = IPPROTO_IPV6;
			return skb->mac_len;
		}

		return -1;

	} break;
	case IPPROTO_IP: {

		struct iphdr _iph;
		const struct iphdr *ip;

		ip = skb_header_pointer(PFQ_SKB(skb), offset, sizeof(_iph), &_iph);
		if (ip == NULL)
			return -1;

                return next_ip_offset(PFQ_SKB(skb), offset + (ip->ihl<<2), ip->protocol, proto);

	} break;
	case IPPROTO_IPV6: {

		struct ipv6hdr _ip6h;
		const struct ipv6hdr *ip6;

		ip6 = skb_header_pointer(PFQ_SKB(skb), offset, sizeof(_ip6h), &_ip6h);
		if (ip6 == NULL)
			return -1;

                return next_ip_offset(PFQ_SKB(skb), offset + sizeof(struct ipv6hdr), ip6->nexthdr, proto);

	} break;
	}

	return -1;
}


static inline const void *
skb_generic_ip_header_pointer(SkBuff skb, int ip_proto, int offset, int len, void *buffer)
{
	int ipoff = PFQ_CB(skb)->monad->ipoff;

	if (unlikely(ipoff < 0))
		return NULL;

	if (PFQ_CB(skb)->monad->ipproto == IPPROTO_NONE)
	{
		int n = 0;
		do
		{
			ipoff = skb_next_ip_offset(PFQ_SKB(skb), ipoff, &PFQ_CB(skb)->monad->ipproto);
			if (ipoff < 0) {
				PFQ_CB(skb)->monad->ipproto = IPPROTO_NONE;
				PFQ_CB(skb)->monad->ipoff = -1;
				return NULL;
			}
		}
		while (n++ < PFQ_CB(skb)->monad->shift);

		PFQ_CB(skb)->monad->ipoff = ipoff;
	}

	if (PFQ_CB(skb)->monad->ipproto != ip_proto)
		return NULL;

	return skb_header_pointer(PFQ_SKB(skb), PFQ_CB(skb)->monad->ipoff + offset, len, buffer);
}

#define skb_ip_header_pointer(skb, offset, len, buffer) skb_generic_ip_header_pointer(skb, IPPROTO_IP, offset, len, buffer)
#define skb_ip6_header_pointer(skb, offset, len, buffer) skb_generic_ip_header_pointer(skb, IPPROTO_IPV6, offset, len, buffer)

static inline int
skb_ip_protocol(SkBuff skb)
{
	if (unlikely(PFQ_CB(skb)->monad->ipoff < 0))
		return IPPROTO_NONE;

	if (PFQ_CB(skb)->monad->ipproto == IPPROTO_NONE)
	{
		skb_ip_header_pointer(skb, 0, 0, NULL);
	}

	return PFQ_CB(skb)->monad->ipproto;
}



#endif /* PFQ_LANG_SKBUFF_H */
