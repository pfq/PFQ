/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#ifndef _PF_Q_PREDICATE_H
#define _PF_Q_PREDICATE_H

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/skbuff.h>

#include <linux/ip.h>
#include <linux/ipv6.h>
#include <linux/udp.h>
#include <linux/tcp.h>
#include <linux/icmp.h>
#include <linux/if_vlan.h>

static inline bool
is_ip(struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip)
                        return true;
        }

        return false;
}

static inline bool
is_ipv6(struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IPV6))
	{
		struct ipv6hdr _ip6h;
    		const struct ipv6hdr *ip6;

		ip6 = skb_header_pointer(skb, skb->mac_len, sizeof(_ip6h), &_ip6h);
 		if (ip6)
                        return true;
	}

        return false;
}

static inline bool
is_udp(struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		if (ip->protocol != IPPROTO_UDP)
                        return false;

		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp)
		        return true;
	}

        return false;
}

static inline bool
is_tcp(struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct tcphdr _tcp;
		const struct tcphdr *tcp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		if (ip->protocol != IPPROTO_TCP)
                        return false;

		tcp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_tcp), &_tcp);
		if (tcp)
		        return true;
	}

        return false;
}

static inline bool
is_icmp(struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct icmphdr _icmp;
		const struct icmphdr *icmp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		if (ip->protocol != IPPROTO_ICMP)
                        return false;

		icmp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_icmp), &_icmp);
		if (icmp)
		        return true;
	}

        return false;
}

static inline bool
has_flow(struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		if (ip->protocol != IPPROTO_UDP &&
		    ip->protocol != IPPROTO_TCP)
                        return false;

		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp)
		        return true;
	}

        return false;
}

static inline bool
has_vlan(struct sk_buff const *skb)
{
        if ((skb->vlan_tci & VLAN_VID_MASK) == 0)
                return false;
        else
                return true;
}

#endif /* _PF_Q_PREDICATE_H */
