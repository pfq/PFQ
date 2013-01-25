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

#include <linux/pf_q-fun.h>

funret_t
steering_mac_addr(const struct sk_buff *skb, const void *state)
{
        uint16_t * a = (uint16_t *)eth_hdr(skb);
	return steering(Q_CLASS_DEFAULT, a[0] ^ a[1] ^ a[2] ^ a[3] ^ a[4] ^ a[5] );		
}


funret_t
steering_vlan_untagged(const struct sk_buff *skb, const void *state)
{
	return steering(Q_CLASS_DEFAULT, skb->vlan_tci == 0);
}


funret_t
steering_vlan_id(const struct sk_buff *skb, const void *state)
{
 	return steering(Q_CLASS_DEFAULT, skb->vlan_tci & VLAN_VID_MASK);
}


funret_t
steering_ipv4_addr(const struct sk_buff *skb, const void *state)
{       
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP)) 
	{ 
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
 			return none();

        	return steering(Q_CLASS_DEFAULT, ip->saddr ^ ip->daddr);
	}

	return none();
}


funret_t
steering_flow(const struct sk_buff *skb, const void *state)
{       
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP)) 
	{ 
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
 			return none();

		if (ip->protocol != IPPROTO_UDP &&
		    ip->protocol != IPPROTO_TCP) 
			return none();
		
		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp == NULL) 
			return none();
		
        	return steering(Q_CLASS_DEFAULT, ip->saddr ^ ip->daddr ^ udp->source ^ udp->dest);
	}

	return none();
}


funret_t
steering_ipv6_addr(const struct sk_buff *skb, const void *state)
{       
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IPV6)) 
	{ 
		struct ipv6hdr _ip6h;
    		const struct ipv6hdr *ip6;

		ip6 = skb_header_pointer(skb, skb->mac_len, sizeof(_ip6h), &_ip6h);
 		if (ip6 == NULL)
 			return none();

		return steering(Q_CLASS_DEFAULT,
			ip6->saddr.in6_u.u6_addr32[0] ^
			ip6->saddr.in6_u.u6_addr32[1] ^
			ip6->saddr.in6_u.u6_addr32[2] ^
			ip6->saddr.in6_u.u6_addr32[3] ^
			ip6->daddr.in6_u.u6_addr32[0] ^
			ip6->daddr.in6_u.u6_addr32[1] ^
			ip6->daddr.in6_u.u6_addr32[2] ^
			ip6->daddr.in6_u.u6_addr32[3] );
	}

	return none();
}


funret_t
steering_legacy(const struct sk_buff *skb, const void *state)
{
        return to_kernel(broadcast(Q_CLASS_ANY));
}


funret_t
steering_sink(const struct sk_buff *skb, const void *state)
{
        struct sk_buff * mskb = (struct sk_buff *)skb;
        kfree_skb(mskb);

        return stolen();
}


struct sk_function_descr default_functions[] = {
        { "steer-vlan-untagged", steering_vlan_untagged },
	{ "steer-mac-addr",      steering_mac_addr   },
        { "steer-vlan-id",       steering_vlan_id    },
        { "steer-ipv4-addr",     steering_ipv4_addr  },
        { "steer-ipv6-addr",     steering_ipv6_addr  },
        { "steer-flow",          steering_flow       },
        { "steer-legacy",        steering_legacy     },
        { "steer-sink",          steering_sink       },
	{ NULL, NULL}};

