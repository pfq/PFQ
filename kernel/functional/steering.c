/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola.bonelli@cnit.it>
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


#include <pf_q-module.h>


static struct sk_buff *
steering_link(arguments_t args, struct sk_buff *skb)
{
        uint32_t * w;

        w = (uint32_t *)eth_hdr(skb);

	return steering(skb, w[0] ^ w[1] ^ w[2]); // 3 * sizeof(uint32_t) = 12 bytes.
}


static struct sk_buff *
steering_vlan_id(arguments_t args, struct sk_buff *skb)
{
        if (skb->vlan_tci & VLAN_VID_MASK)
 	        return steering(skb, skb->vlan_tci & VLAN_VID_MASK);
        else
                return drop(skb);
}


static struct sk_buff *
steering_ip(arguments_t args, struct sk_buff *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return drop(skb);

        	return steering(skb, ip->saddr ^ ip->daddr);
	}

        return drop(skb);
}


static struct sk_buff *
steering_flow(arguments_t args, struct sk_buff *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return drop(skb);

		if (ip->protocol != IPPROTO_UDP &&
		    ip->protocol != IPPROTO_TCP)
                        return drop(skb);

		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp == NULL)
			return drop(skb);  /* broken */

        	return steering(skb, ip->saddr ^ ip->daddr ^ udp->source ^ udp->dest);
	}

        return drop(skb);
}


static struct sk_buff *
steering_ip6(arguments_t args, struct sk_buff *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IPV6))
	{
		struct ipv6hdr _ip6h;
    		const struct ipv6hdr *ip6;

		ip6 = skb_header_pointer(skb, skb->mac_len, sizeof(_ip6h), &_ip6h);
 		if (ip6 == NULL)
                        return drop(skb);

		return steering(skb,
			ip6->saddr.in6_u.u6_addr32[0] ^
			ip6->saddr.in6_u.u6_addr32[1] ^
			ip6->saddr.in6_u.u6_addr32[2] ^
			ip6->saddr.in6_u.u6_addr32[3] ^
			ip6->daddr.in6_u.u6_addr32[0] ^
			ip6->daddr.in6_u.u6_addr32[1] ^
			ip6->daddr.in6_u.u6_addr32[2] ^
			ip6->daddr.in6_u.u6_addr32[3] );
	}

        return drop(skb);
}


struct pfq_monadic_fun_descr steering_functions[] = {

	{ "steer_link",  FUN_ACTION, steering_link    },
        { "steer_vlan",  FUN_ACTION, steering_vlan_id },
        { "steer_ip",    FUN_ACTION, steering_ip      },
        { "steer_ip6",	 FUN_ACTION, steering_ip6     },
        { "steer_flow",  FUN_ACTION, steering_flow    },

        { NULL }};

