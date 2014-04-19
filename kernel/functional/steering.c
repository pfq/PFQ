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

#include <linux/kernel.h>
#include <linux/module.h>


#include <linux/pf_q-module.h>


static struct sk_buff *
steering_mac(argument_t a, struct sk_buff *skb)
{
        uint16_t * b;

        b = (uint16_t *)eth_hdr(skb);

	return steering(skb, b[0] ^ b[1] ^ b[2] ^ b[3] ^ b[4] ^ b[5] );
}


static struct sk_buff *
steering_vlan_id(argument_t a, struct sk_buff *skb)
{
        if (skb->vlan_tci & VLAN_VID_MASK)
 	        return steering(skb, skb->vlan_tci & VLAN_VID_MASK);
        else
                return drop(skb);
}


static struct sk_buff *
steering_ip(argument_t a, struct sk_buff *skb)
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
steering_flow(argument_t a, struct sk_buff *skb)
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
steering_ipv6(argument_t a, struct sk_buff *skb)
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

	{ "steer_mac",          steering_mac            },
        { "steer_vlan",      	steering_vlan_id        },
        { "steer_ip",           steering_ip             },
        { "steer_ipv6",         steering_ipv6           },
        { "steer_flow",         steering_flow           },

        { NULL, NULL}};

