/***************************************************************
 *                                                
 * (C) 2011-12 Nicola Bonelli <nicola.bonelli@cnit.it>   
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

#include <linux/skbuff.h>
#include <linux/ip.h>
#include <linux/ipv6.h>
#include <linux/if_ether.h>
#include <linux/if_vlan.h>

#include <pf_q-steer-fun.h>
#include <pf_q-steer.h>

steering_ret_t
steering_mac_addr(const struct sk_buff *skb, const void *state)
{
        uint16_t * a = (uint16_t *)eth_hdr(skb);
	return steering_data( a[0] ^ a[1] ^ a[2] ^ a[3] ^ a[4] ^ a[5] );		
}


steering_ret_t
steering_vlan_untag(const struct sk_buff *skb, const void *state)
{
	return steering_data(skb->vlan_tci == 0);
}


steering_ret_t
steering_vlan_id(const struct sk_buff *skb, const void *state)
{
 	return steering_data(skb->vlan_tci & VLAN_VID_MASK);
}


steering_ret_t
steering_ipv4_addr(const struct sk_buff *skb, const void *state)
{       
	if (eth_hdr(skb)->h_proto == htons(ETH_P_IP)) 
	{ 
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
 			return steering_none();

        	return steering_data( ip->saddr ^ ip->daddr );
	}

	return steering_none();
}


steering_ret_t
steering_ipv6_addr(const struct sk_buff *skb, const void *state)
{       
	if (eth_hdr(skb)->h_proto == htons(ETH_P_IPV6)) 
	{ 
		struct ipv6hdr _ip6h;
    		const struct ipv6hdr *ip6;

		ip6 = skb_header_pointer(skb, skb->mac_len, sizeof(_ip6h), &_ip6h);
 		if (ip6 == NULL)
 			return steering_none();

		return steering_data(
			ip6->saddr.in6_u.u6_addr32[0] ^
			ip6->saddr.in6_u.u6_addr32[1] ^
			ip6->saddr.in6_u.u6_addr32[2] ^
			ip6->saddr.in6_u.u6_addr32[3] ^
			ip6->daddr.in6_u.u6_addr32[0] ^
			ip6->daddr.in6_u.u6_addr32[1] ^
			ip6->daddr.in6_u.u6_addr32[2] ^
			ip6->daddr.in6_u.u6_addr32[3] );
	}

	return steering_none();
}


struct factory_hook pfq_steering_hooks[] = {
	{ "steer-mac-addr",   steering_mac_addr 	},
        { "steer-vlan-untag", steering_vlan_untag 	},
        { "steer-vlan-id",    steering_vlan_id		},
        { "steer-ipv4-addr",  steering_ipv4_addr 	},
        { "steer-ipv6-addr",  steering_ipv6_addr 	},
	{ NULL, NULL}};

