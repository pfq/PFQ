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
id(context_t ctx, struct sk_buff *skb)
{
        return cont(skb);
}


static struct sk_buff *
filter_ip(context_t ctx, struct sk_buff *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip)
                {
                        return cont(skb);
                }
        }

	return drop(skb);
}


static struct sk_buff *
filter_ipv6(context_t ctx, struct sk_buff *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IPV6))
	{
		struct ipv6hdr _ip6h;
    		const struct ipv6hdr *ip6;

		ip6 = skb_header_pointer(skb, skb->mac_len, sizeof(_ip6h), &_ip6h);
 		if (ip6)
                        return cont(skb);
	}

        return drop(skb);
}


static struct sk_buff *
filter_udp(context_t ctx, struct sk_buff *skb)
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

		if (ip->protocol != IPPROTO_UDP)
			return drop(skb);

		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp)
		{
		        return cont(skb);
                }
	}

	return drop(skb);
}


static struct sk_buff *
filter_tcp(context_t ctx, struct sk_buff *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct tcphdr _tcp;
		const struct tcphdr *tcp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
	                return drop(skb);

		if (ip->protocol != IPPROTO_TCP)
	                return drop(skb);

		tcp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_tcp), &_tcp);
		if (tcp)
                {
                        return cont(skb);
                }
	}

        return drop(skb);
}


static struct sk_buff *
filter_icmp(context_t ctx, struct sk_buff *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct icmphdr _icmp;
		const struct icmphdr *icmp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
	                return drop(skb);

		if (ip->protocol != IPPROTO_ICMP)
	                return drop(skb);

		icmp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_icmp), &_icmp);
		if (icmp)
		{
                        return cont(skb);
                }
	}

        return drop(skb);
}


static struct sk_buff *
filter_flow(context_t ctx, struct sk_buff *skb)
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
		if (udp)
		{
		        return cont(skb);
                }
	}

       	return drop(skb);
}


static struct sk_buff *
filter_vlan(context_t ctx, struct sk_buff *skb)
{
        if ((skb->vlan_tci & VLAN_VID_MASK) == 0)
                return drop(skb);
        else
                return cont(skb);
}


struct pfq_function_descr filter_functions[] = {

        { "id",                 id                      },
        { "ip",                 filter_ip               },
        { "ipv6",               filter_ipv6             },
        { "udp",                filter_udp              },
        { "tcp",                filter_tcp              },
        { "icmp",               filter_icmp             },
        { "flow",               filter_flow             },
        { "vlan",               filter_vlan             },

        { NULL, NULL}};

