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

#include "inline.h"

/****************************************************************
 * 			ip properties
 ****************************************************************/

static uint64_t
ip_tos(arguments_t args, struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return NOTHING;

		return JUST(ip->tos);
	}

        return NOTHING;
}


static uint64_t
ip_tot_len(arguments_t args, struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return NOTHING;

		return JUST(ntohs(ip->tot_len));
	}

        return NOTHING;
}


static uint64_t
ip_id(arguments_t args, struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return NOTHING;

		return JUST(ntohs(ip->id));
	}

        return NOTHING;
}


static uint64_t
ip_ttl(arguments_t args, struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return NOTHING;

		return JUST(ip->ttl);
	}

        return NOTHING;
}

static uint64_t
ip_frag(arguments_t args, struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return NOTHING;

		return JUST(ntohs(ip->frag_off));
	}

        return NOTHING;
}


/****************************************************************
 * 			tcp properties
 ****************************************************************/

static uint64_t
tcp_source(arguments_t args, struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct tcphdr _tcp;
		const struct tcphdr *tcp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return NOTHING;

		if (ip->protocol != IPPROTO_TCP)
                        return NOTHING;

		tcp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_tcp), &_tcp);
		if (tcp == NULL)
			return NOTHING;

		return JUST(ntohs(tcp->source));
	}

        return NOTHING;
}


static uint64_t
tcp_dest(arguments_t args, struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct tcphdr _tcp;
		const struct tcphdr *tcp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return NOTHING;

		if (ip->protocol != IPPROTO_TCP)
                        return NOTHING;

		tcp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_tcp), &_tcp);
		if (tcp == NULL)
			return NOTHING;

		return JUST(ntohs(tcp->dest));
	}

        return NOTHING;
}

static uint64_t
tcp_hdrlen_(arguments_t args, struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct tcphdr _tcp;
		const struct tcphdr *tcp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return NOTHING;

		if (ip->protocol != IPPROTO_TCP)
                        return NOTHING;

		tcp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_tcp), &_tcp);
		if (tcp == NULL)
			return NOTHING;

		return JUST(tcp->doff * 4);
	}

        return NOTHING;
}

/****************************************************************
 * 			udp properties
 ****************************************************************/

static uint64_t
udp_source(arguments_t args, struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return NOTHING;

		if (ip->protocol != IPPROTO_TCP)
                        return NOTHING;

		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp == NULL)
			return NOTHING;

		return JUST(ntohs(udp->source));
	}

        return NOTHING;
}


static uint64_t
udp_dest(arguments_t args, struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return NOTHING;

		if (ip->protocol != IPPROTO_TCP)
                        return NOTHING;

		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp == NULL)
			return NOTHING;

		return JUST(ntohs(udp->dest));
	}

        return NOTHING;
}

static uint64_t
udp_len(arguments_t args, struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return NOTHING;

		if (ip->protocol != IPPROTO_TCP)
                        return NOTHING;

		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp == NULL)
			return NOTHING;

		return JUST(ntohs(udp->len));
	}

        return NOTHING;
}


static uint64_t
icmp_type(arguments_t args, struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct icmphdr _icmp;
		const struct icmphdr *icmp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return NOTHING;

		if (ip->protocol != IPPROTO_ICMP)
                        return NOTHING;

		icmp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_icmp), &_icmp);
		if (icmp == NULL)
			return NOTHING;

		return JUST(icmp->type);
	}

        return NOTHING;
}


static uint64_t
icmp_code(arguments_t args, struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct icmphdr _icmp;
		const struct icmphdr *icmp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return NOTHING;

		if (ip->protocol != IPPROTO_ICMP)
                        return NOTHING;

		icmp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_icmp), &_icmp);
		if (icmp == NULL)
			return NOTHING;

		return JUST(icmp->code);
	}

        return NOTHING;
}


static uint64_t
get_mark(arguments_t args, struct sk_buff const *skb)
{
	return JUST(get_state(skb));
}


struct pfq_property_fun_descr property_functions[] = {

        { "ip_tos", 	 FUN_PROPERTY, ip_tos 	   },
        { "ip_tot_len",  FUN_PROPERTY, ip_tot_len  },
        { "ip_id",  	 FUN_PROPERTY, ip_id 	   },
        { "ip_frag",	 FUN_PROPERTY, ip_frag 	   },
        { "ip_ttl", 	 FUN_PROPERTY, ip_ttl 	   },

        { "tcp_source",  FUN_PROPERTY, tcp_source  },
        { "tcp_dest", 	 FUN_PROPERTY, tcp_dest    },
        { "tcp_hdrlen",  FUN_PROPERTY, tcp_hdrlen_ },

        { "udp_source",  FUN_PROPERTY, udp_source  },
        { "udp_dest", 	 FUN_PROPERTY, udp_dest    },
        { "udp_len",  	 FUN_PROPERTY, udp_len	   },

        { "icmp_type",   FUN_PROPERTY, icmp_type   },
        { "icmp_code",   FUN_PROPERTY, icmp_code   },

	{ "get_mark", 	 FUN_PROPERTY, get_mark    },

        { NULL }};

