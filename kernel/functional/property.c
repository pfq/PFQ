/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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

#include <pragma/diagnostic_push>

#include <linux/kernel.h>
#include <linux/module.h>

#include <pragma/diagnostic_pop>

#include <pf_q-module.h>


/****************************************************************
 * 			ip properties
 ****************************************************************/

static uint64_t
ip_tos(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return NOTHING;

		return JUST(ip->tos);
	}

	return NOTHING;
}


static uint64_t
ip_tot_len(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return NOTHING;

		return JUST(ntohs(ip->tot_len));
	}

	return NOTHING;
}


static uint64_t
ip_id(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return NOTHING;

		return JUST(ntohs(ip->id));
	}

	return NOTHING;
}


static uint64_t
ip_ttl(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return NOTHING;

		return JUST(ip->ttl);
	}

	return NOTHING;
}

static uint64_t
ip_frag(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
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
tcp_source(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		struct tcphdr _tcp;
		const struct tcphdr *tcp;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return NOTHING;

		if (ip->protocol != IPPROTO_TCP)
			return NOTHING;

		tcp = skb_header_pointer(b.skb, b.skb->mac_len + (ip->ihl<<2), sizeof(_tcp), &_tcp);
		if (tcp == NULL)
			return NOTHING;

		return JUST(ntohs(tcp->source));
	}

	return NOTHING;
}


static uint64_t
tcp_dest(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		struct tcphdr _tcp;
		const struct tcphdr *tcp;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return NOTHING;

		if (ip->protocol != IPPROTO_TCP)
			return NOTHING;

		tcp = skb_header_pointer(b.skb, b.skb->mac_len + (ip->ihl<<2), sizeof(_tcp), &_tcp);
		if (tcp == NULL)
			return NOTHING;

		return JUST(ntohs(tcp->dest));
	}

	return NOTHING;
}

static uint64_t
tcp_hdrlen_(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		struct tcphdr _tcp;
		const struct tcphdr *tcp;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return NOTHING;

		if (ip->protocol != IPPROTO_TCP)
			return NOTHING;

		tcp = skb_header_pointer(b.skb, b.skb->mac_len + (ip->ihl<<2), sizeof(_tcp), &_tcp);
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
udp_source(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return NOTHING;

		if (ip->protocol != IPPROTO_TCP)
			return NOTHING;

		udp = skb_header_pointer(b.skb, b.skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp == NULL)
			return NOTHING;

		return JUST(ntohs(udp->source));
	}

	return NOTHING;
}


static uint64_t
udp_dest(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return NOTHING;

		if (ip->protocol != IPPROTO_TCP)
			return NOTHING;

		udp = skb_header_pointer(b.skb, b.skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp == NULL)
			return NOTHING;

		return JUST(ntohs(udp->dest));
	}

	return NOTHING;
}

static uint64_t
udp_len(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return NOTHING;

		if (ip->protocol != IPPROTO_TCP)
			return NOTHING;

		udp = skb_header_pointer(b.skb, b.skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp == NULL)
			return NOTHING;

		return JUST(ntohs(udp->len));
	}

	return NOTHING;
}


static uint64_t
icmp_type(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		struct icmphdr _icmp;
		const struct icmphdr *icmp;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return NOTHING;

		if (ip->protocol != IPPROTO_ICMP)
			return NOTHING;

		icmp = skb_header_pointer(b.skb, b.skb->mac_len + (ip->ihl<<2), sizeof(_icmp), &_icmp);
		if (icmp == NULL)
			return NOTHING;

		return JUST(icmp->type);
	}

	return NOTHING;
}


static uint64_t
icmp_code(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		struct icmphdr _icmp;
		const struct icmphdr *icmp;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return NOTHING;

		if (ip->protocol != IPPROTO_ICMP)
			return NOTHING;

		icmp = skb_header_pointer(b.skb, b.skb->mac_len + (ip->ihl<<2), sizeof(_icmp), &_icmp);
		if (icmp == NULL)
			return NOTHING;

		return JUST(icmp->code);
	}

	return NOTHING;
}


static uint64_t
__get_mark(arguments_t args, SkBuff b)
{
	return JUST(get_mark(b));
}


struct pfq_function_descr property_functions[] = {

	{ "ip_tos",	 "SkBuff -> Word64", ip_tos		},
	{ "ip_tot_len",  "SkBuff -> Word64", ip_tot_len		},
	{ "ip_id",	 "SkBuff -> Word64", ip_id		},
	{ "ip_frag",	 "SkBuff -> Word64", ip_frag		},
	{ "ip_ttl",	 "SkBuff -> Word64", ip_ttl		},

	{ "tcp_source",  "SkBuff -> Word64", tcp_source		},
	{ "tcp_dest",	 "SkBuff -> Word64", tcp_dest		},
	{ "tcp_hdrlen",  "SkBuff -> Word64", tcp_hdrlen_	},

	{ "udp_source",  "SkBuff -> Word64", udp_source		},
	{ "udp_dest",	 "SkBuff -> Word64", udp_dest		},
	{ "udp_len",	 "SkBuff -> Word64", udp_len		},

	{ "icmp_type",   "SkBuff -> Word64", icmp_type		},
	{ "icmp_code",   "SkBuff -> Word64", icmp_code		},

	{ "get_mark",	 "SkBuff -> Word64", __get_mark		},

	{ NULL }};

