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

#include <engine/lang/module.h>
#include <engine/lang/qbuff.h>

#include <pfq/nethdr.h>

/****************************************************************
 * 			ip properties
 ****************************************************************/

static uint64_t
ip_tos(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return NOTHING;

	return JUST(ip->tos);
}


static uint64_t
ip_tot_len(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return NOTHING;

	return JUST(be16_to_cpu(ip->tot_len));
}


static uint64_t
ip_id(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return NOTHING;

	return JUST(be16_to_cpu(ip->id));
}


static uint64_t
ip_ttl(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return NOTHING;

	return JUST(ip->ttl);
}

static uint64_t
ip_frag(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return NOTHING;

	return JUST(be16_to_cpu(ip->frag_off));
}


/****************************************************************
 * 			tcp properties
 ****************************************************************/

static uint64_t
tcp_source(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	struct tcphdr _tcp;
	const struct tcphdr *tcp;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return NOTHING;

	if (ip->protocol != IPPROTO_TCP)
		return NOTHING;

	tcp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(_tcp), &_tcp);
	if (tcp == NULL)
		return NOTHING;

	return JUST(be16_to_cpu(tcp->source));
}


static uint64_t
tcp_dest(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	struct tcphdr _tcp;
	const struct tcphdr *tcp;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return NOTHING;

	if (ip->protocol != IPPROTO_TCP)
		return NOTHING;

	tcp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(_tcp), &_tcp);
	if (tcp == NULL)
		return NOTHING;

	return JUST(be16_to_cpu(tcp->dest));
}

static uint64_t
tcp_hdrlen_(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	struct tcphdr _tcp;
	const struct tcphdr *tcp;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return NOTHING;

	if (ip->protocol != IPPROTO_TCP)
		return NOTHING;

	tcp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(_tcp), &_tcp);
	if (tcp == NULL)
		return NOTHING;

	return JUST(tcp->doff * 4);
}

/****************************************************************
 * 			udp properties
 ****************************************************************/

static uint64_t
udp_source(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	struct udphdr _udp;
	const struct udphdr *udp;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return NOTHING;

	if (ip->protocol != IPPROTO_TCP)
		return NOTHING;

	udp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(_udp), &_udp);
	if (udp == NULL)
		return NOTHING;

	return JUST(be16_to_cpu(udp->source));
}


static uint64_t
udp_dest(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	struct udphdr _udp;
	const struct udphdr *udp;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return NOTHING;

	if (ip->protocol != IPPROTO_TCP)
		return NOTHING;

	udp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(_udp), &_udp);
	if (udp == NULL)
		return NOTHING;

	return JUST(be16_to_cpu(udp->dest));
}

static uint64_t
udp_len(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	struct udphdr _udp;
	const struct udphdr *udp;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return NOTHING;

	if (ip->protocol != IPPROTO_TCP)
		return NOTHING;

	udp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(_udp), &_udp);
	if (udp == NULL)
		return NOTHING;

	return JUST(be16_to_cpu(udp->len));
}


static uint64_t
icmp_type(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	struct icmphdr _icmp;
	const struct icmphdr *icmp;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return NOTHING;

	if (ip->protocol != IPPROTO_ICMP)
		return NOTHING;

	icmp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(_icmp), &_icmp);
	if (icmp == NULL)
		return NOTHING;

	return JUST(icmp->type);
}


static uint64_t
icmp_code(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	struct icmphdr _icmp;
	const struct icmphdr *icmp;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return NOTHING;

	if (ip->protocol != IPPROTO_ICMP)
		return NOTHING;

	icmp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(_icmp), &_icmp);
	if (icmp == NULL)
		return NOTHING;

	return JUST(icmp->code);
}


static uint64_t
__get_mark(arguments_t args, struct qbuff * buff)
{
	return JUST(get_mark(buff));
}


static uint64_t
__get_state(arguments_t args, struct qbuff * buff)
{
	return JUST(get_state(buff));
}


struct pfq_lang_function_descr property_functions[] = {

	{ "ip_tos",	 "Qbuff -> Word64", ip_tos		},
	{ "ip_tot_len",  "Qbuff -> Word64", ip_tot_len		},
	{ "ip_id",	 "Qbuff -> Word64", ip_id		},
	{ "ip_frag",	 "Qbuff -> Word64", ip_frag		},
	{ "ip_ttl",	 "Qbuff -> Word64", ip_ttl		},

	{ "tcp_source",  "Qbuff -> Word64", tcp_source		},
	{ "tcp_dest",	 "Qbuff -> Word64", tcp_dest		},
	{ "tcp_hdrlen",  "Qbuff -> Word64", tcp_hdrlen_	},

	{ "udp_source",  "Qbuff -> Word64", udp_source		},
	{ "udp_dest",	 "Qbuff -> Word64", udp_dest		},
	{ "udp_len",	 "Qbuff -> Word64", udp_len		},

	{ "icmp_type",   "Qbuff -> Word64", icmp_type		},
	{ "icmp_code",   "Qbuff -> Word64", icmp_code		},

	{ "get_mark",	 "Qbuff -> Word64", __get_mark		},
	{ "get_state",	 "Qbuff -> Word64", __get_state	},

	{ NULL }};

