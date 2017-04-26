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

#ifndef PFQ_LANG_PREDICATE_H
#define PFQ_LANG_PREDICATE_H


#include <lang/module.h>
#include <lang/maybe.h>
#include <lang/qbuff.h>

#include <pfq/kcompat.h>
#include <pfq/nethdr.h>
#include <pfq/qbuff.h>


static inline bool
less(arguments_t args, struct qbuff * buff)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, buff);

	if (IS_JUST(ret))
		return FROM_JUST(uint64_t, ret) < data;

	return false;
}

static inline bool
less_eq(arguments_t args, struct qbuff * buff)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, buff);

	if (IS_JUST(ret))
		return FROM_JUST(uint64_t, ret) <= data;

	return false;
}

static inline bool
greater(arguments_t args, struct qbuff * buff)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, buff);

	if (IS_JUST(ret))
		return FROM_JUST(uint64_t, ret) > data;

	return false;
}

static inline bool
greater_eq(arguments_t args, struct qbuff * buff)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, buff);

	if (IS_JUST(ret))
		return FROM_JUST(uint64_t, ret) >= data;

	return false;
}

static inline bool
equal(arguments_t args, struct qbuff * buff)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, buff);

	if (IS_JUST(ret))
		return FROM_JUST(uint64_t, ret) == data;

	return false;
}

static inline bool
not_equal(arguments_t args, struct qbuff * buff)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, buff);

	if (IS_JUST(ret))
		return FROM_JUST(uint64_t, ret) != data;

	return false;
}

static inline bool
any_bit(arguments_t args, struct qbuff * buff)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, buff);

	if (IS_JUST(ret))
		return (FROM_JUST(uint64_t, ret) & data) != 0;

	return false;
}

static inline bool
all_bit(arguments_t args, struct qbuff * buff)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, buff);

	if (IS_JUST(ret))
		return (FROM_JUST(uint64_t, ret) & data) == data;

	return false;
}

/* basic predicates ... */

static inline bool
qbuff_header_available(struct qbuff *buff, int offset, int len)
{
        if ((int)qbuff_len(buff) - offset >= len)
                return true;
        return false;
}


static inline bool
is_ip(struct qbuff * buff)
{
	if (qbuff_ip_version(buff) == 4)
		return true;
        return false;
}

static inline bool
is_udp(struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
                return false;

	if (ip->protocol != IPPROTO_UDP)
                return false;

        return qbuff_header_available(buff, (int)qbuff_maclen(buff)  + (ip->ihl<<2), sizeof(struct udphdr));
}


static inline bool
is_tcp(struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
                return false;

	if (ip->protocol != IPPROTO_TCP)
                return false;

	return qbuff_header_available(buff, (int)qbuff_maclen(buff) + (ip->ihl<<2), sizeof(struct tcphdr));
}


static inline bool
is_icmp(struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
                return false;

	if (ip->protocol != IPPROTO_ICMP)
                return false;

	return qbuff_header_available(buff, (int)qbuff_maclen(buff) + (ip->ihl<<2), sizeof(struct icmphdr));
}


static inline bool
has_addr(struct qbuff * buff, __be32 addr, __be32 mask)
{
	struct iphdr _iph;
	const struct iphdr *ip;

        bool ctx = buff->monad->ep_ctx;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	return  (((ip->saddr & mask) == (addr & mask)) && (ctx & EPOINT_SRC)) ||
		(((ip->daddr & mask) == (addr & mask)) && (ctx & EPOINT_DST));
}


static inline bool
has_src_addr(struct qbuff * buff, __be32 addr, __be32 mask)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	return ((ip->saddr & mask) == (addr & mask));
}

static inline bool
has_dst_addr(struct qbuff * buff, __be32 addr, __be32 mask)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	return ((ip->daddr & mask) == (addr & mask));
}


static inline bool
is_flow(struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	if (ip->protocol != IPPROTO_UDP &&
	    ip->protocol != IPPROTO_TCP)
                return false;

	return qbuff_header_available(buff, (int)qbuff_maclen(buff) + (ip->ihl<<2), ip->protocol == IPPROTO_UDP ?
				    sizeof(struct udphdr) : sizeof(struct tcphdr));
}


static inline bool
is_l3_proto(struct qbuff * buff, uint16_t type)
{
	return qbuff_eth_hdr(buff)->h_proto == __constant_htons(type);
}


static inline bool
is_l4_proto(struct qbuff * buff, uint8_t protocol)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

        return ip->protocol == protocol;
}


static inline bool
is_frag(struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

        return (ip->frag_off & __constant_htons(IP_MF|IP_OFFSET)) != 0;
}

static inline bool
is_first_frag(struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

        return (ip->frag_off & __constant_htons(IP_MF|IP_OFFSET)) == __constant_htons(IP_MF);
}

static inline bool
is_more_frag(struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	return (ip->frag_off & __constant_htons(IP_OFFSET)) != 0;
}

static inline bool
has_src_port(struct qbuff * buff, uint16_t port)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	switch(ip->protocol)
	{
	case IPPROTO_UDP: {
		struct udphdr _udph; const struct udphdr *udp;
		udp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(struct udphdr), &_udph);
		if (udp == NULL)
			return false;

		return udp->source == cpu_to_be16(port);
	}
	case IPPROTO_TCP: {
		struct tcphdr _tcph; const struct tcphdr *tcp;
		tcp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(struct tcphdr), &_tcph);
		if (tcp == NULL)
			return false;

		return tcp->source == cpu_to_be16(port);
	}
	}

	return false;
}

static inline bool
has_dst_port(struct qbuff * buff, uint16_t port)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	switch(ip->protocol)
	{
	case IPPROTO_UDP: {
		struct udphdr _udph; const struct udphdr *udp;
		udp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(struct udphdr), &_udph);
		if (udp == NULL)
			return false;

		return udp->dest == cpu_to_be16(port);
	}
	case IPPROTO_TCP: {
		struct tcphdr _tcph; const struct tcphdr *tcp;
		tcp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(struct tcphdr), &_tcph);
		if (tcp == NULL)
			return false;

		return tcp->dest == cpu_to_be16(port);
	}
	}

	return false;
}


static inline bool
has_port(struct qbuff * buff, uint16_t port)
{
        bool ctx = buff->monad->ep_ctx;

	return (has_src_port(buff, port) && (ctx & EPOINT_SRC)) ||
	       (has_dst_port(buff, port) && (ctx & EPOINT_DST));
}


static inline bool
has_vlan(struct qbuff * buff)
{
	return (qbuff_vlan_tci(buff) & Q_VLAN_VID_MASK);
}

static inline bool
has_vid(struct qbuff * buff, int vid)
{
	return (qbuff_vlan_tci(buff) & Q_VLAN_VID_MASK) == vid;
}


static inline bool
is_broadcast(struct qbuff * buff)
{
	struct ethhdr *eth = qbuff_eth_hdr(buff);
        bool ctx = buff->monad->ep_ctx;

	return (is_broadcast_ether_addr(eth->h_dest)   && (ctx & EPOINT_DST)) ||
	       (is_broadcast_ether_addr(eth->h_source) && (ctx & EPOINT_SRC));
}

static inline bool
is_multicast(struct qbuff * buff)
{
	struct ethhdr *eth = qbuff_eth_hdr(buff);
        bool ctx = buff->monad->ep_ctx;

	return (is_multicast_ether_addr(eth->h_dest) && (ctx & EPOINT_DST)) ||
	       (is_multicast_ether_addr(eth->h_source) && (ctx & EPOINT_SRC));
}


static inline bool
is_ip_broadcast(struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;
        bool ctx = buff->monad->ep_ctx;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	return (ipv4_is_lbcast(ip->saddr) && (ctx & EPOINT_SRC)) ||
	       (ipv4_is_lbcast(ip->daddr) && (ctx & EPOINT_DST));
}


static inline bool
is_ip_multicast(struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;
        bool ctx = buff->monad->ep_ctx;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	return (ipv4_is_multicast(ip->saddr) && (ctx & EPOINT_SRC)) ||
	       (ipv4_is_multicast(ip->daddr) && (ctx & EPOINT_DST));
}


static inline bool
is_ip_host(struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	return qbuff_ingress(buff, ip);
}


static inline bool
is_incoming_host(struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;
	struct ethhdr *eth = qbuff_eth_hdr(buff);

	if (is_broadcast_ether_addr(eth->h_dest) || is_multicast_ether_addr(eth->h_dest))
		return true;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	return qbuff_ingress(buff, ip);
}


#endif /* PFQ_LANG_PREDICATE_H */
