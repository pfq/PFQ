/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
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

#include <engine/lang/skbuff.h>
#include <engine/lang/module.h>

#include <pf_q-nethdr.h>

static inline bool
less(arguments_t args, SkBuff skb)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, skb);

	if (IS_JUST(ret))
		return FROM_JUST(uint64_t, ret) < data;

	return false;
}

static inline bool
less_eq(arguments_t args, SkBuff skb)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, skb);

	if (IS_JUST(ret))
		return FROM_JUST(uint64_t, ret) <= data;

	return false;
}

static inline bool
greater(arguments_t args, SkBuff skb)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, skb);

	if (IS_JUST(ret))
		return FROM_JUST(uint64_t, ret) > data;

	return false;
}

static inline bool
greater_eq(arguments_t args, SkBuff skb)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, skb);

	if (IS_JUST(ret))
		return FROM_JUST(uint64_t, ret) >= data;

	return false;
}

static inline bool
equal(arguments_t args, SkBuff skb)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, skb);

	if (IS_JUST(ret))
		return FROM_JUST(uint64_t, ret) == data;

	return false;
}

static inline bool
not_equal(arguments_t args, SkBuff skb)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, skb);

	if (IS_JUST(ret))
		return FROM_JUST(uint64_t, ret) != data;

	return false;
}

static inline bool
any_bit(arguments_t args, SkBuff skb)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, skb);

	if (IS_JUST(ret))
		return (FROM_JUST(uint64_t, ret) & data) != 0;

	return false;
}

static inline bool
all_bit(arguments_t args, SkBuff skb)
{
	property_t p = GET_ARG_0(property_t, args);
	const uint64_t data = GET_ARG_1(uint64_t, args);

	uint64_t ret = EVAL_PROPERTY(p, skb);

	if (IS_JUST(ret))
		return (FROM_JUST(uint64_t, ret) & data) == data;

	return false;
}

/* basic predicates ... */

static inline bool
skb_header_available(struct sk_buff *skb, int offset, int len)
{
        if (skb->len - offset >= len)
                return true;
        return false;
}


static inline bool
is_ip(SkBuff skb)
{
	if (skb_ip_version(skb) == 4)
		return true;
        return false;
}


static inline bool
is_udp(SkBuff skb)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
                return false;

	if (ip->protocol != IPPROTO_UDP)
                return false;

        return skb_header_available(PFQ_SKB(skb), skb->mac_len  + (ip->ihl<<2), sizeof(struct udphdr));
}


static inline bool
is_tcp(SkBuff skb)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
                return false;

	if (ip->protocol != IPPROTO_TCP)
                return false;

	return skb_header_available(PFQ_SKB(skb), skb->mac_len + (ip->ihl<<2), sizeof(struct tcphdr));
}


static inline bool
is_icmp(SkBuff skb)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
                return false;

	if (ip->protocol != IPPROTO_ICMP)
                return false;

	return skb_header_available(PFQ_SKB(skb), skb->mac_len + (ip->ihl<<2), sizeof(struct icmphdr));
}


static inline bool
has_addr(SkBuff skb, __be32 addr, __be32 mask)
{
	struct iphdr _iph;
	const struct iphdr *ip;

        bool ctx = PFQ_CB(skb)->monad->ep_ctx;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	return  (((ip->saddr & mask) == (addr & mask)) && (ctx & EPOINT_SRC)) ||
		(((ip->daddr & mask) == (addr & mask)) && (ctx & EPOINT_DST));
}


static inline bool
has_src_addr(SkBuff skb, __be32 addr, __be32 mask)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	return ((ip->saddr & mask) == (addr & mask));
}

static inline bool
has_dst_addr(SkBuff skb, __be32 addr, __be32 mask)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	return ((ip->daddr & mask) == (addr & mask));
}


static inline bool
is_flow(SkBuff skb)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	if (ip->protocol != IPPROTO_UDP &&
	    ip->protocol != IPPROTO_TCP)
                return false;

	return skb_header_available(PFQ_SKB(skb), skb->mac_len + (ip->ihl<<2), ip->protocol == IPPROTO_UDP ?
				    sizeof(struct udphdr) : sizeof(struct tcphdr));
}


static inline bool
is_l3_proto(SkBuff skb, uint16_t type)
{
	return eth_hdr(PFQ_SKB(skb))->h_proto == __constant_htons(type);
}


static inline bool
is_l4_proto(SkBuff skb, u8 protocol)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

        return ip->protocol == protocol;
}


static inline bool
is_frag(SkBuff skb)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

        return (ip->frag_off & __constant_htons(IP_MF|IP_OFFSET)) != 0;
}

static inline bool
is_first_frag(SkBuff skb)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

        return (ip->frag_off & __constant_htons(IP_MF|IP_OFFSET)) == __constant_htons(IP_MF);
}

static inline bool
is_more_frag(SkBuff skb)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	return (ip->frag_off & __constant_htons(IP_OFFSET)) != 0;
}

static inline bool
has_src_port(SkBuff skb, uint16_t port)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	switch(ip->protocol)
	{
	case IPPROTO_UDP: {
		struct udphdr _udph; const struct udphdr *udp;
		udp = skb_ip_header_pointer(skb, (ip->ihl<<2), sizeof(struct udphdr), &_udph);
		if (udp == NULL)
			return false;

		return udp->source == cpu_to_be16(port);
	}
	case IPPROTO_TCP: {
		struct tcphdr _tcph; const struct tcphdr *tcp;
		tcp = skb_ip_header_pointer(skb, (ip->ihl<<2), sizeof(struct tcphdr), &_tcph);
		if (tcp == NULL)
			return false;

		return tcp->source == cpu_to_be16(port);
	}
	}

	return false;
}

static inline bool
has_dst_port(SkBuff skb, uint16_t port)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	switch(ip->protocol)
	{
	case IPPROTO_UDP: {
		struct udphdr _udph; const struct udphdr *udp;
		udp = skb_ip_header_pointer(skb, (ip->ihl<<2), sizeof(struct udphdr), &_udph);
		if (udp == NULL)
			return false;

		return udp->dest == cpu_to_be16(port);
	}
	case IPPROTO_TCP: {
		struct tcphdr _tcph; const struct tcphdr *tcp;
		tcp = skb_ip_header_pointer(skb, (ip->ihl<<2), sizeof(struct tcphdr), &_tcph);
		if (tcp == NULL)
			return false;

		return tcp->dest == cpu_to_be16(port);
	}
	}

	return false;
}


static inline bool
has_port(SkBuff skb, uint16_t port)
{
        bool ctx = PFQ_CB(skb)->monad->ep_ctx;

	return (has_src_port(skb, port) && (ctx & EPOINT_SRC)) ||
	       (has_dst_port(skb, port) && (ctx & EPOINT_DST));
}


static inline bool
has_vlan(SkBuff skb)
{
	return (skb->vlan_tci & VLAN_VID_MASK);
}

static inline bool
has_vid(SkBuff skb, int vid)
{
	return (skb->vlan_tci & VLAN_VID_MASK) == vid;
}


static inline bool
is_broadcast(SkBuff skb)
{
	struct ethhdr *eth = eth_hdr(PFQ_SKB(skb));
        bool ctx = PFQ_CB(skb)->monad->ep_ctx;

	return (is_broadcast_ether_addr(eth->h_dest)   && (ctx & EPOINT_DST)) ||
	       (is_broadcast_ether_addr(eth->h_source) && (ctx & EPOINT_SRC));
}

static inline bool
is_multicast(SkBuff skb)
{
	struct ethhdr *eth = eth_hdr(PFQ_SKB(skb));
        bool ctx = PFQ_CB(skb)->monad->ep_ctx;

	return (is_multicast_ether_addr(eth->h_dest) && (ctx & EPOINT_DST)) ||
	       (is_multicast_ether_addr(eth->h_source) && (ctx & EPOINT_SRC));
}


static inline bool
is_ip_broadcast(SkBuff skb)
{
	struct iphdr _iph;
	const struct iphdr *ip;
        bool ctx = PFQ_CB(skb)->monad->ep_ctx;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	return (ipv4_is_lbcast(ip->saddr) && (ctx & EPOINT_SRC)) ||
	       (ipv4_is_lbcast(ip->daddr) && (ctx & EPOINT_DST));
}


static inline bool
is_ip_multicast(SkBuff skb)
{
	struct iphdr _iph;
	const struct iphdr *ip;
        bool ctx = PFQ_CB(skb)->monad->ep_ctx;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	return (ipv4_is_multicast(ip->saddr) && (ctx & EPOINT_SRC)) ||
	       (ipv4_is_multicast(ip->daddr) && (ctx & EPOINT_DST));
}


static inline bool
is_ip_host(SkBuff skb)
{
	struct in_device *in_dev;
	struct iphdr _iph;
	const struct iphdr *ip;
	bool ret = false;
        bool ctx = PFQ_CB(skb)->monad->ep_ctx;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	rcu_read_lock();
	in_dev = __in_dev_get_rcu(PFQ_SKB(skb)->dev);
	if (in_dev != NULL) {
		for_primary_ifa(in_dev) {
			if (((ifa->ifa_address == ip->daddr) && (ctx & EPOINT_DST)) ||
			    ((ifa->ifa_address == ip->saddr) && (ctx & EPOINT_SRC))){
				ret = true;
				break;
			}
		} endfor_ifa(in_dev);
	}
	rcu_read_unlock();
	return ret;
}


static inline bool
is_incoming_host(SkBuff skb)
{
	struct in_device *in_dev;
	struct iphdr _iph;
	const struct iphdr *ip;
	bool ret = false;
	struct ethhdr *eth = eth_hdr(PFQ_SKB(skb));

	if (is_broadcast_ether_addr(eth->h_dest) || is_multicast_ether_addr(eth->h_dest))
		return true;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return false;

	rcu_read_lock();
	in_dev = __in_dev_get_rcu(PFQ_SKB(skb)->dev);
	if (in_dev != NULL) {
		for_primary_ifa(in_dev) {
			if (ifa->ifa_address == ip->daddr)
			{
				ret = true;
				break;
			}
		} endfor_ifa(in_dev);
	}
	rcu_read_unlock();

	return ret;
}


#endif /* PFQ_LANG_PREDICATE_H */
