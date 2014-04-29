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

#ifndef _FUNCTIONAL_PREDICATE_H
#define _FUNCTIONAL_PREDICATE_H

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/skbuff.h>

#include <linux/ip.h>
#include <linux/ipv6.h>
#include <linux/udp.h>
#include <linux/tcp.h>
#include <linux/icmp.h>
#include <linux/if_vlan.h>

#include <pf_q-engine.h>


#ifdef PFQ_USE_INLINE_FUN

/* predicate functions */

#define INLINE_less 			200
#define INLINE_less_eq 			201
#define INLINE_equal			202
#define INLINE_not_equal	       	203
#define INLINE_greater 			204
#define INLINE_greater_eq  		205
#define INLINE_any_bit 			206
#define INLINE_all_bit			207


#define CASE_PREDICATE(f, call, skb) \
	case INLINE_ ## f: return f(&call->args, skb)


#define RETURN_EVAL_PREDICATE(call, skb) \
	switch((ptrdiff_t)call->fun) \
	{ 	\
		CASE_PREDICATE(less,  call, skb);\
		CASE_PREDICATE(less_eq, call, skb);\
		CASE_PREDICATE(equal, call, skb);\
		CASE_PREDICATE(not_equal, call, skb);\
		CASE_PREDICATE(greater, call, skb);\
		CASE_PREDICATE(greater_eq, call, skb);\
		CASE_PREDICATE(any_bit, call, skb);\
		CASE_PREDICATE(all_bit, call, skb);\
	}

#endif

static inline bool
less(arguments_t *a, struct sk_buff const *skb)
{
	property_expression_t * p = get_property(a);
	const uint64_t * data = get_data(uint64_t, a);
	uint64_t ret = eval_property((property_t *)p, skb);

	if (IS_JUST(ret))
		return FROM_JUST(ret) < *data;

	return false;
}

static inline bool
less_eq(arguments_t *a, struct sk_buff const *skb)
{
	property_expression_t * p = get_property(a);
	const uint64_t * data = get_data(uint64_t, a);
	uint64_t ret = eval_property((property_t *)p, skb);

	if (IS_JUST(ret))
		return FROM_JUST(ret) <= *data;

	return false;
}

static inline bool
greater(arguments_t *a, struct sk_buff const *skb)
{
	property_expression_t * p = get_property(a);
	const uint64_t * data = get_data(uint64_t, a);
	uint64_t ret = eval_property((property_t *)p, skb);

	if (IS_JUST(ret))
		return FROM_JUST(ret) > *data;

	return false;
}

static inline bool
greater_eq(arguments_t *a, struct sk_buff const *skb)
{
	property_expression_t * p = get_property(a);
	const uint64_t * data = get_data(uint64_t, a);
	uint64_t ret = eval_property((property_t *)p, skb);

	if (IS_JUST(ret))
		return FROM_JUST(ret) >= *data;

	return false;
}

static inline bool
equal(arguments_t *a, struct sk_buff const *skb)
{
	property_expression_t * p = get_property(a);
	const uint64_t * data = get_data(uint64_t, a);
	uint64_t ret = eval_property((property_t *)p, skb);

	if (IS_JUST(ret))
		return FROM_JUST(ret) == *data;

	return false;
}

static inline bool
not_equal(arguments_t *a, struct sk_buff const *skb)
{
	property_expression_t * p = get_property(a);
	const uint64_t * data = get_data(uint64_t, a);
	uint64_t ret = eval_property((property_t *)p, skb);

	if (IS_JUST(ret))
		return FROM_JUST(ret) != *data;

	return false;
}

static inline bool
any_bit(arguments_t *a, struct sk_buff const *skb)
{
	property_expression_t * p = get_property(a);

	const uint64_t * data = get_data(uint64_t, a);
	uint64_t ret = eval_property((property_t *)p, skb);

	if (IS_JUST(ret))
		return (FROM_JUST(ret) & *data) != 0;

	return false;
}

static inline bool
all_bit(arguments_t *a, struct sk_buff const *skb)
{
	property_expression_t * p = get_property(a);

	const uint64_t * data = get_data(uint64_t, a);
	uint64_t ret = eval_property((property_t *)p, skb);

	if (IS_JUST(ret))
		return (FROM_JUST(ret) & *data) == *data;

	return false;
}

/* basic predicates ... */

static inline bool
skb_header_available(const struct sk_buff *skb, int offset, int len)
{
        if (skb->len - offset >= len)
                return true;
        return false;
}


static inline bool
is_ip(struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	        return skb_header_available(skb, skb->mac_len, sizeof(struct iphdr));

        return false;
}

static inline bool
is_ip6(struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IPV6))
                return skb_header_available(skb, skb->mac_len, sizeof(struct ipv6hdr));

        return false;
}

static inline bool
is_udp(struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		if (ip->protocol != IPPROTO_UDP)
                        return false;

                return skb_header_available(skb, skb->mac_len  + (ip->ihl<<2), sizeof(struct udphdr));
	}

        return false;
}


static inline bool
is_udp6(struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IPV6))
	{
		struct ipv6hdr _iph6;
    		const struct ipv6hdr *ip6;

		ip6 = skb_header_pointer(skb, skb->mac_len, sizeof(_iph6), &_iph6);
 		if (ip6 == NULL)
                        return false;

		if (ip6->nexthdr != IPPROTO_UDP)
                        return false;

                return skb_header_available(skb, skb->mac_len  + sizeof(struct ipv6hdr), sizeof(struct udphdr));
	}

        return false;
}

static inline bool
is_tcp(struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		if (ip->protocol != IPPROTO_TCP)
                        return false;

		return skb_header_available(skb, skb->mac_len + (ip->ihl<<2), sizeof(struct tcphdr));
	}

        return false;
}


static inline bool
is_tcp6(struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IPV6))
	{
		struct ipv6hdr _iph6;
    		const struct ipv6hdr *ip6;

		ip6 = skb_header_pointer(skb, skb->mac_len, sizeof(_iph6), &_iph6);
 		if (ip6 == NULL)
                        return false;

		if (ip6->nexthdr != IPPROTO_TCP)
                        return false;

                return skb_header_available(skb, skb->mac_len  + sizeof(struct ipv6hdr), sizeof(struct tcphdr));
	}

        return false;
}

static inline bool
is_icmp(struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		if (ip->protocol != IPPROTO_ICMP)
                        return false;

		return skb_header_available(skb, skb->mac_len + (ip->ihl<<2), sizeof(struct icmphdr));
	}

        return false;
}


static inline bool
is_icmp6(struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IPV6))
	{
		struct ipv6hdr _iph6;
    		const struct ipv6hdr *ip6;

		ip6 = skb_header_pointer(skb, skb->mac_len, sizeof(_iph6), &_iph6);
 		if (ip6 == NULL)
                        return false;

		if (ip6->nexthdr != IPPROTO_ICMPV6)
                        return false;

		// ... the icmpv6 header is 32 bits long.

                return skb_header_available(skb, skb->mac_len  + sizeof(struct ipv6hdr), 32 >> 3);
	}

        return false;
}


static inline bool
has_addr(struct sk_buff const *skb, uint32_t addr, uint32_t mask)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		if ((ip->saddr & mask) == (addr & mask) ||
		    (ip->daddr & mask) == (addr & mask))
			return true;
	}

        return false;
}


static inline bool
has_src_addr(struct sk_buff const *skb, uint32_t addr, uint32_t mask)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		if ((ip->saddr & mask) == (addr & mask))
			return true;
	}

        return false;
}

static inline bool
has_dst_addr(struct sk_buff const *skb, uint32_t addr, uint32_t mask)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		if ((ip->daddr & mask) == (addr & mask))
			return true;
	}

        return false;
}


static inline bool
is_flow(struct sk_buff const *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		if (ip->protocol != IPPROTO_UDP &&
		    ip->protocol != IPPROTO_TCP)
                        return false;

		return skb_header_available(skb, skb->mac_len + (ip->ihl<<2), ip->protocol == IPPROTO_UDP ? sizeof(struct udphdr) : sizeof(struct tcphdr));
	}

        return false;
}


static inline bool
is_l3_proto(struct sk_buff const *skb, u16 type)
{
	return eth_hdr(skb)->h_proto == __constant_htons(type);
}


static inline bool
is_l4_proto(struct sk_buff const *skb, u8 protocol)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

                return ip->protocol == protocol;
	}

	return false;
}


static inline bool
has_src_port(struct sk_buff const *skb, uint16_t port)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		switch(ip->protocol)
		{
                case IPPROTO_UDP: {
                	struct udphdr _udph; const struct udphdr *udp;
			udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(struct udphdr), &_udph);
			if (udp == NULL)
				return false;

			return udp->source == htons(port);
		}
		case IPPROTO_TCP: {
                	struct tcphdr _tcph; const struct tcphdr *tcp;
			tcp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(struct tcphdr), &_tcph);
			if (tcp == NULL)
				return false;

			return tcp->source == htons(port);
		}

		default:
			return false;
		}
	}

	return false;
}

static inline bool
has_dst_port(struct sk_buff const *skb, uint16_t port)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		switch(ip->protocol)
		{
                case IPPROTO_UDP: {
                	struct udphdr _udph; const struct udphdr *udp;
			udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(struct udphdr), &_udph);
			if (udp == NULL)
				return false;

			return udp->dest == htons(port);
		}
		case IPPROTO_TCP: {
                	struct tcphdr _tcph; const struct tcphdr *tcp;
			tcp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(struct tcphdr), &_tcph);
			if (tcp == NULL)
				return false;

			return tcp->dest == htons(port);
		}

		default:
			return false;
		}
	}

	return false;
}


static inline bool
has_port(struct sk_buff const *skb, uint16_t port)
{
	return has_src_port(skb, port) || has_dst_port(skb, port);
}


static inline bool
has_vlan(struct sk_buff const *skb)
{
	return (skb->vlan_tci & VLAN_VID_MASK);
}

static inline bool
has_vid(struct sk_buff const *skb, int vid)
{
	return (skb->vlan_tci & VLAN_VID_MASK) == vid;
}


#endif /* _FUNCTIONAL_PREDICATE_H */
