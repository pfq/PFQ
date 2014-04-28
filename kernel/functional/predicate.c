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

#include "predicate.h"


static bool
__is_ip(arguments_t * a, struct sk_buff const *skb)
{
        return  is_ip(skb);
}

static bool
__is_ip6(arguments_t * a, struct sk_buff const *skb)
{
        return  is_ip6(skb);
}

static bool
__is_udp(arguments_t * a, struct sk_buff const *skb)
{
        return  is_udp(skb);
}

static bool
__is_udp6(arguments_t * a, struct sk_buff const *skb)
{
        return  is_udp6(skb);
}

static bool
__is_tcp(arguments_t * a, struct sk_buff const *skb)
{
        return  is_tcp(skb);
}

static bool
__is_tcp6(arguments_t * a, struct sk_buff const *skb)
{
        return  is_tcp6(skb);
}

static bool
__is_icmp(arguments_t * a, struct sk_buff const *skb)
{
        return  is_icmp(skb);
}

static bool
__is_icmp6(arguments_t * a, struct sk_buff const *skb)
{
        return  is_icmp6(skb);
}

static bool
__is_flow(arguments_t * a, struct sk_buff const *skb)
{
        return  is_flow(skb);
}

static bool
__is_l3_proto(arguments_t * a, struct sk_buff const *skb)
{
	const u16 *type = get_data(u16, a);
	return is_l3_proto(skb, *type);
}

static bool
__is_l4_proto(arguments_t * a, struct sk_buff const *skb)
{
	const u8 *protocol = get_data(u8, a);
	return is_l4_proto(skb, *protocol);
}

static bool
__has_port(arguments_t * a, struct sk_buff const *skb)
{
	const u16 *port = get_data(u16, a);
	return has_port(skb, *port);
}

static bool
__has_src_port(arguments_t * a, struct sk_buff const *skb)
{
	const u16 *port = get_data(u16, a);
	return has_src_port(skb, *port);
}

static bool
__has_dst_port(arguments_t * a, struct sk_buff const *skb)
{
	const u16 *port = get_data(u16, a);
	return has_dst_port(skb, *port);
}

static bool
__has_vlan(arguments_t * a, struct sk_buff const *skb)
{
        return  has_vlan(skb);
}

static bool
__has_vid(arguments_t * a, struct sk_buff const *skb)
{
	const int *id = get_data(int, a);
        return  has_vid(skb, *id);
}

static bool
__has_mark(arguments_t * a, struct sk_buff const *skb)
{
	const unsigned long *value = get_data(unsigned long, a);
	return get_state(skb) == *value;
}

static bool
__has_addr(arguments_t * a, struct sk_buff const *skb)
{
	const u64 *data = get_data(u64, a);
	uint32_t addr = *data >> 32;
	uint32_t mask = *data & 0xffffffff;

	return has_addr(skb, addr, mask);
}


static bool
__has_src_addr(arguments_t * a, struct sk_buff const *skb)
{
	const u64 *data = get_data(u64, a);
	uint32_t addr = *data >> 32;
	uint32_t mask = *data & 0xffffffff;

	return has_src_addr(skb, addr, mask);
}

static bool
__has_dst_addr(arguments_t * a, struct sk_buff const *skb)
{
	const u64 *data = get_data(u64, a);
	uint32_t addr = *data >> 32;
	uint32_t mask = *data & 0xffffffff;

	return has_dst_addr(skb, addr, mask);
}


struct pfq_predicate_fun_descr predicate_functions[] = {

        { "is_ip", 	 __is_ip        , FUN_PREDICATE },
        { "is_tcp",      __is_tcp       , FUN_PREDICATE },
        { "is_udp",      __is_udp       , FUN_PREDICATE },
        { "is_icmp",     __is_icmp      , FUN_PREDICATE },
        { "is_ip6",	 __is_ip6       , FUN_PREDICATE },
        { "is_udp6",	 __is_udp6      , FUN_PREDICATE },
        { "is_tcp6",     __is_tcp6      , FUN_PREDICATE },
        { "is_icmp6",    __is_icmp6     , FUN_PREDICATE },
        { "is_flow",     __is_flow 	, FUN_PREDICATE },
        { "has_vlan",    __has_vlan     , FUN_PREDICATE },
        { "is_l3_proto", __is_l3_proto 	, FUN_PREDICATE | FUN_ARG_DATA },
        { "is_l4_proto", __is_l4_proto	, FUN_PREDICATE | FUN_ARG_DATA },
        { "has_port",    __has_port     , FUN_PREDICATE | FUN_ARG_DATA },
        { "has_src_port",__has_src_port , FUN_PREDICATE | FUN_ARG_DATA },
        { "has_dst_port",__has_dst_port , FUN_PREDICATE | FUN_ARG_DATA },
        { "has_vid",     __has_vid	, FUN_PREDICATE | FUN_ARG_DATA },
        { "has_mark",    __has_mark     , FUN_PREDICATE | FUN_ARG_DATA },
        { "has_addr",    __has_addr     , FUN_PREDICATE | FUN_ARG_DATA },
        { "has_src_addr",__has_src_addr , FUN_PREDICATE | FUN_ARG_DATA },
        { "has_dst_addr",__has_dst_addr , FUN_PREDICATE | FUN_ARG_DATA },

        { NULL, NULL}};

