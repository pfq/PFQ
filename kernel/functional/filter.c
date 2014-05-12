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

#include "filter.h"


static inline struct sk_buff *
filter_l3_proto(arguments_t args, struct sk_buff *skb)
{
	const u16 type = get_data(u16, args);
        return is_l3_proto(skb, type) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_l4_proto(arguments_t args, struct sk_buff *skb)
{
	const u8 proto = get_data(u8, args);
        return is_l4_proto(skb, proto) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_port(arguments_t args, struct sk_buff *skb)
{
	const u16 port = get_data(u16, args);
        return has_port(skb, port) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_src_port(arguments_t args, struct sk_buff *skb)
{
	const u16 port = get_data(u16, args);
        return has_src_port(skb, port) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_dst_port(arguments_t args, struct sk_buff *skb)
{
	const u16 port = get_data(u16, args);
        return has_dst_port(skb, port) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_addr(arguments_t args, struct sk_buff *skb)
{
	const u64 data = get_data(u64, args);
	uint32_t addr = data >> 32;
	uint32_t mask = data & 0xffffffff;

	// pr_devel("[PFQ] filter_addr: %pI4/%pI4\n", &addr, &mask);

	return has_addr(skb, addr, mask) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_src_addr(arguments_t args, struct sk_buff *skb)
{
	const u64 data = get_data(u64, args);
	uint32_t addr = data >> 32;
	uint32_t mask = data & 0xffffffff;

	return has_src_addr(skb, addr, mask) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_dst_addr(arguments_t args, struct sk_buff *skb)
{
	const u64 data = get_data(u64, args);
	uint32_t addr = data >> 32;
	uint32_t mask = data & 0xffffffff;

	return has_dst_addr(skb, addr, mask) ? skb : drop(skb);
}


struct pfq_monadic_fun_descr filter_functions[] = {

        { "unit",	FUN_ACTION,	INLINE_FUN(unit)          },
        { "ip",         FUN_ACTION,	INLINE_FUN(filter_ip)     },
        { "ip6",        FUN_ACTION,	INLINE_FUN(filter_ip6)    },
        { "udp",        FUN_ACTION,	INLINE_FUN(filter_udp)    },
        { "tcp",        FUN_ACTION,	INLINE_FUN(filter_tcp)    },
        { "icmp",       FUN_ACTION,	INLINE_FUN(filter_icmp)   },
        { "udp6",       FUN_ACTION, 	INLINE_FUN(filter_udp6)   },
        { "tcp6",       FUN_ACTION, 	INLINE_FUN(filter_tcp6)   },
        { "icmp6",      FUN_ACTION, 	INLINE_FUN(filter_icmp6)  },
        { "flow",       FUN_ACTION,	INLINE_FUN(filter_flow)   },
        { "vlan",       FUN_ACTION,	INLINE_FUN(filter_vlan)   },

        { "port",      	FUN_ACTION | FUN_ARG_DATA, filter_port 	   },
        { "src_port",	FUN_ACTION | FUN_ARG_DATA, filter_src_port },
        { "dst_port",   FUN_ACTION | FUN_ARG_DATA, filter_dst_port },
        { "addr",      	FUN_ACTION | FUN_ARG_DATA, filter_addr	   },
        { "src_addr",   FUN_ACTION | FUN_ARG_DATA, filter_src_addr },
        { "dst_addr",   FUN_ACTION | FUN_ARG_DATA, filter_dst_addr },
 	{ "l3_proto", 	FUN_ACTION | FUN_ARG_DATA, filter_l3_proto },
        { "l4_proto",   FUN_ACTION | FUN_ARG_DATA, filter_l4_proto },

        { NULL }};

