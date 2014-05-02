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
#include "inline.h"

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

        { "unit",		INLINE_FUN(unit)          	, FUN_ACTION  	},
        { "ip",         	INLINE_FUN(filter_ip)    	, FUN_ACTION   	},
        { "ip6",        	INLINE_FUN(filter_ip6)   	, FUN_ACTION   	},
        { "udp",        	INLINE_FUN(filter_udp)   	, FUN_ACTION   	},
        { "tcp",        	INLINE_FUN(filter_tcp)   	, FUN_ACTION   	},
        { "icmp",       	INLINE_FUN(filter_icmp)  	, FUN_ACTION   	},
        { "udp6",        	INLINE_FUN(filter_udp6)  	, FUN_ACTION    },
        { "tcp6",        	INLINE_FUN(filter_tcp6)  	, FUN_ACTION   	},
        { "icmp6",       	INLINE_FUN(filter_icmp6)	, FUN_ACTION    },
        { "flow",       	INLINE_FUN(filter_flow)  	, FUN_ACTION   	},
        { "vlan",       	INLINE_FUN(filter_vlan)  	, FUN_ACTION   	},

        { "port",      		filter_port 	 		, FUN_ACTION | FUN_ARG_DATA   },
        { "src_port",		filter_src_port  		, FUN_ACTION | FUN_ARG_DATA   },
        { "dst_port",   	filter_dst_port  		, FUN_ACTION | FUN_ARG_DATA   },
        { "addr",      		filter_addr	 		, FUN_ACTION | FUN_ARG_DATA   },
        { "src_addr",   	filter_src_addr	 		, FUN_ACTION | FUN_ARG_DATA   },
        { "dst_addr",   	filter_dst_addr	 		, FUN_ACTION | FUN_ARG_DATA   },
 	{ "l3_proto", 		filter_l3_proto  		, FUN_ACTION | FUN_ARG_DATA   },
        { "l4_proto",   	filter_l4_proto  		, FUN_ACTION | FUN_ARG_DATA   },

        { NULL, NULL}};

