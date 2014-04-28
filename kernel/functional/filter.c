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

static inline struct sk_buff *
filter_l3_proto(arguments_t * a, struct sk_buff *skb)
{
	const u16 *type = get_data(u16, a);
        return is_l3_proto(skb, *type) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_l4_proto(arguments_t * a, struct sk_buff *skb)
{
	const u8 *proto = get_data(u8, a);
        return is_l4_proto(skb, *proto) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_port(arguments_t * a, struct sk_buff *skb)
{
	const u16 *port = get_data(u16, a);
        return has_port(skb, *port) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_src_port(arguments_t * a, struct sk_buff *skb)
{
	const u16 *port = get_data(u16, a);
        return has_src_port(skb, *port) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_dst_port(arguments_t * a, struct sk_buff *skb)
{
	const u16 *port = get_data(u16, a);
        return has_dst_port(skb, *port) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_addr(arguments_t * a, struct sk_buff *skb)
{
	const u64 *data = get_data(u64, a);
	uint32_t addr = *data >> 32;
	uint32_t mask = *data & 0xffffffff;

	// pr_devel("[PFQ] filter_addr: %pI4/%pI4\n", &addr, &mask);

	return has_addr(skb, addr, mask) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_src_addr(arguments_t * a, struct sk_buff *skb)
{
	const u64 *data = get_data(u64, a);
	uint32_t addr = *data >> 32;
	uint32_t mask = *data & 0xffffffff;

	return has_src_addr(skb, addr, mask) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_dst_addr(arguments_t * a, struct sk_buff *skb)
{
	const u64 *data = get_data(u64, a);
	uint32_t addr = *data >> 32;
	uint32_t mask = *data & 0xffffffff;

	return has_dst_addr(skb, addr, mask) ? skb : drop(skb);
}


struct pfq_monadic_fun_descr filter_functions[] = {

        { "port",      	filter_port 	 , FUN_ACTION | FUN_WITH_ARG   },
        { "src_port",	filter_src_port  , FUN_ACTION | FUN_WITH_ARG   },
        { "dst_port",   filter_dst_port  , FUN_ACTION | FUN_WITH_ARG   },
        { "addr",      	filter_addr	 , FUN_ACTION | FUN_WITH_ARG   },
        { "src_addr",   filter_src_addr	 , FUN_ACTION | FUN_WITH_ARG   },
        { "dst_addr",   filter_dst_addr	 , FUN_ACTION | FUN_WITH_ARG   },
 	{ "l3_proto", 	filter_l3_proto  , FUN_ACTION | FUN_WITH_ARG   },
        { "l4_proto",   filter_l4_proto  , FUN_ACTION | FUN_WITH_ARG   },

        { NULL, NULL}};

