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
__is_ip(argument_t a, struct sk_buff const *skb)
{
        return  is_ip(skb);
}

static bool
__is_ip6(argument_t a, struct sk_buff const *skb)
{
        return  is_ip6(skb);
}

static bool
__is_udp(argument_t a, struct sk_buff const *skb)
{
        return  is_udp(skb);
}

static bool
__is_udp6(argument_t a, struct sk_buff const *skb)
{
        return  is_udp6(skb);
}

static bool
__is_tcp(argument_t a, struct sk_buff const *skb)
{
        return  is_tcp(skb);
}

static bool
__is_tcp6(argument_t a, struct sk_buff const *skb)
{
        return  is_tcp6(skb);
}

static bool
__is_icmp(argument_t a, struct sk_buff const *skb)
{
        return  is_icmp(skb);
}

static bool
__is_icmp6(argument_t a, struct sk_buff const *skb)
{
        return  is_icmp6(skb);
}

static bool
__is_flow(argument_t a, struct sk_buff const *skb)
{
        return  is_flow(skb);
}

static bool
__is_l3_proto(argument_t a, struct sk_buff const *skb)
{
	const u16 *type = get_argument(u16, a);
	return is_l3_proto(skb, *type);
}

static bool
__is_l4_proto(argument_t a, struct sk_buff const *skb)
{
	const u8 *protocol = get_argument(u8, a);
	return is_l4_proto(skb, *protocol);
}

static bool
__has_port(argument_t a, struct sk_buff const *skb)
{
	const u16 *port = get_argument(u16, a);
	return has_port(skb, *port);
}

static bool
__has_src_port(argument_t a, struct sk_buff const *skb)
{
	const u16 *port = get_argument(u16, a);
	return has_src_port(skb, *port);
}

static bool
__has_dst_port(argument_t a, struct sk_buff const *skb)
{
	const u16 *port = get_argument(u16, a);
	return has_dst_port(skb, *port);
}

static bool
__has_vlan(argument_t a, struct sk_buff const *skb)
{
        return  has_vlan(skb);
}

static bool
__has_vid(argument_t a, struct sk_buff const *skb)
{
	const int *id = get_argument(int, a);
        return  has_vid(skb, *id);
}

static bool
__has_mark(argument_t a, struct sk_buff const *skb)
{
	const unsigned long *value = get_argument(unsigned long, a);
	return get_state(skb) == *value;
}

struct pfq_predicate_fun_descr predicate_functions[] = {

        { "is_ip",       __is_ip         },
        { "is_tcp",      __is_tcp        },
        { "is_udp",      __is_udp        },
        { "is_icmp",     __is_icmp       },
        { "is_ip6",    	 __is_ip6        },
        { "is_udp6",     __is_udp6       },
        { "is_tcp6",     __is_tcp6       },
        { "is_icmp6",    __is_icmp6      },
        { "is_flow",     __is_flow 	 },
        { "is_l3_proto", __is_l3_proto 	 },
        { "is_l4_proto", __is_l4_proto	 },
        { "has_port",    __has_port      },
        { "has_src_port",__has_src_port  },
        { "has_dst_port",__has_dst_port  },
        { "has_vlan",    __has_vlan      },
        { "has_vid",     __has_vid	 },
        { "has_mark",    __has_mark      },

        { NULL, NULL}};

