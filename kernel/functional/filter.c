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


static struct sk_buff *
filter_l3_proto(arguments_t args, struct sk_buff *skb)
{
	const u16 type = get_data(u16, args);
        return is_l3_proto(skb, type) ? skb : drop(skb);
}

static struct sk_buff *
filter_l4_proto(arguments_t args, struct sk_buff *skb)
{
	const u8 proto = get_data(u8, args);
        return is_l4_proto(skb, proto) ? skb : drop(skb);
}

static struct sk_buff *
filter_port(arguments_t args, struct sk_buff *skb)
{
	const u16 port = get_data(u16, args);
        return has_port(skb, port) ? skb : drop(skb);
}

static struct sk_buff *
filter_src_port(arguments_t args, struct sk_buff *skb)
{
	const u16 port = get_data(u16, args);
        return has_src_port(skb, port) ? skb : drop(skb);
}

static struct sk_buff *
filter_dst_port(arguments_t args, struct sk_buff *skb)
{
	const u16 port = get_data(u16, args);
        return has_dst_port(skb, port) ? skb : drop(skb);
}



static int filter_addr_init(arguments_t args)
{
	struct network_addr {
	 	__be32 addr;
	 	int 	 prefix;
	} data = get_data(struct network_addr, args);

	__be32 ipv4 = data.addr;
	__be32 mask = make_mask(data.prefix);

	set_data (args, ipv4);
	set_data2(args, mask);

	pr_devel("[PFQ|init] filter: addr:%pI4 mask:%pI4\n", &ipv4, &mask);

	return 0;
}


static struct sk_buff *
filter_addr(arguments_t args, struct sk_buff *skb)
{
	__be32 addr = get_data(__be32, args);
	__be32 mask = get_data2(__be32, args);

	return has_addr(skb, addr, mask) ? skb : drop(skb);
}


static struct sk_buff *
filter_src_addr(arguments_t args, struct sk_buff *skb)
{
	__be32 addr = get_data(__be32, args);
	__be32 mask = get_data2(__be32, args);

	return has_src_addr(skb, addr, mask) ? skb : drop(skb);
}

static struct sk_buff *
filter_dst_addr(arguments_t args, struct sk_buff *skb)
{
	__be32 addr = get_data(__be32, args);
	__be32 mask = get_data2(__be32, args);

	return has_dst_addr(skb, addr, mask) ? skb : drop(skb);
}

static struct sk_buff *
filter_no_frag(arguments_t args, struct sk_buff *skb)
{
	return is_frag(skb) ? drop(skb) : skb;
}

static struct sk_buff *
filter_no_more_frag(arguments_t args, struct sk_buff *skb)
{
	return is_more_frag(skb) ? drop(skb) : skb;
}


struct pfq_function_descr filter_functions[] = {

        { "unit",	  "SkBuff -> Action SkBuff", 	unit          		},
        { "ip",           "SkBuff -> Action SkBuff", 	filter_ip     		},
        { "ip6",          "SkBuff -> Action SkBuff", 	filter_ip6    		},
        { "udp",          "SkBuff -> Action SkBuff", 	filter_udp    		},
        { "tcp",          "SkBuff -> Action SkBuff", 	filter_tcp    		},
        { "icmp",         "SkBuff -> Action SkBuff", 	filter_icmp   		},
        { "udp6",         "SkBuff -> Action SkBuff", 	filter_udp6   		},
        { "tcp6",         "SkBuff -> Action SkBuff", 	filter_tcp6   		},
        { "icmp6",        "SkBuff -> Action SkBuff", 	filter_icmp6  		},
        { "flow",         "SkBuff -> Action SkBuff", 	filter_flow   		},
        { "vlan",         "SkBuff -> Action SkBuff", 	filter_vlan   		},
 	{ "no_frag", 	  "SkBuff -> Action SkBuff", 	filter_no_frag 		},
 	{ "no_more_frag", "SkBuff -> Action SkBuff", 	filter_no_more_frag     },

        { "port",     	  "Word16 -> SkBuff -> Action SkBuff", 		 filter_port     },
        { "src_port", 	  "Word16 -> SkBuff -> Action SkBuff", 		 filter_src_port },
        { "dst_port", 	  "Word16 -> SkBuff -> Action SkBuff", 		 filter_dst_port },
        { "addr",     	  "Word32 -> SkBuff -> Action SkBuff", 		 filter_addr     , filter_addr_init },
        { "src_addr", 	  "Word32 -> SkBuff -> Action SkBuff", 		 filter_src_addr , filter_addr_init },
        { "dst_addr", 	  "Word32 -> SkBuff -> Action SkBuff", 		 filter_dst_addr , filter_addr_init },
 	{ "l3_proto",     "Word16 -> SkBuff -> Action SkBuff",           filter_l3_proto },
        { "l4_proto",     "Word8  -> SkBuff -> Action SkBuff",           filter_l4_proto },

        { NULL }};

