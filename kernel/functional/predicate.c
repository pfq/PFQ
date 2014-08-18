/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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

#include "predicate.h"


static bool
pred_is_ip(arguments_t args, SkBuff b)
{
        return  is_ip(b);
}

static bool
pred_is_ip6(arguments_t args, SkBuff b)
{
        return  is_ip6(b);
}

static bool
pred_is_udp(arguments_t args, SkBuff b)
{
        return  is_udp(b);
}

static bool
pred_is_udp6(arguments_t args, SkBuff b)
{
        return  is_udp6(b);
}

static bool
pred_is_tcp(arguments_t args, SkBuff b)
{
        return  is_tcp(b);
}

static bool
pred_is_tcp6(arguments_t args, SkBuff b)
{
        return  is_tcp6(b);
}

static bool
pred_is_icmp(arguments_t args, SkBuff b)
{
        return  is_icmp(b);
}

static bool
pred_is_icmp6(arguments_t args, SkBuff b)
{
        return  is_icmp6(b);
}

static bool
pred_is_flow(arguments_t args, SkBuff b)
{
        return  is_flow(b);
}

static bool
pred_is_l3_proto(arguments_t args, SkBuff b)
{
	const u16 type = get_arg(u16, args);
	return is_l3_proto(b, type);
}

static bool
pred_is_l4_proto(arguments_t args, SkBuff b)
{
	const u8 protocol = get_arg(u8, args);
	return is_l4_proto(b, protocol);
}

static bool
pred_has_port(arguments_t args, SkBuff b)
{
	const u16 port = get_arg(u16, args);
	return has_port(b, port);
}

static bool
pred_has_src_port(arguments_t args, SkBuff b)
{
	const u16 port = get_arg(u16, args);
	return has_src_port(b, port);
}

static bool
pred_has_dst_port(arguments_t args, SkBuff b)
{
	const u16 port = get_arg(u16, args);
	return has_dst_port(b, port);
}

static bool
pred_has_vlan(arguments_t args, SkBuff b)
{
        return  has_vlan(b);
}

static bool
pred_has_vid(arguments_t args, SkBuff b)
{
	const int id = get_arg(int, args);
        return  has_vid(b, id);
}

static bool
pred_has_mark(arguments_t args, SkBuff b)
{
	const unsigned long value = get_arg(unsigned long, args);
	return get_mark(b) == value;
}


static int pred_addr_init(arguments_t args)
{
	struct network_addr {
	 	__be32  addr;
	 	int 	prefix;
	} data = get_arg(struct network_addr, args);

	__be32 ipv4 = data.addr;
	__be32 mask = make_mask(data.prefix);

	set_arg_0(args, ipv4);
	set_arg_1(args, mask);

	pr_devel("[PFQ|init] predicate: addr:%pI4 mask:%pI4\n", &ipv4, &mask);

	return 0;
}


static bool
pred_has_addr(arguments_t args, SkBuff b)
{
	__be32 addr = get_arg_0(__be32, args);
	__be32 mask = get_arg_1(__be32, args);

	return has_addr(b, addr, mask);
}


static bool
pred_has_src_addr(arguments_t args, SkBuff b)
{
	__be32 addr = get_arg_0(__be32, args);
	__be32 mask = get_arg_1(__be32, args);

	return has_src_addr(b, addr, mask);
}

static bool
pred_has_dst_addr(arguments_t args, SkBuff b)
{
	__be32 addr = get_arg_0(__be32, args);
	__be32 mask = get_arg_1(__be32, args);

	return has_dst_addr(b, addr, mask);
}

static bool
pred_is_frag(arguments_t args, SkBuff b)
{
        return  is_frag(b);
}

static bool
pred_is_first_frag(arguments_t args, SkBuff b)
{
        return  is_first_frag(b);
}

static bool
pred_is_more_frag(arguments_t args, SkBuff b)
{
        return  is_more_frag(b);
}

struct pfq_function_descr predicate_functions[] = {

        { "less", 	   "(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", less	},
        { "less_eq", 	   "(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", less_eq	},
        { "greater", 	   "(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", greater 	},
        { "greater_eq",    "(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", greater_eq },
        { "equal",  	   "(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", equal	},
        { "not_equal",     "(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", not_equal	},
        { "any_bit", 	   "(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", any_bit	},
        { "all_bit", 	   "(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", all_bit	},

        { "is_ip", 	   "SkBuff -> Bool", pred_is_ip    },
        { "is_tcp",        "SkBuff -> Bool", pred_is_tcp   },
        { "is_udp",        "SkBuff -> Bool", pred_is_udp   },
        { "is_icmp",       "SkBuff -> Bool", pred_is_icmp  },
        { "is_ip6",	   "SkBuff -> Bool", pred_is_ip6   },
        { "is_udp6",	   "SkBuff -> Bool", pred_is_udp6  },
        { "is_tcp6",       "SkBuff -> Bool", pred_is_tcp6  },
        { "is_icmp6",      "SkBuff -> Bool", pred_is_icmp6 },
        { "is_flow",       "SkBuff -> Bool", pred_is_flow  },
        { "has_vlan",      "SkBuff -> Bool", pred_has_vlan },
        { "is_frag", 	   "SkBuff -> Bool", pred_is_frag  },
        { "is_first_frag", "SkBuff -> Bool", pred_is_first_frag },
        { "is_more_frag",  "SkBuff -> Bool", pred_is_more_frag  },

        { "is_l3_proto",  "Word16  -> SkBuff -> Bool", pred_is_l3_proto  },
        { "is_l4_proto",  "Word8   -> SkBuff -> Bool", pred_is_l4_proto  },
        { "has_port",     "Word16 -> SkBuff -> Bool",  pred_has_port     },
        { "has_src_port", "Word16 -> SkBuff -> Bool",  pred_has_src_port },
        { "has_dst_port", "Word16 -> SkBuff -> Bool",  pred_has_dst_port },
        { "has_vid",      "Int -> SkBuff -> Bool",     pred_has_vid      },
        { "has_mark",     "CULong -> SkBuff -> Bool",  pred_has_mark     },

        { "has_addr",     "NetworkAddr -> SkBuff -> Bool", pred_has_addr     , pred_addr_init },
        { "has_src_addr", "NetworkAddr -> SkBuff -> Bool", pred_has_src_addr , pred_addr_init },
        { "has_dst_addr", "NetworkAddr -> SkBuff -> Bool", pred_has_dst_addr , pred_addr_init },

        { NULL }};

