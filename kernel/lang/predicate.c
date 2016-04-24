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

#include <pragma/diagnostic_push>

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/inetdevice.h>

#include <pragma/diagnostic_pop>

#include <lang/module.h>
#include <lang/predicate.h>


static bool
pred_is_ip(arguments_t args, SkBuff b)
{
        return  is_ip(b);
}

static bool
pred_is_udp(arguments_t args, SkBuff b)
{
        return  is_udp(b);
}

static bool
pred_is_tcp(arguments_t args, SkBuff b)
{
        return  is_tcp(b);
}

static bool
pred_is_icmp(arguments_t args, SkBuff b)
{
        return  is_icmp(b);
}

static bool
pred_is_flow(arguments_t args, SkBuff b)
{
        return  is_flow(b);
}

static bool
pred_is_l3_proto(arguments_t args, SkBuff b)
{
	const u16 type = GET_ARG(u16, args);
	return is_l3_proto(b, type);
}

static bool
pred_is_l4_proto(arguments_t args, SkBuff b)
{
	const u8 protocol = GET_ARG(u8, args);
	return is_l4_proto(b, protocol);
}

static bool
pred_has_port(arguments_t args, SkBuff b)
{
	const u16 port = GET_ARG(u16, args);
	return has_port(b, port);
}

static bool
pred_has_src_port(arguments_t args, SkBuff b)
{
	const u16 port = GET_ARG(u16, args);
	return has_src_port(b, port);
}

static bool
pred_has_dst_port(arguments_t args, SkBuff b)
{
	const u16 port = GET_ARG(u16, args);
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
	const int id = GET_ARG(int, args);
        return  has_vid(b, id);
}

static bool
pred_has_mark(arguments_t args, SkBuff b)
{
	const uint32_t value = GET_ARG(uint32_t, args);
	return get_mark(b) == value;
}

static bool
pred_has_state(arguments_t args, SkBuff b)
{
	const uint32_t value = GET_ARG(uint32_t, args);
	return get_state(b) == value;
}

static int pred_addr_init(arguments_t args)
{
	struct CIDR_ *data;
	CIDR_INIT(args, 0);
	data = GET_PTR_0(struct CIDR_, args);
	pr_devel("[PFQ|init] predicate: addr:%pI4 mask:%pI4\n", &data->addr, &data->mask);
	return 0;
}


static bool
pred_has_addr(arguments_t args, SkBuff b)
{
	struct CIDR_ *data = GET_PTR_0(struct CIDR_, args);
	return has_addr(b, data->addr, data->mask);
}


static bool
pred_has_src_addr(arguments_t args, SkBuff b)
{
	struct CIDR_ *data = GET_PTR_0(struct CIDR_, args);
	return has_src_addr(b, data->addr, data->mask);
}

static bool
pred_has_dst_addr(arguments_t args, SkBuff b)
{
	struct CIDR_ *data = GET_PTR_0(struct CIDR_, args);
	return has_dst_addr(b, data->addr, data->mask);
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

struct pfq_lang_function_descr predicate_functions[] = {

        { "less",	"(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", less	},
        { "less_eq",	"(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", less_eq	},
        { "greater",	"(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", greater	},
        { "greater_eq",	"(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", greater_eq },
        { "equal",	"(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", equal	},
        { "not_equal",     "(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", not_equal	},
        { "any_bit",	   "(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", any_bit	},
        { "all_bit",	   "(SkBuff -> Word64) -> Word64 -> SkBuff -> Bool", all_bit	},

        { "is_ip",	   "SkBuff -> Bool", pred_is_ip    },
        { "is_tcp",        "SkBuff -> Bool", pred_is_tcp   },
        { "is_udp",        "SkBuff -> Bool", pred_is_udp   },
        { "is_icmp",       "SkBuff -> Bool", pred_is_icmp  },
        { "is_flow",       "SkBuff -> Bool", pred_is_flow  },
        { "has_vlan",      "SkBuff -> Bool", pred_has_vlan },
        { "is_frag",	   "SkBuff -> Bool", pred_is_frag  },
        { "is_first_frag", "SkBuff -> Bool", pred_is_first_frag },
        { "is_more_frag",  "SkBuff -> Bool", pred_is_more_frag  },

        { "is_l3_proto",  "Word16 -> SkBuff -> Bool",  pred_is_l3_proto  },
        { "is_l4_proto",  "Word8  -> SkBuff -> Bool",  pred_is_l4_proto  },
        { "has_port",     "Word16 -> SkBuff -> Bool",  pred_has_port     },
        { "has_src_port", "Word16 -> SkBuff -> Bool",  pred_has_src_port },
        { "has_dst_port", "Word16 -> SkBuff -> Bool",  pred_has_dst_port },
        { "has_vid",      "CInt   -> SkBuff -> Bool",  pred_has_vid      },
        { "has_mark",     "Word32 -> SkBuff -> Bool",  pred_has_mark     },
        { "has_state",    "Word32 -> SkBuff -> Bool",  pred_has_state	 },

        { "has_addr",     "CIDR -> SkBuff -> Bool", pred_has_addr     , pred_addr_init },
        { "has_src_addr", "CIDR -> SkBuff -> Bool", pred_has_src_addr , pred_addr_init },
        { "has_dst_addr", "CIDR -> SkBuff -> Bool", pred_has_dst_addr , pred_addr_init },

        { NULL }};

