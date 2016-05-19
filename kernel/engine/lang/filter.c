/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#include <engine/lang/module.h>
#include <engine/lang/filter.h>
#include <engine/lang/types.h>

#include <pfq/printk.h>

static ActionQbuff
filter_generic(arguments_t args, struct qbuff * b)
{
	predicate_t pred_ = GET_ARG(predicate_t, args);

	if (EVAL_PREDICATE(pred_, b))
		return Pass(b);

	return Drop(b);
}

static ActionQbuff
filter_l3_proto(arguments_t args, struct qbuff * b)
{
	const uint16_t type = GET_ARG(uint16_t, args);
        return is_l3_proto(b, type) ? Pass(b) : Drop(b);
}

static ActionQbuff
filter_l4_proto(arguments_t args, struct qbuff * b)
{
	const uint8_t proto = GET_ARG(uint8_t, args);
        return is_l4_proto(b, proto) ? Pass(b) : Drop(b);
}

static ActionQbuff
filter_port(arguments_t args, struct qbuff * b)
{
	const uint16_t port = GET_ARG(uint16_t, args);
        return has_port(b, port) ? Pass(b) : Drop(b);
}

static ActionQbuff
filter_src_port(arguments_t args, struct qbuff * b)
{
	const uint16_t port = GET_ARG(uint16_t, args);
        return has_src_port(b, port) ? Pass(b) : Drop(b);
}

static ActionQbuff
filter_dst_port(arguments_t args, struct qbuff * b)
{
	const uint16_t port = GET_ARG(uint16_t, args);
        return has_dst_port(b, port) ? Pass(b) : Drop(b);
}



static int filter_addr_init(arguments_t args)
{
	struct CIDR_ *data;
	CIDR_INIT(args, 0);
	data = GET_PTR_0(struct CIDR_, args);
	pr_devel("[PFQ|init] filter: addr:%pI4 mask:%pI4\n", &data->addr, &data->mask);
	return 0;
}


static ActionQbuff
filter_addr(arguments_t args, struct qbuff * b)
{
	struct CIDR_ *data = GET_PTR_0(struct CIDR_, args);
	return has_addr(b, data->addr, data->mask) ? Pass(b) : Drop(b);
}


static ActionQbuff
filter_src_addr(arguments_t args, struct qbuff * b)
{
	struct CIDR_ *data = GET_PTR_0(struct CIDR_, args);
	return has_src_addr(b, data->addr, data->mask) ? Pass(b) : Drop(b);
}

static ActionQbuff
filter_dst_addr(arguments_t args, struct qbuff * b)
{
	struct CIDR_ *data = GET_PTR_0(struct CIDR_, args);
	return has_dst_addr(b, data->addr, data->mask) ? Pass(b) : Drop(b);
}

static ActionQbuff
filter_no_frag(arguments_t args, struct qbuff * b)
{
	return is_frag(b) ? Drop(b) : Pass(b);
}

static ActionQbuff
filter_no_more_frag(arguments_t args, struct qbuff * b)
{
	return is_more_frag(b) ? Drop(b) : Pass(b);
}


static ActionQbuff
filter_broadcast(arguments_t args, struct qbuff * b)
{
	return is_broadcast(b) ? Drop(b) : Pass(b);
}

static ActionQbuff
filter_multicast(arguments_t args, struct qbuff * b)
{
	return is_multicast(b) ? Drop(b) : Pass(b);
}


static ActionQbuff
filter_ip_broadcast(arguments_t args, struct qbuff * b)
{
	return is_ip_broadcast(b) ? Drop(b) : Pass(b);
}

static ActionQbuff
filter_ip_multicast(arguments_t args, struct qbuff * b)
{
	return is_ip_multicast(b) ? Drop(b) : Pass(b);
}


static ActionQbuff
filter_ip_host(arguments_t args, struct qbuff * b)
{
	return is_ip_host(b) ? Drop(b) : Pass(b);
}

static ActionQbuff
filter_incoming_host(arguments_t args, struct qbuff * b)
{
	return is_incoming_host(b) ? Drop(b) : Pass(b);
}


struct pfq_lang_function_descr filter_functions[] = {

        { "unit",	  "SkBuff -> Action SkBuff",	unit			},
        { "ip",           "SkBuff -> Action SkBuff",	filter_ip		},
        { "udp",          "SkBuff -> Action SkBuff",	filter_udp		},
        { "tcp",          "SkBuff -> Action SkBuff",	filter_tcp		},
        { "icmp",         "SkBuff -> Action SkBuff",	filter_icmp		},
        { "flow",         "SkBuff -> Action SkBuff",	filter_flow		},
        { "vlan",         "SkBuff -> Action SkBuff",	filter_vlan		},
	{ "no_frag",	  "SkBuff -> Action SkBuff",	filter_no_frag		},
	{ "no_more_frag", "SkBuff -> Action SkBuff",	filter_no_more_frag     },

        { "port",	  "Word16 -> SkBuff -> Action SkBuff",		 filter_port     },
        { "src_port",	  "Word16 -> SkBuff -> Action SkBuff",		 filter_src_port },
        { "dst_port",	  "Word16 -> SkBuff -> Action SkBuff",		 filter_dst_port },

        { "addr",	  "CIDR -> SkBuff -> Action SkBuff", filter_addr     , filter_addr_init },
        { "src_addr",	  "CIDR -> SkBuff -> Action SkBuff", filter_src_addr , filter_addr_init },
        { "dst_addr",	  "CIDR -> SkBuff -> Action SkBuff", filter_dst_addr , filter_addr_init },

	{ "l3_proto",     "Word16 -> SkBuff -> Action SkBuff",           filter_l3_proto },
        { "l4_proto",     "Word8  -> SkBuff -> Action SkBuff",           filter_l4_proto },
        { "filter",       "(SkBuff -> Bool) -> SkBuff -> Action SkBuff", filter_generic  },

	{ "mac_broadcast","SkBuff -> Action SkBuff",	filter_broadcast	},
	{ "mac_multicast","SkBuff -> Action SkBuff",	filter_multicast	},
	{ "incoming_host","SkBuff -> Action SkBuff",	filter_incoming_host	},
	{ "ip_host",	  "SkBuff -> Action SkBuff",	filter_ip_host		},
	{ "ip_broadcast", "SkBuff -> Action SkBuff",	filter_ip_broadcast	},
	{ "ip_multicast", "SkBuff -> Action SkBuff",	filter_ip_multicast	},

        { NULL }};

