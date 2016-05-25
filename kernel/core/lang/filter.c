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

#include <core/lang/module.h>
#include <core/lang/filter.h>
#include <core/lang/types.h>

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

        { "unit",	  "Qbuff -> Action Qbuff",	unit		     , NULL, NULL   },
        { "ip",           "Qbuff -> Action Qbuff",	filter_ip	     , NULL, NULL   },
        { "udp",          "Qbuff -> Action Qbuff",	filter_udp	     , NULL, NULL   },
        { "tcp",          "Qbuff -> Action Qbuff",	filter_tcp	     , NULL, NULL   },
        { "icmp",         "Qbuff -> Action Qbuff",	filter_icmp	     , NULL, NULL   },
        { "flow",         "Qbuff -> Action Qbuff",	filter_flow	     , NULL, NULL   },
        { "vlan",         "Qbuff -> Action Qbuff",	filter_vlan	     , NULL, NULL   },
	{ "no_frag",	  "Qbuff -> Action Qbuff",	filter_no_frag	     , NULL, NULL   },
	{ "no_more_frag", "Qbuff -> Action Qbuff",	filter_no_more_frag  , NULL, NULL   },

        { "port",	  "Word16 -> Qbuff -> Action Qbuff", filter_port     , NULL, NULL   },
        { "src_port",	  "Word16 -> Qbuff -> Action Qbuff", filter_src_port , NULL, NULL   },
        { "dst_port",	  "Word16 -> Qbuff -> Action Qbuff", filter_dst_port , NULL, NULL   },

        { "addr",	  "CIDR -> Qbuff -> Action Qbuff", filter_addr     , filter_addr_init , NULL},
        { "src_addr",	  "CIDR -> Qbuff -> Action Qbuff", filter_src_addr , filter_addr_init , NULL},
        { "dst_addr",	  "CIDR -> Qbuff -> Action Qbuff", filter_dst_addr , filter_addr_init , NULL},

	{ "l3_proto",     "Word16 -> Qbuff -> Action Qbuff",           filter_l3_proto , NULL, NULL},
        { "l4_proto",     "Word8  -> Qbuff -> Action Qbuff",           filter_l4_proto , NULL, NULL},
        { "filter",       "(Qbuff -> Bool) -> Qbuff -> Action Qbuff",  filter_generic  , NULL, NULL},

	{ "mac_broadcast","Qbuff -> Action Qbuff",	filter_broadcast     , NULL, NULL  },
	{ "mac_multicast","Qbuff -> Action Qbuff",	filter_multicast     , NULL, NULL  },
	{ "incoming_host","Qbuff -> Action Qbuff",	filter_incoming_host , NULL, NULL  },
	{ "ip_host",	  "Qbuff -> Action Qbuff",	filter_ip_host	     , NULL, NULL  },
	{ "ip_broadcast", "Qbuff -> Action Qbuff",	filter_ip_broadcast  , NULL, NULL  },
	{ "ip_multicast", "Qbuff -> Action Qbuff",	filter_ip_multicast  , NULL, NULL  },

        { NULL }};

