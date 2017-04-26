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

#include <lang/module.h>
#include <lang/types.h>
#include <lang/predicate.h>

#include <pfq/printk.h>

static bool
pred_is_ip(arguments_t args, struct qbuff * b)
{
        return  is_ip(b);
}

static bool
pred_is_udp(arguments_t args, struct qbuff * b)
{
        return  is_udp(b);
}

static bool
pred_is_tcp(arguments_t args, struct qbuff * b)
{
        return  is_tcp(b);
}

static bool
pred_is_icmp(arguments_t args, struct qbuff * b)
{
        return  is_icmp(b);
}

static bool
pred_is_flow(arguments_t args, struct qbuff * b)
{
        return  is_flow(b);
}

static bool
pred_is_l3_proto(arguments_t args, struct qbuff * b)
{
	const uint16_t type = GET_ARG(uint16_t, args);
	return is_l3_proto(b, type);
}

static bool
pred_is_l4_proto(arguments_t args, struct qbuff * b)
{
	const uint8_t protocol = GET_ARG(uint8_t, args);
	return is_l4_proto(b, protocol);
}

static bool
pred_has_port(arguments_t args, struct qbuff * b)
{
	const uint16_t port = GET_ARG(uint16_t, args);
	return has_port(b, port);
}

static bool
pred_has_src_port(arguments_t args, struct qbuff * b)
{
	const uint16_t port = GET_ARG(uint16_t, args);
	return has_src_port(b, port);
}

static bool
pred_has_dst_port(arguments_t args, struct qbuff * b)
{
	const uint16_t port = GET_ARG(uint16_t, args);
	return has_dst_port(b, port);
}

static bool
pred_has_vlan(arguments_t args, struct qbuff * b)
{
        return  has_vlan(b);
}

static bool
pred_has_vid(arguments_t args, struct qbuff * b)
{
	const int id = GET_ARG(int, args);
        return  has_vid(b, id);
}

static bool
pred_has_mark(arguments_t args, struct qbuff * b)
{
	const uint32_t value = GET_ARG(uint32_t, args);
	return get_mark(b) == value;
}

static bool
pred_has_state(arguments_t args, struct qbuff * b)
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
pred_has_addr(arguments_t args, struct qbuff * b)
{
	struct CIDR_ *data = GET_PTR_0(struct CIDR_, args);
	return has_addr(b, data->addr, data->mask);
}


static bool
pred_has_src_addr(arguments_t args, struct qbuff * b)
{
	struct CIDR_ *data = GET_PTR_0(struct CIDR_, args);
	return has_src_addr(b, data->addr, data->mask);
}

static bool
pred_has_dst_addr(arguments_t args, struct qbuff * b)
{
	struct CIDR_ *data = GET_PTR_0(struct CIDR_, args);
	return has_dst_addr(b, data->addr, data->mask);
}

static bool
pred_is_frag(arguments_t args, struct qbuff * b)
{
        return  is_frag(b);
}

static bool
pred_is_first_frag(arguments_t args, struct qbuff * b)
{
        return  is_first_frag(b);
}

static bool
pred_is_more_frag(arguments_t args, struct qbuff * b)
{
        return  is_more_frag(b);
}

static bool
pred_is_broadcast(arguments_t args, struct qbuff * b)
{
	return is_broadcast(b);
}

static bool
pred_is_multicast(arguments_t args, struct qbuff * b)
{
	return is_multicast(b);
}

static bool
pred_is_ip_host(arguments_t args, struct qbuff * b)
{
	return is_ip_host(b);
}

static bool
pred_is_ip_broadcast(arguments_t args, struct qbuff * b)
{
	return is_ip_broadcast(b);
}

static bool
pred_is_ip_multicast(arguments_t args, struct qbuff * b)
{
	return is_ip_broadcast(b);
}

static bool
pred_is_incoming_host(arguments_t args, struct qbuff * b)
{
	return is_incoming_host(b);
}

struct pfq_lang_function_descr predicate_functions[] = {

        { "less",	"(Qbuff -> Word64) -> Word64 -> Qbuff -> Bool", less	   , NULL, NULL },
        { "less_eq",	"(Qbuff -> Word64) -> Word64 -> Qbuff -> Bool", less_eq	   , NULL, NULL },
        { "greater",	"(Qbuff -> Word64) -> Word64 -> Qbuff -> Bool", greater	   , NULL, NULL },
        { "greater_eq",	"(Qbuff -> Word64) -> Word64 -> Qbuff -> Bool", greater_eq , NULL, NULL },
        { "equal",	"(Qbuff -> Word64) -> Word64 -> Qbuff -> Bool", equal	   , NULL, NULL },
        { "not_equal",  "(Qbuff -> Word64) -> Word64 -> Qbuff -> Bool", not_equal  , NULL, NULL },
        { "any_bit",	"(Qbuff -> Word64) -> Word64 -> Qbuff -> Bool", any_bit	   , NULL, NULL },
        { "all_bit",	"(Qbuff -> Word64) -> Word64 -> Qbuff -> Bool", all_bit	   , NULL, NULL },

        { "is_ip",	   "Qbuff -> Bool", pred_is_ip	       , NULL, NULL },
        { "is_tcp",        "Qbuff -> Bool", pred_is_tcp	       , NULL, NULL },
        { "is_udp",        "Qbuff -> Bool", pred_is_udp	       , NULL, NULL },
        { "is_icmp",       "Qbuff -> Bool", pred_is_icmp       , NULL, NULL },
        { "is_flow",       "Qbuff -> Bool", pred_is_flow       , NULL, NULL },
        { "has_vlan",      "Qbuff -> Bool", pred_has_vlan      , NULL, NULL },
        { "is_frag",	   "Qbuff -> Bool", pred_is_frag       , NULL, NULL },
        { "is_first_frag", "Qbuff -> Bool", pred_is_first_frag , NULL, NULL },
        { "is_more_frag",  "Qbuff -> Bool", pred_is_more_frag  , NULL, NULL },

        { "is_l3_proto",  "Word16 -> Qbuff -> Bool",  pred_is_l3_proto  , NULL, NULL },
        { "is_l4_proto",  "Word8  -> Qbuff -> Bool",  pred_is_l4_proto  , NULL, NULL },
        { "has_port",     "Word16 -> Qbuff -> Bool",  pred_has_port     , NULL, NULL },
        { "has_src_port", "Word16 -> Qbuff -> Bool",  pred_has_src_port , NULL, NULL },
        { "has_dst_port", "Word16 -> Qbuff -> Bool",  pred_has_dst_port , NULL, NULL },
        { "has_vid",      "CInt   -> Qbuff -> Bool",  pred_has_vid      , NULL, NULL },
        { "has_mark",     "Word32 -> Qbuff -> Bool",  pred_has_mark     , NULL, NULL },
        { "has_state",    "Word32 -> Qbuff -> Bool",  pred_has_state	, NULL, NULL },

        { "has_addr",     "CIDR -> Qbuff -> Bool", pred_has_addr     , pred_addr_init , NULL},
        { "has_src_addr", "CIDR -> Qbuff -> Bool", pred_has_src_addr , pred_addr_init , NULL},
        { "has_dst_addr", "CIDR -> Qbuff -> Bool", pred_has_dst_addr , pred_addr_init , NULL},

        { "is_broadcast",    "Qbuff -> Bool",  pred_is_broadcast	, NULL, NULL },
        { "is_multicast",    "Qbuff -> Bool",  pred_is_multicast	, NULL, NULL },
        { "is_incoming_host","Qbuff -> Bool",  pred_is_incoming_host	, NULL, NULL },
        { "is_ip_host",      "Qbuff -> Bool",  pred_is_ip_host		, NULL, NULL },
        { "is_ip_broadcast", "Qbuff -> Bool",  pred_is_ip_broadcast	, NULL, NULL },
        { "is_ip_multicast", "Qbuff -> Bool",  pred_is_ip_multicast	, NULL, NULL },

        { NULL }};

