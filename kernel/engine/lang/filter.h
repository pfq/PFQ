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

#ifndef PFQ_LANG_FILTER_H
#define PFQ_LANG_FILTER_H

#include <engine/lang/module.h>
#include <engine/lang/predicate.h>


static inline ActionQbuff
filter_ip(arguments_t args, struct qbuff * b)
{
        return is_ip(b) ? Pass(b) : Drop(b);
}

static inline ActionQbuff
filter_udp(arguments_t args, struct qbuff * b)
{
        return is_udp(b) ? Pass(b) : Drop(b);
}

static inline ActionQbuff
filter_tcp(arguments_t args, struct qbuff * b)
{
        return is_tcp(b) ? Pass(b) : Drop(b);
}

static inline ActionQbuff
filter_icmp(arguments_t args, struct qbuff * b)
{
        return is_icmp(b) ? Pass(b) : Drop(b);
}

static inline ActionQbuff
filter_flow(arguments_t args, struct qbuff * b)
{
        return is_flow(b) ? Pass(b) : Drop(b);
}

static inline ActionQbuff
filter_vlan(arguments_t args, struct qbuff * b)
{
        return has_vlan(b) ? Pass(b) : Drop(b);
}

static inline ActionQbuff
unit(arguments_t args, struct qbuff * b)
{
        return Pass(b);
}

#endif /* PFQ_LANG_FILTER_H */
