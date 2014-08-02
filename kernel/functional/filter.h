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

#ifndef _FUNCTIONAL_FILTER_H_
#define _FUNCTIONAL_FILTER_H_

#include <pf_q-module.h>

#include "predicate.h"


static inline Action_SkBuff
filter_ip(arguments_t args, SkBuff b)
{
        return is_ip(b) ? Pass(b) : Drop(b);
}

static inline Action_SkBuff
filter_ip6(arguments_t args, SkBuff b)
{
        return is_ip6(b) ? Pass(b) : Drop(b);
}

static inline Action_SkBuff
filter_udp(arguments_t args, SkBuff b)
{
        return is_udp(b) ? Pass(b) : Drop(b);
}

static inline Action_SkBuff
filter_udp6(arguments_t args, SkBuff b)
{
        return is_udp6(b) ? Pass(b) : Drop(b);
}

static inline Action_SkBuff
filter_tcp(arguments_t args, SkBuff b)
{
        return is_tcp(b) ? Pass(b) : Drop(b);
}

static inline Action_SkBuff
filter_tcp6(arguments_t args, SkBuff b)
{
        return is_tcp6(b) ?  Pass(b) : Drop(b);
}

static inline Action_SkBuff
filter_icmp(arguments_t args, SkBuff b)
{
        return is_icmp(b) ? Pass(b) : Drop(b);
}

static inline Action_SkBuff
filter_icmp6(arguments_t args, SkBuff b)
{
        return is_icmp6(b) ? Pass(b) : Drop(b);
}

static inline Action_SkBuff
filter_flow(arguments_t args, SkBuff b)
{
        return is_flow(b) ? Pass(b) : Drop(b);
}

static inline Action_SkBuff
filter_vlan(arguments_t args, SkBuff b)
{
        return has_vlan(b) ? Pass(b) : Drop(b);
}

static inline Action_SkBuff
unit(arguments_t args, SkBuff b)
{
        return Pass(b);
}

#endif /* _FUNCTIONAL_FILTER_H_ */
