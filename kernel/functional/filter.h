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

#ifndef _FUNCTIONAL_FILTER_H_
#define _FUNCTIONAL_FILTER_H_

#include <pf_q-module.h>

#include "predicate.h"


static inline struct sk_buff *
filter_ip(arguments_t args, struct sk_buff *skb)
{
        return is_ip(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_ip6(arguments_t args, struct sk_buff *skb)
{
        return is_ip6(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_udp(arguments_t args, struct sk_buff *skb)
{
        return is_udp(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_udp6(arguments_t args, struct sk_buff *skb)
{
        return is_udp6(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_tcp(arguments_t args, struct sk_buff *skb)
{
        return is_tcp(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_tcp6(arguments_t args, struct sk_buff *skb)
{
        return is_tcp6(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_icmp(arguments_t args, struct sk_buff *skb)
{
        return is_icmp(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_icmp6(arguments_t args, struct sk_buff *skb)
{
        return is_icmp6(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_flow(arguments_t args, struct sk_buff *skb)
{
        return is_flow(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_vlan(arguments_t args, struct sk_buff *skb)
{
        return has_vlan(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
unit(arguments_t args, struct sk_buff *skb)
{
        return skb;
}

#endif /* _FUNCTIONAL_FILTER_H_ */
