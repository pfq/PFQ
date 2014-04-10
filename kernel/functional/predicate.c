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
#include <pf_q-predicate.h>

#include <linux/pf_q-module.h>

static bool
__is_ip(struct sk_buff const *skb, argument_t a)
{
        return  is_ip(skb);
}

static bool
__is_ipv6(struct sk_buff const *skb, argument_t a)
{
        return  is_ipv6(skb);
}

static bool
__is_udp(struct sk_buff const *skb, argument_t a)
{
        return  is_udp(skb);
}

static bool
__is_tcp(struct sk_buff const *skb, argument_t a)
{
        return  is_tcp(skb);
}

static bool
__is_icmp(struct sk_buff const *skb, argument_t a)
{
        return  is_icmp(skb);
}

static bool
__has_flow(struct sk_buff const *skb, argument_t a)
{
        return  has_flow(skb);
}

static bool
__has_vlan(struct sk_buff const *skb, argument_t a)
{
        return  has_vlan(skb);
}


struct pfq_predicate_fun_descr predicate_functions[] = {

        { "is_ip",      __is_ip         },
        { "is_ipv6",    __is_ipv6       },
        { "is_udp",     __is_udp        },
        { "is_tcp",     __is_tcp        },
        { "is_icmp",    __is_icmp       },
        { "has_flow",   __has_flow      },
        { "has_vlan",   __has_vlan      },

        { NULL, NULL}};

