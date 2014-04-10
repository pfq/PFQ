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


static struct sk_buff *
id(struct sk_buff *skb, argument_t a)
{
        return skb;
}


static struct sk_buff *
filter_ip(struct sk_buff *skb, argument_t a)
{
        return is_ip(skb) ? skb : drop(skb);
}


static struct sk_buff *
filter_ipv6(struct sk_buff *skb, argument_t a)
{
        return is_ipv6(skb) ? skb : drop(skb);
}


static struct sk_buff *
filter_udp(struct sk_buff *skb, argument_t a)
{
        return is_udp(skb) ? skb : drop(skb);
}


static struct sk_buff *
filter_tcp(struct sk_buff *skb, argument_t a)
{
        return is_tcp(skb) ? skb : drop(skb);
}


static struct sk_buff *
filter_icmp(struct sk_buff *skb, argument_t a)
{
        return is_icmp(skb) ? skb : drop(skb);
}


static struct sk_buff *
filter_flow(struct sk_buff *skb, argument_t a)
{
        return has_flow(skb) ? skb : drop(skb);
}


static struct sk_buff *
filter_vlan(struct sk_buff *skb, argument_t a)
{
        return has_vlan(skb) ? skb : drop(skb);
}


struct pfq_function_descr filter_functions[] = {

        { "id",                 id                      },
        { "ip",                 filter_ip               },
        { "ipv6",               filter_ipv6             },
        { "udp",                filter_udp              },
        { "tcp",                filter_tcp              },
        { "icmp",               filter_icmp             },
        { "flow",               filter_flow             },
        { "vlan",               filter_vlan             },

        { NULL, NULL}};

