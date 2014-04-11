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


#include <linux/pf_q-module.h>


static struct sk_buff *
forward_legacy(argument_t a, struct sk_buff *skb)
{
        return to_kernel(drop(skb));
}


static struct sk_buff *
forward_broadcast(argument_t a, struct sk_buff *skb)
{
        return broadcast(skb);
}


static struct sk_buff *
forward_class(argument_t a, struct sk_buff *skb)
{
        int *c = argument_as(int, a);

        if (!c) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] fun/class: internal error!\n");
                return skb;
        }
        return class(skb, (1ULL << *c));
}


static struct sk_buff *
forward_sink(argument_t a, struct sk_buff *skb)
{
        if (!is_stolen(skb))
        {
                kfree_skb(skb);
        }
        return NULL;
}

static struct sk_buff *
forward_drop(argument_t a, struct sk_buff *skb)
{
        return drop(skb);
}


struct pfq_function_descr forward_functions[] = {

        { "legacy",             forward_legacy          },
        { "broadcast",          forward_broadcast       },
        { "class",              forward_class           },
        { "sink",               forward_sink            },
        { "drop",               forward_drop            },

        { NULL, NULL}};

