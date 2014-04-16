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
#include <linux/pf_q-sparse.h>


static struct sk_buff *
dummy(argument_t a, struct sk_buff *skb)
{
        const int *ptr = argument_as(int,a);

        if (printk_ratelimit()) {
                printk(KERN_INFO "[PFQ] fun/dummy context: %d\n", ptr ? *ptr : 0);
        }

        return copy(skb);
}


static struct sk_buff *
counter(argument_t a, struct sk_buff *skb)
{
        const int *idx = argument_as(int,a);

        sparse_counter_t * ctr;

        if (idx == NULL) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] fun/counter: internal error!\n");
                return skb;
        }

        ctr = get_counter(skb, *idx);
        if (ctr)  {
                sparse_inc(ctr);
        }
        else {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] fun/count(%d): bad index!\n", *idx);
        }

        return skb;
}


static struct sk_buff *
mark(argument_t a, struct sk_buff *skb)
{
	const unsigned long *value = argument_as(unsigned long, a);
	set_state(skb, *value);
	return skb;
}

static struct sk_buff *
conditional(argument_t a, struct sk_buff *skb)
{
        expression_t * expr = expression(a);
        PFQ_CB(skb)->right = expr->ptr(skb, expr);
        return skb;
}

static struct sk_buff *
when(argument_t a, struct sk_buff *skb)
{
        expression_t * expr = expression(a);
        PFQ_CB(skb)->right = expr->ptr(skb, expr);
        return skb;
}

static struct sk_buff *
unless(argument_t a, struct sk_buff *skb)
{
        expression_t * expr = expression(a);
        PFQ_CB(skb)->right = !expr->ptr(skb, expr);
        return skb;
}


struct pfq_function_descr misc_functions[] = {

        { "counter",            counter         },
	{ "mark", 		mark 		},
        { "dummy",              dummy           },
        { "conditional",        conditional     },
        { "when",             	when 		},
        { "unless",             unless          },

        { NULL, NULL}};


