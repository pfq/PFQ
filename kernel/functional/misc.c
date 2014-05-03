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

#include <pf_q-module.h>
#include <pf_q-sparse.h>

#include "inline.h"

static struct sk_buff *
dummy(arguments_t args, struct sk_buff *skb)
{
        const int data = get_data(int,args);

        if (printk_ratelimit()) {
                printk(KERN_INFO "[PFQ] dummy context: %d\n", data);
        }

        return skb;
}


static struct sk_buff *
counter(arguments_t args, struct sk_buff *skb)
{
        const int idx = get_data(int,args);

        sparse_counter_t * ctr;

        ctr = get_counter(skb, idx);
        if (ctr)  {
                sparse_inc(ctr);
        }
        else {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] fun/count(%d): bad index!\n", idx);
        }

        return skb;
}


static inline struct sk_buff *
mark(arguments_t args, struct sk_buff *skb)
{
	const unsigned long value = get_data(unsigned long, args);
	set_state(skb, value);
	return skb;
}


struct pfq_function_descr misc_functions[] = {

        { "counter", 	counter 	, FUN_ACTION | FUN_ARG_DATA },
        { "dummy",      dummy   	, FUN_ACTION | FUN_ARG_DATA },
 	{ "mark", 	mark		, FUN_ACTION | FUN_ARG_DATA },

        { NULL, NULL}};


