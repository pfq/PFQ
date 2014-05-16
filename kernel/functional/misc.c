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

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/crc16.h>

#include <pf_q-module.h>
#include <pf_q-sparse.h>

#include "inline.h"
#include "misc.h"

static struct sk_buff *
dummy(arguments_t args, struct sk_buff *skb)
{
        const int data = get_data(int,args);

        if (printk_ratelimit()) {
                printk(KERN_INFO "[PFQ] dummy context: %d\n", data);
        }

        return skb;
}

static int
dummy_init(arguments_t args)
{
	printk(KERN_INFO "[PFQ] %s :)\n", __PRETTY_FUNCTION__);
	return 0;
}

static int
dummy_fini(arguments_t args)
{
	printk(KERN_INFO "[PFQ] %s :(\n", __PRETTY_FUNCTION__);
	return 0;
}


static struct sk_buff *
inc_counter(arguments_t args, struct sk_buff *skb)
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

static struct sk_buff *
dec_counter(arguments_t args, struct sk_buff *skb)
{
        const int idx = get_data(int,args);

        sparse_counter_t * ctr;

        ctr = get_counter(skb, idx);
        if (ctr)  {
                sparse_dec(ctr);
        }
        else {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] fun/count(%d): bad index!\n", idx);
        }

        return skb;
}


static struct sk_buff *
crc16_sum(arguments_t args, struct sk_buff *skb)
{
	u16 crc = crc16(0, (u8 const *)eth_hdr(skb), skb->len);
	set_state(skb, crc);

        return skb;
}



struct pfq_function_descr misc_functions[] = {

        { "inc", 	FUN_ACTION | FUN_ARG_DATA , inc_counter },
        { "dec", 	FUN_ACTION | FUN_ARG_DATA , dec_counter },

        { "dummy",      FUN_ACTION | FUN_ARG_DATA , dummy, dummy_init,  dummy_fini },
 	{ "mark", 	FUN_ACTION | FUN_ARG_DATA , INLINE_FUN(mark) },

        { "crc16", 	FUN_ACTION, crc16_sum},

        { NULL }};


