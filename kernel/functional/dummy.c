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

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/crc16.h>

#include <pf_q-module.h>
#include <pf_q-sparse.h>

#include "headers.h"
#include "misc.h"


static Action_SkBuff
dummy(arguments_t args, SkBuff b)
{
        const int data = get_arg(int,args);

	SkBuff new;

	printk(KERN_INFO "[PFQ/lang] dummy = %d\n", data);

        new = pfq_copy_buff(b);

	if (new.skb == NULL) {
                printk(KERN_INFO "[PFQ/lang] clone error!!!\n");
                return Drop(b);
	}

        printk(KERN_INFO "[PFQ/lang] packet cloned: %p -> %p\n", new.skb, b.skb);

        return Pass(new);
}


static Action_SkBuff
dummy_vector(arguments_t args, SkBuff b)
{
        const int *data = get_array(int,args);
	size_t n, len = get_array_len(args);

	printk(KERN_INFO "[PFQ/lang] dummy: vector len: %zu...\n", len);

	for(n = 0; n < len; n++)
	{
		printk(KERN_INFO "[PFQ/lang]  data[%zu] = %d\n", n, data[n]);
	}

        return Pass(b);
}


static Action_SkBuff
dummy_string(arguments_t args, SkBuff b)
{
        const char *data = get_arg(const char *,args);

	printk(KERN_INFO "[PFQ/lang] dummy: vector string: %s\n", data);

        return Pass(b);
}


static Action_SkBuff
dummy_strings(arguments_t args, SkBuff b)
{
        const char **data = get_array(const char *,args);
	size_t n, len = get_array_len(args);

	printk(KERN_INFO "[PFQ/lang] dummy: vector strings len: %zu...\n", len);

	for(n = 0; n < len; n++)
	{
		printk(KERN_INFO "[PFQ/lang] string[%zu]: %s\n", n, data[n]);
	}

        return Pass(b);
}


static int
dummy_init(arguments_t args)
{
	printk(KERN_INFO "[PFQ/lang] %s :)\n", __PRETTY_FUNCTION__);
	return 0;
}

static int
dummy_fini(arguments_t args)
{
	printk(KERN_INFO "[PFQ/lang] %s :(\n", __PRETTY_FUNCTION__);
	return 0;
}



struct pfq_function_descr dummy_functions[] = {

        { "dummy",         "CInt   -> SkBuff -> Action SkBuff",  	dummy, dummy_init,  dummy_fini },
        { "dummy_vector",  "[CInt] -> SkBuff -> Action SkBuff",     	dummy_vector, dummy_init,  dummy_fini },
        { "dummy_string",  "String -> SkBuff -> Action SkBuff",     	dummy_string, dummy_init,  dummy_fini },
        { "dummy_strings", "[String] -> SkBuff -> Action SkBuff",     	dummy_strings, dummy_init,  dummy_fini },

        { NULL }};

