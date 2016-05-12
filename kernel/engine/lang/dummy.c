/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
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

#include <engine/lang/module.h>
#include <engine/lang/headers.h>
#include <engine/lang/misc.h>
#include <engine/lang/types.h>


#include <pf_q-sparse.h>

static ActionSkBuff
dummy(arguments_t args, SkBuff skb)
{
        const int data = GET_ARG(int,args);

	SkBuff nskb;

	printk(KERN_INFO "[pfq-lang] dummy = %d\n", data);

        nskb = pfq_lang_copy_buff(skb);

	if (nskb == NULL) {
                printk(KERN_INFO "[pfq-lang] clone error!!!\n");
                return Drop(skb);
	}

        printk(KERN_INFO "[pfq-lang] packet cloned: %p -> %p\n", nskb, skb);

        return Pass(nskb);
}


static ActionSkBuff
dummy_ip(arguments_t args, SkBuff skb)
{
	__be32  ipv4 = GET_ARG_0(__be32, args);
	printk(KERN_INFO "[pfq-lang] ip:%pI4\n", &ipv4);
	return Pass (skb);
}


static ActionSkBuff
dummy_vector(arguments_t args, SkBuff skb)
{
        const int *data = GET_ARRAY(int,args);
	size_t n, len = LEN_ARRAY(args);

	printk(KERN_INFO "[pfq-lang] dummy: vector len: %zu...\n", len);

	for(n = 0; n < len; n++)
	{
		printk(KERN_INFO "[pfq-lang] data[%zu] = %d\n", n, data[n]);
	}

        return Pass(skb);
}


static ActionSkBuff
dummy_string(arguments_t args, SkBuff skb)
{
        const char *data = GET_ARG(const char *,args);

	printk(KERN_INFO "[pfq-lang] dummy: string: %s\n", data);

        return Pass(skb);
}


static ActionSkBuff
dummy_strings(arguments_t args, SkBuff skb)
{
        const char **data = GET_ARRAY(const char *,args);
	size_t n, len = LEN_ARRAY(args);

	printk(KERN_INFO "[pfq-lang] dummy: vector strings len: %zu...\n", len);

	for(n = 0; n < len; n++)
	{
		printk(KERN_INFO "[pfq-lang] string[%zu]: %s\n", n, data[n]);
	}

        return Pass(skb);
}


static ActionSkBuff
dummy_cidr(arguments_t args, SkBuff skb)
{
	struct CIDR na = GET_ARG_0(struct CIDR, args);
	printk(KERN_INFO "[pfq-lang] (addr:%pI4,%d)\n", &na.addr, na.prefix);
	return Pass (skb);
}

static ActionSkBuff
dummy_cidrs(arguments_t args, SkBuff skb)
{
        const struct CIDR *data = GET_ARRAY(struct CIDR, args);
	size_t n, len = LEN_ARRAY(args);

	for(n = 0; n < len; n++)
	{
		printk(KERN_INFO "[pfq-lang] (addr:%pI4,%d)\n", &data[n].addr, data[n].prefix);
	}
	return Pass (skb);
}


static int
dummy_init(arguments_t args)
{
	printk(KERN_INFO "[pfq-lang] %s :)\n", __PRETTY_FUNCTION__);
	return 0;
}

static int
dummy_fini(arguments_t args)
{
	printk(KERN_INFO "[pfq-lang] %s :(\n", __PRETTY_FUNCTION__);
	return 0;
}



struct pfq_lang_function_descr dummy_functions[] = {

        { "dummy",         "CInt   -> SkBuff -> Action SkBuff",	  dummy, dummy_init,	     dummy_fini },
        { "dummy_ip",      "Word32 -> SkBuff -> Action SkBuff",	  dummy_ip,	dummy_init,  dummy_fini },
        { "dummy_cidr",	   "CIDR   -> SkBuff -> Action SkBuff",   dummy_cidr,	dummy_init,  dummy_fini },
        { "dummy_cidrs",   "[CIDR] -> SkBuff -> Action SkBuff",   dummy_cidrs,	dummy_init,  dummy_fini },
        { "dummy_vector",  "[CInt] -> SkBuff -> Action SkBuff",	  dummy_vector, dummy_init,  dummy_fini },
        { "dummy_string",  "String -> SkBuff -> Action SkBuff",	  dummy_string, dummy_init,  dummy_fini },
        { "dummy_strings", "[String] -> SkBuff -> Action SkBuff", dummy_strings, dummy_init, dummy_fini },

        { NULL }};

