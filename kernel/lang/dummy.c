/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#include <lang/module.h>
#include <lang/headers.h>
#include <lang/misc.h>
#include <lang/types.h>


#include <pfq/sparse.h>

static ActionQbuff
dummy(arguments_t args, struct qbuff * buff)
{
        const int data = GET_ARG(int,args);

	printk(KERN_INFO "[pfq-lang] dummy = %d\n", data);

        return Pass(buff);
}


static ActionQbuff
dummy_ip(arguments_t args, struct qbuff * buff)
{
	__be32  ipv4 = GET_ARG_0(__be32, args);
	printk(KERN_INFO "[pfq-lang] ip:%pI4\n", &ipv4);
	return Pass (buff);
}


static ActionQbuff
dummy_vector(arguments_t args, struct qbuff * buff)
{
        const int *data = GET_ARRAY(int,args);
	size_t n, len = LEN_ARRAY(args);

	printk(KERN_INFO "[pfq-lang] dummy: vector len: %zu...\n", len);

	for(n = 0; n < len; n++)
	{
		printk(KERN_INFO "[pfq-lang] data[%zu] = %d\n", n, data[n]);
	}

        return Pass(buff);
}


static ActionQbuff
dummy_string(arguments_t args, struct qbuff * buff)
{
        const char *data = GET_ARG(const char *,args);

	printk(KERN_INFO "[pfq-lang] dummy: string: %s\n", data);

        return Pass(buff);
}


static ActionQbuff
dummy_strings(arguments_t args, struct qbuff * buff)
{
        const char **data = GET_ARRAY(const char *,args);
	size_t n, len = LEN_ARRAY(args);

	printk(KERN_INFO "[pfq-lang] dummy: vector strings len: %zu...\n", len);

	for(n = 0; n < len; n++)
	{
		printk(KERN_INFO "[pfq-lang] string[%zu]: %s\n", n, data[n]);
	}

        return Pass(buff);
}


static ActionQbuff
dummy_cidr(arguments_t args, struct qbuff * buff)
{
	struct CIDR na = GET_ARG_0(struct CIDR, args);
	printk(KERN_INFO "[pfq-lang] (addr:%pI4,%d)\n", &na.addr, na.prefix);
	return Pass (buff);
}

static ActionQbuff
dummy_cidrs(arguments_t args, struct qbuff * buff)
{
        const struct CIDR *data = GET_ARRAY(struct CIDR, args);
	size_t n, len = LEN_ARRAY(args);

	for(n = 0; n < len; n++)
	{
		printk(KERN_INFO "[pfq-lang] (addr:%pI4,%d)\n", &data[n].addr, data[n].prefix);
	}
	return Pass (buff);
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

        { "dummy",         "CInt   -> Qbuff -> Action Qbuff",	  dummy, dummy_init,	     dummy_fini },
        { "dummy_ip",      "Word32 -> Qbuff -> Action Qbuff",	  dummy_ip,	dummy_init,  dummy_fini },
        { "dummy_cidr",	   "CIDR   -> Qbuff -> Action Qbuff",   dummy_cidr,	dummy_init,  dummy_fini },
        { "dummy_cidrs",   "[CIDR] -> Qbuff -> Action Qbuff",   dummy_cidrs,	dummy_init,  dummy_fini },
        { "dummy_vector",  "[CInt] -> Qbuff -> Action Qbuff",	  dummy_vector, dummy_init,  dummy_fini },
        { "dummy_string",  "String -> Qbuff -> Action Qbuff",	  dummy_string, dummy_init,  dummy_fini },
        { "dummy_strings", "[String] -> Qbuff -> Action Qbuff", dummy_strings, dummy_init, dummy_fini },

        { NULL }};

