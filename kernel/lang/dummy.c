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
dummy_int(arguments_t args, struct qbuff * buff)
{
        const int data = GET_ARG(int,args);
	printk(KERN_INFO "[pfq-lang] dummy_int: %d\n", data);
        return Pass(buff);
}

static int
dummy_init_int(arguments_t args)
{
	size_t len = LEN_ARRAY(args);
	printk(KERN_INFO "[pfq-lang] %s (len: %zu)!\n", __PRETTY_FUNCTION__, len);
	dummy_int(args, NULL);
	return 0;
}

static ActionQbuff
dummy_ints(arguments_t args, struct qbuff * buff)
{
        const int *data = GET_ARRAY(int,args);
	size_t n, len = LEN_ARRAY(args);

	printk(KERN_INFO "[pfq-lang] dummy_ints: len: %zu...\n", len);

	for(n = 0; n < len; n++)
	{
		printk(KERN_INFO "[pfq-lang] data[%zu] = %d\n", n, data[n]);
	}

        return Pass(buff);
}

static int
dummy_init_ints(arguments_t args)
{
	size_t len = LEN_ARRAY(args);
	printk(KERN_INFO "[pfq-lang] %s (len: %zu)!\n", __PRETTY_FUNCTION__, len);
	dummy_ints(args, NULL);
	return 0;
}


static ActionQbuff
dummy_ip(arguments_t args, struct qbuff * buff)
{
	__be32  ipv4 = GET_ARG_0(__be32, args);
	printk(KERN_INFO "[pfq-lang] ip:%pI4\n", &ipv4);
	return Pass (buff);
}

static int
dummy_init_ip(arguments_t args)
{
	size_t len = LEN_ARRAY(args);
	printk(KERN_INFO "[pfq-lang] %s (len: %zu)!\n", __PRETTY_FUNCTION__, len);
	dummy_ip(args, NULL);
	return 0;
}


static ActionQbuff
dummy_ips(arguments_t args, struct qbuff * buff)
{
        const int *ips = GET_ARRAY(__be32, args);
	size_t n, len = LEN_ARRAY(args);

	printk(KERN_INFO "[pfq-lang] dummy: ips len: %zu...\n", len);

	for(n = 0; n < len; n++)
	{
	        printk(KERN_INFO "[pfq-lang] ip[%zu]:%pI4\n", n, &ips[n]);
	}

        return Pass(buff);
}

static int
dummy_init_ips(arguments_t args)
{
	size_t len = LEN_ARRAY(args);
	printk(KERN_INFO "[pfq-lang] %s (len: %zu)!\n", __PRETTY_FUNCTION__, len);
	dummy_ips(args, NULL);
	return 0;
}


static ActionQbuff
dummy_string(arguments_t args, struct qbuff * buff)
{
        const char *data = GET_ARG(const char *,args);
	printk(KERN_INFO "[pfq-lang] dummy: string: %s\n", data);
        return Pass(buff);
}

static int
dummy_init_string(arguments_t args)
{
	size_t len = LEN_ARRAY(args);
	printk(KERN_INFO "[pfq-lang] %s (len: %zu)!\n", __PRETTY_FUNCTION__, len);
	dummy_string(args, NULL);
	return 0;
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

static int
dummy_init_strings(arguments_t args)
{
	size_t len = LEN_ARRAY(args);
	printk(KERN_INFO "[pfq-lang] %s (len: %zu)!\n", __PRETTY_FUNCTION__, len);
	dummy_strings(args, NULL);
	return 0;
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
dummy_init_cidr(arguments_t args)
{
	size_t len = LEN_ARRAY(args);
	printk(KERN_INFO "[pfq-lang] %s (len: %zu)!\n", __PRETTY_FUNCTION__, len);
	dummy_cidr(args, NULL);
	return 0;
}

static int
dummy_init_cidrs(arguments_t args)
{
	size_t len = LEN_ARRAY(args);
	printk(KERN_INFO "[pfq-lang] %s (len: %zu)!\n", __PRETTY_FUNCTION__, len);
	dummy_cidrs(args, NULL);
	return 0;
}


struct pfq_lang_function_descr dummy_functions[] = {

        { "dummy_int",     "CInt   -> Qbuff -> Action Qbuff",	dummy_int,      dummy_init_int,         NULL },
        { "dummy_ints",    "[CInt] -> Qbuff -> Action Qbuff",	dummy_ints,     dummy_init_ints,        NULL },
        { "dummy_ip",      "Word32 -> Qbuff -> Action Qbuff",	dummy_ip,	dummy_init_ip,          NULL },
        { "dummy_ips",     "[Word32] -> Qbuff -> Action Qbuff",	dummy_ips,	dummy_init_ips,         NULL },
        { "dummy_cidr",	   "CIDR   -> Qbuff -> Action Qbuff",   dummy_cidr,	dummy_init_cidr,        NULL },
        { "dummy_cidrs",   "[CIDR] -> Qbuff -> Action Qbuff",   dummy_cidrs,	dummy_init_cidrs,       NULL },
        { "dummy_string",  "String -> Qbuff -> Action Qbuff",	dummy_string,   dummy_init_string,      NULL },
        { "dummy_strings", "[String] -> Qbuff -> Action Qbuff", dummy_strings,  dummy_init_strings,     NULL },

        { NULL }};

