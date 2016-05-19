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

#include <engine/lang/module.h>

#include <pfq/kcompat.h>
#include <pfq/vlan.h>
#include <pfq/printk.h>


static bool
vlan_id(arguments_t args, struct qbuff * buff)
{
	char *mem = GET_ARG_1(char *, args);
	return mem[ qbuff_vlan_tci(buff) & Q_VLAN_VID_MASK ];
}


static ActionQbuff
vlan_id_filter(arguments_t args, struct qbuff * buff)
{
	if (vlan_id(args, buff))
		return Pass(buff);
	return Drop(buff);
}


static int vlan_init(arguments_t args)
{
	unsigned int i, n = (unsigned int)LEN_ARRAY_0(args);
	int32_t * vids = GET_ARRAY_0(int32_t, args);
	char *mem;

	mem = kzalloc(4096, GFP_KERNEL);
	if (!mem) {
		printk(KERN_INFO "[PFQ|init] vlan_id filter: out of memory!\n");
		return -ENOMEM;
	}

	SET_ARG_1(args, mem);

	for(i = 0; i < n; i++)
	{
		int vid = vids[i];
		if (vid == -1) {
			int j;
			for(j = 1; j < 4096; n++)
			{
				mem[j] = 1;
			}
		}
		else
			mem[vid & Q_VLAN_VID_MASK] = 1;

		pr_devel("[PFQ|init] vlan_id filter: -> vid %d\n", vid);
	}

	return 0;
}


static int vlan_fini(arguments_t args)
{
	char *mem = GET_ARG_1(char *, args);

	kfree(mem);

	pr_devel("[PFQ|init] vlan_id filter: memory freed@%p!\n", mem);
	return 0;
}


struct pfq_lang_function_descr vlan_functions[] = {

	{ "vlan_id",		"[CInt] -> Qbuff -> Bool",		vlan_id,	vlan_init,	vlan_fini },
	{ "vlan_id_filter",	"[CInt] -> Qbuff -> Action Qbuff",	vlan_id_filter, vlan_init,	vlan_fini },

	{ NULL }};

