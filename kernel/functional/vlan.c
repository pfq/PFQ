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

#include <pf_q-module.h>


static bool
vlan_id(arguments_t args, SkBuff b)
{
	char *mem = get_arg_1(char *, args);
	return mem[ b.skb->vlan_tci & VLAN_VID_MASK ];
}


static Action_SkBuff
vlan_id_filter(arguments_t args, SkBuff b)
{
	if (vlan_id(args, b))
		return Pass(b);
	return Drop(b);
}


static int vlan_init(arguments_t args)
{
	unsigned int n = get_array_len_0(args);
	int32_t *vid  = get_array_0(int32_t, args);
        char *mem; int i;

        mem = kzalloc(4096, GFP_KERNEL);
        if (!mem) {
	       	printk(KERN_INFO "[PFQ|init] vlan_id filter: out of memory!\n");
	       	return -ENOMEM;
	}

	set_arg_1(args, mem);

	for(i = 0; i < n; i++)
	{
		if (vid[i] == -1) {
                	int n;
                	for(n = 1; n < 4096; n++)
			{
                        	mem[n] = 1;
			}
		}
		else {
			mem[vid[i] & VLAN_VID_MASK] = 1;
		}

		pr_devel("[PFQ|init] vlan_id filter: -> vid %d\n", vid[i]);
	}

	return 0;
}


static int vlan_fini(arguments_t args)
{
	char *mem = get_arg_1(char *, args);

	kfree(mem);

	pr_devel("[PFQ|init] vlan_id filter: memory freed@%p!\n", mem);
	return 0;
}


struct pfq_function_descr vlan_functions[] = {

        { "vlan_id",   		"[CInt] -> SkBuff -> Bool",		vlan_id, 	vlan_init, 	vlan_fini },
        { "vlan_id_filter",  	"[CInt] -> SkBuff -> Action SkBuff", 	vlan_id_filter, vlan_init, 	vlan_fini },

        { NULL }};

