/***************************************************************
 *                                                
 * (C) 2011-12 Nicola Bonelli <nicola.bonelli@cnit.it>   
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

#include <linux/list.h>
#include <linux/string.h>
#include <linux/module.h>
#include <linux/semaphore.h>

#include <pf_q-steer.h>
#include <pf_q-steer-fun.h>


MODULE_LICENSE("GPL");
 
/* devmap */

DEFINE_SEMAPHORE(steer_sem);


struct steer_factory_elem {

	struct list_head steer_list;
	
	char 			name[STEER_NAME_LEN];
	steer_function_t 	function;
};



LIST_HEAD(steer_factory);

/*
 * register the default steer funcitons here!
 */

void 
pfq_steer_factory_init() 
{
        pfq_register_steer_function("steer-mac-addr",   steer_mac_addr);
        pfq_register_steer_function("steer-vlan-untag", steer_vlan_untag);
        pfq_register_steer_function("steer-vlan-id",    steer_vlan_id);
        pfq_register_steer_function("steer-ipv4-addr",  steer_ipv4_addr);
        pfq_register_steer_function("steer-ipv6-addr",  steer_ipv6_addr);
	
	printk(KERN_INFO "[PFQ] steer factory initialized.\n");
}


void 
pfq_steer_factory_free()
{
	struct list_head *pos = NULL, *q;
	struct steer_factory_elem *this;

	down(&steer_sem);
	list_for_each_safe(pos, q, &steer_factory)
	{
    		this = list_entry(pos, struct steer_factory_elem, steer_list);
		list_del(pos);
		kfree(this);
	}
	up(&steer_sem);
	printk(KERN_INFO "[PFQ] steer factory freed.\n");
}


steer_function_t
__pfq_get_steer_function(const char *name)
{
	struct list_head *pos = NULL;
	struct steer_factory_elem *this;

	list_for_each(pos, &steer_factory)
	{
    		this = list_entry(pos, struct steer_factory_elem, steer_list);
        	if (!strcmp(this->name, name)) 
			return this->function;
	}
	return NULL;
}


steer_function_t
pfq_get_steer_function(const char *name)
{
	steer_function_t ret;
	down(&steer_sem);
	ret = __pfq_get_steer_function(name);
	up(&steer_sem);
	return ret;
}


int 
pfq_register_steer_function(const char *name, steer_function_t fun)
{
	struct steer_factory_elem * elem;

	down(&steer_sem);
	if (__pfq_get_steer_function(name) != NULL) {
		up(&steer_sem);
		printk(KERN_INFO "[PFQ] steer factory error: name %s already in use.\n", name);
		return -1;
	}

	elem = kmalloc(sizeof(struct steer_factory_elem), GFP_KERNEL);
	if (elem == NULL) {
		up(&steer_sem);
		printk(KERN_INFO "[PFQ] steer factory error: out of memory.\n");
		return -1;
	}

	INIT_LIST_HEAD(&elem->steer_list);

	elem->function = fun;
	
	strncpy(elem->name, name, STEER_NAME_LEN-1);
        elem->name[STEER_NAME_LEN-1] = '\0';

	list_add(&elem->steer_list, &steer_factory);
	up(&steer_sem);
	
	printk(KERN_INFO "[PFQ] %s function registered.\n", name);
	return 0;
}


int 
pfq_unregister_steer_function(const char *name)
{
	struct list_head *pos = NULL, *q;
	struct steer_factory_elem *this;
	
	down(&steer_sem);
	list_for_each_safe(pos, q, &steer_factory)
	{
    		this = list_entry(pos, struct steer_factory_elem, steer_list);
		if (!strcmp(this->name, name))
		{
			list_del(pos);
			up(&steer_sem);
	       		kfree(this);
			return 0;
		}
	}
	up(&steer_sem);
	printk(KERN_INFO "[PFQ] steer factory error: %s no such function.\n", name);
	return -1;
}

