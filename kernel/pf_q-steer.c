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


DEFINE_SEMAPHORE(steering_sem);


struct steering_factory_elem 
{
	struct list_head 	steering_list;
	char 			name[Q_STEERING_NAME_LEN];
	steering_function_t 	function;
};


extern struct factory_hook pfq_steering_hooks[];


LIST_HEAD(steering_factory);

/*
 * register the default steer funcitons here!
 */

void 
pfq_steering_factory_init(void) 
{
	int i = 0;
	for(; pfq_steering_hooks[i].name != NULL ; i++)
	{          
        	pfq_register_steering_function(pfq_steering_hooks[i].name, pfq_steering_hooks[i].function);
	}

	printk(KERN_INFO "[PFQ] steer factory initialized\n");
}


void 
pfq_steering_factory_free(void)
{
	struct list_head *pos = NULL, *q;
	struct steering_factory_elem *this;

	down(&steering_sem);
	list_for_each_safe(pos, q, &steering_factory)
	{
    		this = list_entry(pos, struct steering_factory_elem, steering_list);
		list_del(pos);
		kfree(this);
	}
	up(&steering_sem);
	printk(KERN_INFO "[PFQ] steer factory freed\n");
}


steering_function_t
__pfq_get_steering_function(const char *name)
{
	struct list_head *pos = NULL;
	struct steering_factory_elem *this;

	list_for_each(pos, &steering_factory)
	{
    		this = list_entry(pos, struct steering_factory_elem, steering_list);
        	if (!strcmp(this->name, name)) 
			return this->function;
	}
	return NULL;
}


steering_function_t
pfq_get_steering_function(const char *name)
{
	steering_function_t ret;
	down(&steering_sem);
	ret = __pfq_get_steering_function(name);
	up(&steering_sem);
	return ret;
}


int 
pfq_register_steering_function(const char *name, steering_function_t fun)
{
	struct steering_factory_elem * elem;

	down(&steering_sem);
	if (__pfq_get_steering_function(name) != NULL) {
		up(&steering_sem);
		printk(KERN_INFO "[PFQ] steer factory error: name %s already in use\n", name);
		return -1;
	}

	elem = kmalloc(sizeof(struct steering_factory_elem), GFP_KERNEL);
	if (elem == NULL) {
		up(&steering_sem);
		printk(KERN_INFO "[PFQ] steer factory error: out of memory\n");
		return -1;
	}

	INIT_LIST_HEAD(&elem->steering_list);

	elem->function = fun;
	
	strncpy(elem->name, name, Q_STEERING_NAME_LEN-1);
        elem->name[Q_STEERING_NAME_LEN-1] = '\0';

	list_add(&elem->steering_list, &steering_factory);
	up(&steering_sem);
	
	printk(KERN_INFO "[PFQ] %s function registered\n", name);
	return 0;
}


int 
pfq_unregister_steering_function(const char *name)
{
	struct list_head *pos = NULL, *q;
	struct steering_factory_elem *this;
	
	down(&steering_sem);
	list_for_each_safe(pos, q, &steering_factory)
	{
    		this = list_entry(pos, struct steering_factory_elem, steering_list);
		if (!strcmp(this->name, name))
		{
			list_del(pos);
			up(&steering_sem);
	       		kfree(this);
			return 0;
		}
	}
	up(&steering_sem);
	printk(KERN_INFO "[PFQ] steer factory error: %s no such function\n", name);
	return -1;
}

