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

#include <linux/list.h>
#include <linux/string.h>
#include <linux/semaphore.h>

#include <linux/pf_q-module.h>

#include <pf_q-group.h>
#include <pf_q-factory.h>


DEFINE_SEMAPHORE(factory_sem);

LIST_HEAD(monadic_cat);
LIST_HEAD(predicate_cat);


struct factory_entry
{
	struct list_head 	list;
	char 			symbol[Q_FUN_SYMB_LEN];
	void *                  function;
};


int
pfq_register_functions(const char *module, struct list_head *category, struct pfq_function_descr *fun)
{
	int i = 0;
	for(; fun[i].symbol != NULL; i++)
	{
		pfq_register_function(module, category, fun[i].symbol, fun[i].function);
	}
	return 0;
}

int
pfq_unregister_functions(const char *module, struct list_head *category, struct pfq_function_descr *fun)
{
	int i = 0;
	for(; fun[i].symbol != NULL; i++)
	{
		pfq_unregister_function(module, category, fun[i].symbol);
	}
	return 0;
}

/* register all functions available by default */

extern struct pfq_function_descr filter_functions[];
extern struct pfq_function_descr forward_functions[];
extern struct pfq_function_descr steering_functions[];
extern struct pfq_function_descr misc_functions[];

extern struct pfq_function_descr predicate_functions[];

void
pfq_factory_init(void)
{
        pfq_register_functions(NULL, &monadic_cat, filter_functions);
        pfq_register_functions(NULL, &monadic_cat, forward_functions);
        pfq_register_functions(NULL, &monadic_cat, steering_functions);
        pfq_register_functions(NULL, &monadic_cat, misc_functions);

        pfq_register_functions(NULL, &predicate_cat, predicate_functions);

	printk(KERN_INFO "[PFQ] function-factory initialized.\n");
}

void
__pfq_factory_free(struct list_head *category)
{
	struct list_head *pos = NULL, *q;
	struct factory_entry *this;

	list_for_each_safe(pos, q, category)
	{
    		this = list_entry(pos, struct factory_entry, list);
		list_del(pos);
		kfree(this);
	}
}

void
pfq_factory_free(void)
{
	down(&factory_sem);
	__pfq_factory_free(&monadic_cat);
	__pfq_factory_free(&predicate_cat);
	up(&factory_sem);
	printk(KERN_INFO "[PFQ] function factory freed.\n");
}


pfq_function_t
__pfq_get_function(struct list_head *category, const char *symbol)
{
	struct list_head *pos = NULL;
	struct factory_entry *this;

        if (symbol == NULL)
                return NULL;

	list_for_each(pos, category)
	{
    		this = list_entry(pos, struct factory_entry, list);
        	if (!strcmp(this->symbol, symbol))
			return this->function;
	}
	return NULL;
}


pfq_function_t
pfq_get_function(struct list_head *category, const char *symbol)
{
	pfq_function_t ret;
	down(&factory_sem);
	ret = __pfq_get_function(category, symbol);
	up(&factory_sem);
	return ret;
}


int
__pfq_register_function(struct list_head *category, const char *symbol, pfq_function_t fun)
{
	struct factory_entry * elem;

	if (__pfq_get_function(category, symbol) != NULL) {
		pr_devel("[PFQ] function factory error: symbol %s already in use!\n", symbol);
		return -1;
	}

	elem = kmalloc(sizeof(struct factory_entry), GFP_KERNEL);
	if (elem == NULL) {
		printk(KERN_WARNING "[PFQ] function factory error: out of memory!\n");
		return -1;
	}

	INIT_LIST_HEAD(&elem->list);

	elem->function = fun;

	strncpy(elem->symbol, symbol, Q_FUN_SYMB_LEN-1);
        elem->symbol[Q_FUN_SYMB_LEN-1] = '\0';
	list_add(&elem->list, category);

	return 0;
}


int
pfq_register_function(const char *module, struct list_head *category, const char *symbol, pfq_function_t fun)
{
	int r;
	down(&factory_sem);
	r = __pfq_register_function(category, symbol, fun);
	up(&factory_sem);
	if (r == 0 && module)
		printk(KERN_INFO "[PFQ]%s '%s' @%p function registered.\n", module, symbol, fun);

	return r;
}


int
__pfq_unregister_function(struct list_head *category, const char *symbol)
{
	struct list_head *pos = NULL, *q;
	struct factory_entry *this;

	list_for_each_safe(pos, q, category)
	{
    		this = list_entry(pos, struct factory_entry, list);
		if (!strcmp(this->symbol, symbol))
		{
			list_del(pos);
	       		kfree(this);
			return 0;
		}
	}
	pr_devel("[PFQ] function factory error: %s no such function\n", symbol);
	return -1;
}


int
pfq_unregister_function(const char *module, struct list_head *category, const char *symbol)
{
	pfq_function_t fun;

	down(&factory_sem);

	fun = __pfq_get_function(category, symbol);
	if (fun == NULL) {
        	return -1;
	}

	__pfq_dismiss_function(fun);
        __pfq_unregister_function(category, symbol);

	printk(KERN_INFO "[PFQ]%s '%s' function unregistered.\n", module, symbol);
	up(&factory_sem);

	return 0;
}


