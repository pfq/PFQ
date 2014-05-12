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

#include <linux/list.h>
#include <linux/string.h>
#include <linux/semaphore.h>
#include <linux/rwsem.h>

#include <pf_q-module.h>
#include <pf_q-group.h>
#include <pf_q-symtable.h>


DECLARE_RWSEM(symtable_rw_sem);
DEFINE_SEMAPHORE(symtable_sem);

LIST_HEAD(pfq_monadic_cat);
LIST_HEAD(pfq_predicate_cat);
LIST_HEAD(pfq_property_cat);

EXPORT_SYMBOL_GPL(pfq_monadic_cat);
EXPORT_SYMBOL_GPL(pfq_predicate_cat);
EXPORT_SYMBOL_GPL(pfq_property_cat);



static void
__pfq_symtable_free(struct list_head *category)
{
	struct list_head *pos = NULL, *q;
	struct symtable_entry *this;

	list_for_each_safe(pos, q, category)
	{
    		this = list_entry(pos, struct symtable_entry, list);
		list_del(pos);
                kfree(this);
	}
}


static struct symtable_entry *
__pfq_symtable_search(struct list_head *category, const char *symbol)
{
	struct list_head *pos = NULL;
	struct symtable_entry *this;

        if (symbol == NULL)
                return NULL;

	list_for_each(pos, category)
	{
    		this = list_entry(pos, struct symtable_entry, list);
        	if (!strcmp(this->symbol, symbol))
			return this;
	}
	return NULL;

}

static int
__pfq_symtable_register_function(struct list_head *category, const char *symbol, void *fun, uint64_t properties)
{
	struct symtable_entry * elem;

	if (__pfq_symtable_search(category, symbol) != NULL) {
		printk(KERN_INFO "[PFQ] symtable error: symbol '%s' already in use!\n", symbol);
		return -1;
	}

	elem = kmalloc(sizeof(struct symtable_entry), GFP_KERNEL);
	if (elem == NULL) {
		printk(KERN_WARNING "[PFQ] symtable error: out of memory!\n");
		return -1;
	}

	INIT_LIST_HEAD(&elem->list);

	elem->function = fun;

	strncpy(elem->symbol, symbol, Q_FUN_SYMB_LEN-1);
        elem->symbol[Q_FUN_SYMB_LEN-1] = '\0';
	list_add(&elem->list, category);

	elem->properties = properties;

	return 0;
}


static int
__pfq_symtable_unregister_function(struct list_head *category, const char *symbol)
{
	struct list_head *pos = NULL, *q;
	struct symtable_entry *this;

	list_for_each_safe(pos, q, category)
	{
    		this = list_entry(pos, struct symtable_entry, list);
		if (!strcmp(this->symbol, symbol)) {
			list_del(pos);
	       		kfree(this);
			return 0;
		}
	}
	printk(KERN_INFO "[PFQ] symtable error: '%s' no such function\n", symbol);
	return -1;
}


int
pfq_symtable_unregister_functions(const char *module, struct list_head *category, struct pfq_function_descr *fun)
{
	int i = 0;
	for(; fun[i].symbol != NULL; i++)
	{
		pfq_symtable_unregister_function(module, category, fun[i].symbol);
	}
	return 0;
}


int
pfq_symtable_register_functions(const char *module, struct list_head *category, struct pfq_function_descr *fun)
{
	int i = 0;
	for(; fun[i].symbol != NULL; i++)
	{
		if (pfq_symtable_register_function(module, category, fun[i].symbol, fun[i].ptr, fun[i].properties) < 0)
                {
                        /* unregister all functions */
                        int j = 0;

                        for(; j < i; j++)
                                pfq_symtable_unregister_function(module, category, fun[j].symbol);

                        return -1;
                }
	}

	return 0;
}


static void
pfq_symtable_pr_devel(const char *hdr, struct list_head *category)
{
	struct list_head *pos = NULL;
	struct symtable_entry *this;

        down(&symtable_sem);

	pr_devel("[PFQ] %s:\n", hdr);

	list_for_each(pos, category)
	{
    		this = list_entry(pos, struct symtable_entry, list);
    		pr_devel("      %s %p\n", this->symbol, this->function);
	}

        up(&symtable_sem);
}


void
pfq_symtable_init(void)
{
	extern struct pfq_monadic_fun_descr 	filter_functions[];
	extern struct pfq_monadic_fun_descr 	forward_functions[];
	extern struct pfq_monadic_fun_descr 	steering_functions[];

	extern struct pfq_predicate_fun_descr 	predicate_functions[];
	extern struct pfq_combinator_fun_descr 	combinator_functions[];

	extern struct pfq_property_fun_descr 	property_functions[];

	extern struct pfq_function_descr 	high_order_functions[];
	extern struct pfq_function_descr 	misc_functions[];

        pfq_symtable_register_functions(NULL, &pfq_monadic_cat, (struct pfq_function_descr *)filter_functions);
        pfq_symtable_register_functions(NULL, &pfq_monadic_cat, (struct pfq_function_descr *)forward_functions);
        pfq_symtable_register_functions(NULL, &pfq_monadic_cat, (struct pfq_function_descr *)steering_functions);
        pfq_symtable_register_functions(NULL, &pfq_monadic_cat, (struct pfq_function_descr *)high_order_functions);
        pfq_symtable_register_functions(NULL, &pfq_monadic_cat, (struct pfq_function_descr *)misc_functions);

        pfq_symtable_register_functions(NULL, &pfq_predicate_cat, (struct pfq_function_descr *)predicate_functions);
        pfq_symtable_register_functions(NULL, &pfq_predicate_cat, (struct pfq_function_descr *)combinator_functions);

        pfq_symtable_register_functions(NULL, &pfq_property_cat, (struct pfq_function_descr *)property_functions);

	pfq_symtable_pr_devel("monadic functions: ",   &pfq_monadic_cat);
	pfq_symtable_pr_devel("predicate functions: ", &pfq_predicate_cat);
	pfq_symtable_pr_devel("property functions: ",  &pfq_property_cat);

	printk(KERN_INFO "[PFQ] symtable initialized.\n");
}


void
pfq_symtable_free(void)
{
        down_write(&symtable_rw_sem);
	down(&symtable_sem);

        __pfq_symtable_free(&pfq_monadic_cat);
	__pfq_symtable_free(&pfq_predicate_cat);
	__pfq_symtable_free(&pfq_property_cat);

        up(&symtable_sem);
        up_write(&symtable_rw_sem);

	printk(KERN_INFO "[PFQ] symtable freed.\n");
}


struct symtable_entry *
pfq_symtable_search(struct list_head *category, const char *symbol)
{
	void *ptr;

        down(&symtable_sem);

	ptr = __pfq_symtable_search(category, symbol);

	up(&symtable_sem);

        return ptr;
}


int
pfq_symtable_register_function(const char *module, struct list_head *category, const char *symbol, void *fun, uint64_t properties)
{
	int rc;

        down(&symtable_sem);

	rc = __pfq_symtable_register_function(category, symbol, fun, properties);

	up(&symtable_sem);

	if (rc == 0 && module)
		printk(KERN_INFO "[PFQ]%s '%s' @%p function registered.\n", module, symbol, fun);

	return rc;
}


int
pfq_symtable_unregister_function(const char *module, struct list_head *category, const char *symbol)
{
	struct symtable_entry * elem;

        down_write(&symtable_rw_sem);
	down(&symtable_sem);

	elem = __pfq_symtable_search(category, symbol);
	if (elem == NULL)
        	return -1;

	__pfq_dismiss_function(elem->function);
        __pfq_symtable_unregister_function(category, symbol);

	up(&symtable_sem);
        up_write(&symtable_rw_sem);

	printk(KERN_INFO "[PFQ]%s '%s' function unregistered.\n", module, symbol);

	return 0;
}


