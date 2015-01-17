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

#include <linux/list.h>
#include <linux/string.h>
#include <linux/semaphore.h>
#include <linux/rwsem.h>

#include <pf_q-module.h>
#include <pf_q-group.h>
#include <pf_q-symtable.h>

#include <pf_q-string-view.h>
#include <pf_q-signature.h>


DECLARE_RWSEM(symtable_rw_sem);
DEFINE_SEMAPHORE(symtable_sem);

LIST_HEAD(pfq_lang_functions);

EXPORT_SYMBOL_GPL(pfq_lang_functions);


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
__pfq_symtable_register_function(struct list_head *category, const char *symbol, void *fun, init_ptr_t init, fini_ptr_t fini, const char *signature)
{
	struct symtable_entry * elem;

	if (__pfq_symtable_search(category, symbol) != NULL) {
		printk(KERN_INFO "[PFQ] symtable error: symbol '%s' already in use!\n", symbol);
		return -EPERM;
	}

	if (!pfq_signature_check(make_string_view(signature))) {
		printk(KERN_INFO "[PFQ] symtable error: symbol '%s' bad signature '%s'!\n", symbol, signature);
		return -EFAULT;
	}

	elem = kmalloc(sizeof(struct symtable_entry), GFP_KERNEL);
	if (elem == NULL) {
		printk(KERN_WARNING "[PFQ] symtable error: out of memory!\n");
		return -ENOMEM;
	}

	INIT_LIST_HEAD(&elem->list);

	elem->function = fun;
        elem->init = init;
        elem->fini = fini;

	strncpy(elem->symbol, symbol, Q_FUN_SYMB_LEN-1);
        elem->symbol[Q_FUN_SYMB_LEN-1] = '\0';
	list_add(&elem->list, category);

	elem->signature = signature;

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
	return -EFAULT;
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
		if (pfq_symtable_register_function(module, category, fun[i].symbol, fun[i].ptr, fun[i].init, fun[i].fini, fun[i].signature) < 0) {
                        /* unregister all functions */
                        int j = 0;

                        for(; j < i; j++)
                                pfq_symtable_unregister_function(module, category, fun[j].symbol);

                        return -EFAULT;
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
    		pr_devel("      %s %pF\n", this->symbol, this->function);
	}

        up(&symtable_sem);
}


void
pfq_symtable_init(void)
{
        pfq_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_function_descr *)filter_functions);
        pfq_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_function_descr *)forward_functions);
        pfq_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_function_descr *)steering_functions);
        pfq_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_function_descr *)high_order_functions);
        pfq_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_function_descr *)bloom_functions);
        pfq_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_function_descr *)vlan_functions);
        pfq_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_function_descr *)misc_functions);
        pfq_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_function_descr *)dummy_functions);

        pfq_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_function_descr *)predicate_functions);
        pfq_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_function_descr *)combinator_functions);
        pfq_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_function_descr *)property_functions);

	pfq_symtable_pr_devel("pfq-lang functions: ",   &pfq_lang_functions);

	printk(KERN_INFO "[PFQ] symtable initialized.\n");
}


void
pfq_symtable_free(void)
{
        down_write(&symtable_rw_sem);
	down(&symtable_sem);

        __pfq_symtable_free(&pfq_lang_functions);

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
pfq_symtable_register_function(const char *module, struct list_head *category, const char *symbol, void *fun, init_ptr_t init, fini_ptr_t fini, const char *signature)
{
	int rc;

        down(&symtable_sem);

	rc = __pfq_symtable_register_function(category, symbol, fun, init, fini, signature);

	up(&symtable_sem);

	if (rc == 0 && module)
		printk(KERN_INFO "[PFQ]%s '%s' @%pF function registered.\n", module, symbol, fun);

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
        	return -EFAULT;

	__pfq_dismiss_function(elem->function);
        __pfq_symtable_unregister_function(category, symbol);

	up(&symtable_sem);
        up_write(&symtable_rw_sem);

	printk(KERN_INFO "[PFQ]%s '%s' function unregistered.\n", module, symbol);

	return 0;
}


