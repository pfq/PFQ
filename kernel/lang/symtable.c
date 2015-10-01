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

#include <pragma/diagnostic_push>

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/list.h>
#include <linux/string.h>
#include <linux/semaphore.h>
#include <linux/rwsem.h>

#include <pragma/diagnostic_pop>


#include <lang/module.h>
#include <lang/string-view.h>
#include <lang/signature.h>
#include <lang/symtable.h>


DECLARE_RWSEM(symtable_sem);

LIST_HEAD(pfq_lang_functions);


EXPORT_SYMBOL_GPL(pfq_lang_functions);


static void
__pfq_lang_symtable_free(struct list_head *category)
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
__pfq_lang_symtable_search(struct list_head *category, const char *symbol)
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
__pfq_lang_symtable_register_function(struct list_head *category, const char *symbol, void *fun,
				 init_ptr_t init, fini_ptr_t fini, const char *signature)
{
	struct symtable_entry * elem;

	if (__pfq_lang_symtable_search(category, symbol) != NULL) {
		printk(KERN_INFO "[PFQ] symtable error: symbol '%s' already in use!\n", symbol);
		return -EPERM;
	}

	if (!pfq_lang_signature_check(make_string_view(signature))) {
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
__pfq_lang_symtable_unregister_function(struct list_head *category, const char *symbol)
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
pfq_lang_symtable_unregister_functions(const char *module, struct list_head *category, struct pfq_lang_function_descr *fun)
{
	int i = 0;
	for(; fun[i].symbol != NULL; i++)
	{
		pfq_lang_symtable_unregister_function(module, category, fun[i].symbol);
	}
	return 0;
}


int
pfq_lang_symtable_register_functions(const char *module, struct list_head *category, struct pfq_lang_function_descr *fun)
{
	int i = 0;
	for(; fun[i].symbol != NULL; i++)
	{
		if (pfq_lang_symtable_register_function(module, category, fun[i].symbol, fun[i].ptr,
						   fun[i].init, fun[i].fini, fun[i].signature) < 0) {
                        /* unregister all functions */
                        int j = 0;

                        for(; j < i; j++)
                                pfq_lang_symtable_unregister_function(module, category, fun[j].symbol);

                        return -EFAULT;
                }
	}

	return 0;
}


static size_t
pfq_lang_symtable_pr_devel(const char *hdr, struct list_head *category)
{
	struct list_head *pos = NULL;
        size_t ret = 0;

        down_read(&symtable_sem);

#ifdef PFQ_DEBUG
	pr_devel("[PFQ] %s:\n", hdr);
#endif

	list_for_each(pos, category)
	{
#ifdef PFQ_DEBUG
		struct symtable_entry *this = list_entry(pos, struct symtable_entry, list);
		pr_devel("      %s %pF\n", this->symbol, this->function);
#endif
		ret++;
	}

        up_read(&symtable_sem);
        return ret;
}


void
pfq_lang_symtable_init(void)
{
	size_t numfun;

        pfq_lang_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_lang_function_descr *)filter_functions);
        pfq_lang_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_lang_function_descr *)forward_functions);
        pfq_lang_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_lang_function_descr *)steering_functions);
        pfq_lang_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_lang_function_descr *)high_order_functions);
        pfq_lang_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_lang_function_descr *)bloom_functions);
        pfq_lang_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_lang_function_descr *)vlan_functions);
        pfq_lang_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_lang_function_descr *)misc_functions);
        pfq_lang_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_lang_function_descr *)dummy_functions);
        pfq_lang_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_lang_function_descr *)predicate_functions);
        pfq_lang_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_lang_function_descr *)combinator_functions);
        pfq_lang_symtable_register_functions(NULL, &pfq_lang_functions, (struct pfq_lang_function_descr *)property_functions);

	numfun = pfq_lang_symtable_pr_devel("pfq-lang functions",   &pfq_lang_functions);

	printk(KERN_INFO "[PFQ] symtable initialized (%zu pfq-lang functions loaded).\n",
	       numfun);
}


void
pfq_lang_symtable_free(void)
{
        down_write(&symtable_sem);

        __pfq_lang_symtable_free(&pfq_lang_functions);

        up_write(&symtable_sem);

	printk(KERN_INFO "[PFQ] symtable freed.\n");
}


struct symtable_entry *
pfq_lang_symtable_search(struct list_head *category, const char *symbol)
{
	void *ptr;

        down_read(&symtable_sem);

	ptr = __pfq_lang_symtable_search(category, symbol);

	up_read(&symtable_sem);

        return ptr;
}


int
pfq_lang_symtable_register_function(const char *module, struct list_head *category, const char *symbol, void *fun,
			       init_ptr_t init, fini_ptr_t fini, const char *signature)
{
	int rc;

        down_write(&symtable_sem);

	rc = __pfq_lang_symtable_register_function(category, symbol, fun, init, fini, signature);

	up_write(&symtable_sem);

	if (rc == 0 && module)
		printk(KERN_INFO "[PFQ]%s '%s' @%pF function registered.\n", module, symbol, fun);

	return rc;
}


int
pfq_lang_symtable_unregister_function(const char *module, struct list_head *category, const char *symbol)
{
	struct symtable_entry * elem;

        down_write(&symtable_sem);

	elem = __pfq_lang_symtable_search(category, symbol);
	if (elem == NULL)
		return -EFAULT;

        __pfq_lang_symtable_unregister_function(category, symbol);

        up_write(&symtable_sem);

	printk(KERN_INFO "[PFQ]%s '%s' function unregistered.\n", module, symbol);

	return 0;
}


