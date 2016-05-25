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

#include <core/lang/module.h>
#include <core/lang/string-view.h>
#include <core/lang/signature.h>
#include <core/lang/symtable.h>
#include <core/global.h>

#include <pfq/kcompat.h>
#include <pfq/printk.h>


static void
__pfq_lang_symtable_free(struct symtable *table)
{
	size_t n = 0;
	for(; n < table->size ; ++n)
	{
		table->entry[n].function = NULL;
		table->entry[n].init = NULL;
		table->entry[n].fini = NULL;
	}
	table->size = 0;
}


static struct symtable_entry *
__pfq_lang_symtable_search(struct symtable *table, const char *symbol)
{
        size_t n = 0;
        if (symbol != NULL)
	{
		for(; n < table->size; ++n)
		{
			if (table->entry[n].function == NULL)
				continue;
			if (!strcmp(table->entry[n].symbol, symbol))
				return &table->entry[n];
		}
	}
	return NULL;
}


static struct symtable_entry *
__pfq_lang_get_free_entry(struct symtable *table)
{
	size_t n = 0;
	for(; n < table->size; ++n)
	{
		if (table->entry[n].function == NULL)
			return &table->entry[n];
	}

	if (table->size < Q_CORE_FUN_MAX_ENTRIES)
		return &table->entry[table->size++];

	return NULL;
}


static int
__pfq_lang_symtable_register_function(struct symtable *table, const char *symbol, void *fun,
				 init_ptr_t init, fini_ptr_t fini, const char *signature)
{
	struct symtable_entry * elem;

	if (__pfq_lang_symtable_search(table, symbol) != NULL) {
		printk(KERN_INFO "[PFQ] symtable error: symbol '%s' already in use!\n", symbol);
		return -EPERM;
	}

	if (!pfq_lang_signature_check(make_string_view(signature))) {
		printk(KERN_INFO "[PFQ] symtable error: symbol '%s' bad signature '%s'!\n", symbol, signature);
		return -EFAULT;
	}

	elem = __pfq_lang_get_free_entry(table);
	if (!elem) {
		return -ENOMEM;
	}

	strncpy(elem->symbol, symbol, Q_CORE_FUN_SYMB_LEN-1);
        elem->symbol[Q_CORE_FUN_SYMB_LEN-1] = '\0';

	strncpy(elem->signature, signature, Q_CORE_FUN_SIGN_LEN-1);
        elem->signature[Q_CORE_FUN_SIGN_LEN-1] = '\0';

	elem->function = fun;
        elem->init     = init;
        elem->fini     = fini;
	return 0;
}


static int
__pfq_lang_symtable_unregister_function(struct symtable *table, const char *symbol)
{
	size_t n = 0;

	for(; n < table->size; ++n)
	{
		if (!strcmp(table->entry[n].symbol, symbol)) {
			table->entry[n].function = NULL;
			table->entry[n].init = NULL;
			table->entry[n].fini = NULL;
			return 0;
		}
	}

	return -EINVAL;
}


void
pfq_lang_symtable_unregister_functions(const char *module, struct symtable *table, struct pfq_lang_function_descr *fun)
{
	int i = 0;
	for(; fun[i].symbol != NULL; i++)
	{
		pfq_lang_symtable_unregister_function(module, table, fun[i].symbol);
	}
}


int
pfq_lang_symtable_register_functions(const char *module, struct symtable *table, struct pfq_lang_function_descr *fun)
{
	int i = 0;
	for(; fun[i].symbol != NULL; i++)
	{
		if (pfq_lang_symtable_register_function(module, table, fun[i].symbol, fun[i].ptr,
							fun[i].init, fun[i].fini, fun[i].signature) < 0)
		{
                        int j = 0;
                        for(; j < i; j++)
			{
                                pfq_lang_symtable_unregister_function(module, table, fun[j].symbol);
			}
                        return -EFAULT;
                }
	}

	return 0;
}


static size_t
pfq_lang_symtable_pr_devel(const char *hdr, struct symtable *table)
{
        size_t n, ret = 0;

        down_read(&global->symtable_sem);

#ifdef PFQ_DEBUG
	pr_devel("[PFQ] %s:\n", hdr);
#endif

	for(n = 0; n < table->size; ++n)
	{
#ifdef PFQ_DEBUG
		if (table->entry[n].function)
		{
			pr_devel("      %s %pF\n", table->entry[n].symbol, table->entry[n].function);
		}
#endif
		ret++;
	}

        up_read(&global->symtable_sem);
        return ret;
}


void
pfq_lang_symtable_init(void)
{
	size_t numfun;

        pfq_lang_symtable_register_functions(NULL, &global->functions, (struct pfq_lang_function_descr *)filter_functions);
        pfq_lang_symtable_register_functions(NULL, &global->functions, (struct pfq_lang_function_descr *)forward_functions);
        pfq_lang_symtable_register_functions(NULL, &global->functions, (struct pfq_lang_function_descr *)steering_functions);
        pfq_lang_symtable_register_functions(NULL, &global->functions, (struct pfq_lang_function_descr *)bloom_functions);
        pfq_lang_symtable_register_functions(NULL, &global->functions, (struct pfq_lang_function_descr *)control_functions);
        pfq_lang_symtable_register_functions(NULL, &global->functions, (struct pfq_lang_function_descr *)vlan_functions);
        pfq_lang_symtable_register_functions(NULL, &global->functions, (struct pfq_lang_function_descr *)misc_functions);
        pfq_lang_symtable_register_functions(NULL, &global->functions, (struct pfq_lang_function_descr *)dummy_functions);
        pfq_lang_symtable_register_functions(NULL, &global->functions, (struct pfq_lang_function_descr *)predicate_functions);
        pfq_lang_symtable_register_functions(NULL, &global->functions, (struct pfq_lang_function_descr *)combinator_functions);
        pfq_lang_symtable_register_functions(NULL, &global->functions, (struct pfq_lang_function_descr *)property_functions);

	numfun = pfq_lang_symtable_pr_devel("pfq-lang functions",   &global->functions);

	printk(KERN_INFO "[PFQ] symtable initialized (%zu pfq-lang functions loaded).\n",
	       numfun);
}


void
pfq_lang_symtable_free(void)
{
        down_write(&global->symtable_sem);
        __pfq_lang_symtable_free(&global->functions);
        up_write(&global->symtable_sem);

	printk(KERN_INFO "[PFQ] symtable freed.\n");
}


struct symtable_entry *
pfq_lang_symtable_search(struct symtable *table, const char *symbol)
{
	void *ptr;

        down_read(&global->symtable_sem);
	ptr = __pfq_lang_symtable_search(table, symbol);
	up_read(&global->symtable_sem);

        return ptr;
}


int
pfq_lang_symtable_register_function(const char *module, struct symtable *table, const char *symbol, void *fun,
				    init_ptr_t init, fini_ptr_t fini, const char *signature)
{
	int rc;

        down_write(&global->symtable_sem);
	rc = __pfq_lang_symtable_register_function(table, symbol, fun, init, fini, signature);
	up_write(&global->symtable_sem);

	if (rc == 0 && module)
		printk(KERN_INFO "[PFQ]%s '%s' @%pF function registered.\n", module, symbol, fun);

	return rc;
}


int
pfq_lang_symtable_unregister_function(const char *module, struct symtable *table, const char *symbol)
{
	int rc;

        down_write(&global->symtable_sem);
        rc = __pfq_lang_symtable_unregister_function(table, symbol);
        up_write(&global->symtable_sem);

	printk(KERN_INFO "[PFQ]%s '%s' function %s\n", module, symbol, rc == 0 ? "unregistered." : "not registered.");
	return rc;
}


