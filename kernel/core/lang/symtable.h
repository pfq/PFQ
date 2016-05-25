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

#ifndef PFQ_LANG_SYMTABLE_H
#define PFQ_LANG_SYMTABLE_H

#include <core/define.h>

/* symtable_entry */

struct symtable_entry
{
	char			symbol[Q_CORE_FUN_SYMB_LEN];
	char			signature[Q_CORE_FUN_SIGN_LEN];
	void *                  function;
	void *			init;
	void *			fini;
};



struct symtable
{
	size_t			size;
	struct symtable_entry	entry[Q_CORE_FUN_MAX_ENTRIES];
};


/* forward declaration */

struct pfq_lang_functional;
struct pfq_lang_function_descr;

typedef struct pfq_lang_functional * arguments_t;
typedef int (*init_ptr_t)	(arguments_t);
typedef int (*fini_ptr_t)	(arguments_t);

/* symtable */

extern void pfq_lang_symtable_init(void);
extern void pfq_lang_symtable_free(void);

extern int  pfq_lang_symtable_register_function(const char *module, struct symtable *table, const char *symbol, void * fun, init_ptr_t init, fini_ptr_t fini, const char *signature);
extern int  pfq_lang_symtable_register_functions(const char *module, struct symtable *table, struct pfq_lang_function_descr *fun);
extern int  pfq_lang_symtable_unregister_function(const char *module, struct symtable *table, const char *symbol);
extern void pfq_lang_symtable_unregister_functions(const char *module, struct symtable *table, struct pfq_lang_function_descr *fun);
extern struct symtable_entry *pfq_lang_symtable_search(struct symtable *table, const char *symbol);


#endif /* PFQ_LANG_SYMTABLE_H */
