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

#ifndef PF_Q_SYMTABLE_H
#define PF_Q_SYMTBALE_H

#include <linux/skbuff.h>
#include <linux/list.h>
#include <linux/semaphore.h>
#include <linux/rwsem.h>
#include <linux/pf_q.h>

#include <pf_q-module.h>


/* symtable_entry */

struct symtable_entry
{
	struct list_head 	list;
	char 			symbol[Q_FUN_SYMB_LEN];
	void *                  function;
	void *			init;
	void *			fini;
	const char * 		signature;
};

/* semaphore */

extern struct semaphore         symtable_sem;
extern struct rw_semaphore      symtable_rw_sem;

/* categories */

extern struct list_head pfq_lang_functions;


/* symtable */

extern void pfq_symtable_init(void);
extern void pfq_symtable_free(void);

extern int  pfq_symtable_register_function(const char *module, struct list_head *category, const char *symbol, void * fun, init_ptr_t init, fini_ptr_t fini, const char *signature);
extern int  pfq_symtable_unregister_function(const char *module, struct list_head *category, const char *symbol);

extern int pfq_symtable_register_functions  (const char *module, struct list_head *category, struct pfq_function_descr *fun);
extern int pfq_symtable_unregister_functions(const char *module, struct list_head *category, struct pfq_function_descr *fun);

extern struct symtable_entry *pfq_symtable_search(struct list_head *category, const char *symbol);


#endif /* PF_Q_SYMTABLE_H */
