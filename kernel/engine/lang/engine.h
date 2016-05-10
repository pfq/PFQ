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


#ifndef PFQ_LANG_ENGINE_H
#define PFQ_LANG_ENGINE_H

#include <pragma/diagnostic_push>
#include <linux/kernel.h>
#include <pragma/diagnostic_pop>

#include <engine/lang/monad.h>
#include <engine/lang/module.h>


static inline bool is_arg_null(struct pfq_lang_functional_arg_descr const *arg)
{
	return !arg->addr && !arg->size && !arg->nelem;
}

static inline bool is_arg_data(struct pfq_lang_functional_arg_descr const *arg)
{
	return arg->addr && arg->size != 0 && arg->nelem == -1;
}

static inline bool is_arg_vector(struct pfq_lang_functional_arg_descr const *arg)
{
	return arg->size != 0 && arg->nelem != -1;
}

static inline bool is_arg_string(struct pfq_lang_functional_arg_descr const *arg)
{
	return arg->addr && arg->size == 0 && arg->nelem == -1;
}

static inline bool is_arg_vector_str(struct pfq_lang_functional_arg_descr const *arg)
{
	return arg->addr && arg->size == 0 && arg->nelem != -1;
}

static inline bool is_arg_function(struct pfq_lang_functional_arg_descr const *arg)
{
	return !arg->addr && arg->size != 0 && arg->nelem == -1;
}


extern int pfq_lang_check_computation_descr(struct pfq_lang_computation_descr const *
				       descr);

extern int pfq_lang_computation_rtlink(struct pfq_lang_computation_descr const *descr,
				  struct pfq_lang_computation_tree *comp,
				  void *context);

extern int pfq_lang_computation_init(struct pfq_lang_computation_tree *comp);
extern int pfq_lang_computation_destruct(struct pfq_lang_computation_tree *comp);

extern struct pfq_lang_computation_tree * pfq_lang_computation_alloc(struct pfq_lang_computation_descr const *);
extern void * pfq_lang_context_alloc(struct pfq_lang_computation_descr const *);
extern const char *pfq_lang_signature_by_user_symbol(const char __user *symb);
extern size_t pfq_lang_number_of_arguments(struct pfq_lang_functional_descr const *fun);

extern char * strdup_user(const char __user *str);

extern ActionSkBuff pfq_lang_run(SkBuff, struct pfq_lang_computation_tree *prg);


#endif /* PFQ_LANG_ENGINE_H */
