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


#ifndef PF_Q_ENGINE_H
#define PF_Q_ENGINE_H

#include <pragma/diagnostic_push>
#include <linux/kernel.h>
#include <pragma/diagnostic_pop>

#include <pf_q-monad.h>
#include <pf_q-module.h>


static inline bool is_arg_null(struct pfq_functional_arg_descr const *arg)
{
	return !arg->addr && !arg->size && !arg->nelem;
}

static inline bool is_arg_data(struct pfq_functional_arg_descr const *arg)
{
	return arg->addr && arg->size != 0 && arg->nelem == -1;
}

static inline bool is_arg_vector(struct pfq_functional_arg_descr const *arg)
{
	return arg->size != 0 && arg->nelem != -1;
}

static inline bool is_arg_string(struct pfq_functional_arg_descr const *arg)
{
	return arg->addr && arg->size == 0 && arg->nelem == -1;
}

static inline bool is_arg_vector_str(struct pfq_functional_arg_descr const *arg)
{
	return arg->addr && arg->size == 0 && arg->nelem != -1;
}

static inline bool is_arg_function(struct pfq_functional_arg_descr const *arg)
{
	return !arg->addr && arg->size != 0 && arg->nelem == -1;
}


extern int pfq_check_computation_descr(struct pfq_computation_descr const *
				       descr);

extern int pfq_computation_rtlink(struct pfq_computation_descr const *descr,
				  struct pfq_computation_tree *comp,
				  void *context);

extern int pfq_computation_init(struct pfq_computation_tree *comp);
extern int pfq_computation_fini(struct pfq_computation_tree *comp);

extern struct pfq_computation_tree * pfq_computation_alloc(struct pfq_computation_descr const *);
extern void * pfq_context_alloc(struct pfq_computation_descr const *);
extern const char *pfq_signature_by_user_symbol(const char __user *symb);
extern size_t pfq_number_of_arguments(struct pfq_functional_descr const *fun);

extern char * strdup_user(const char __user *str);

extern Action_SkBuff pfq_run(struct pfq_computation_tree *prg, SkBuff);


#endif /* PF_Q_ENGINE_H */
