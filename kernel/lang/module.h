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

#ifndef PFQ_LANG_MODULE_H
#define PFQ_LANG_MODULE_H

#include <lang/monad.h>
#include <lang/maybe.h>

#include <pfq/sparse.h>
#include <pfq/kcompat.h>

#define ARGS_TYPE(a)		__builtin_choose_expr(__builtin_types_compatible_p(arguments_t, typeof(a)), a, (void)0)

#define EVAL_FUNCTION(f,buff)    eval_function(f, buff)
#define EVAL_PROPERTY(f, buff)	((property_ptr_t )f.fun->run)(f.fun, buff)
#define EVAL_PREDICATE(f,buff)	((predicate_ptr_t)f.fun->run)(f.fun, buff)

#define GET_PTR_0(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), (type *)&ARGS_TYPE(a)->arg[0].value, (void *)ARGS_TYPE(a)->arg[0].value)
#define GET_PTR_1(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), (type *)&ARGS_TYPE(a)->arg[1].value, (void *)ARGS_TYPE(a)->arg[1].value)
#define GET_PTR_2(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), (type *)&ARGS_TYPE(a)->arg[2].value, (void *)ARGS_TYPE(a)->arg[2].value)
#define GET_PTR_3(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), (type *)&ARGS_TYPE(a)->arg[3].value, (void *)ARGS_TYPE(a)->arg[3].value)
#define GET_PTR_4(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), (type *)&ARGS_TYPE(a)->arg[4].value, (void *)ARGS_TYPE(a)->arg[4].value)
#define GET_PTR_5(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), (type *)&ARGS_TYPE(a)->arg[5].value, (void *)ARGS_TYPE(a)->arg[5].value)
#define GET_PTR_6(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), (type *)&ARGS_TYPE(a)->arg[6].value, (void *)ARGS_TYPE(a)->arg[6].value)
#define GET_PTR_7(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), (type *)&ARGS_TYPE(a)->arg[7].value, (void *)ARGS_TYPE(a)->arg[7].value)
#define GET_PTR(type,a)		GET_ARG_0(type,a)

#define GET_ARG_0(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), *(type *)&ARGS_TYPE(a)->arg[0].value, (void *)ARGS_TYPE(a)->arg[0].value)
#define GET_ARG_1(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), *(type *)&ARGS_TYPE(a)->arg[1].value, (void *)ARGS_TYPE(a)->arg[1].value)
#define GET_ARG_2(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), *(type *)&ARGS_TYPE(a)->arg[2].value, (void *)ARGS_TYPE(a)->arg[2].value)
#define GET_ARG_3(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), *(type *)&ARGS_TYPE(a)->arg[3].value, (void *)ARGS_TYPE(a)->arg[3].value)
#define GET_ARG_4(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), *(type *)&ARGS_TYPE(a)->arg[4].value, (void *)ARGS_TYPE(a)->arg[4].value)
#define GET_ARG_5(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), *(type *)&ARGS_TYPE(a)->arg[5].value, (void *)ARGS_TYPE(a)->arg[5].value)
#define GET_ARG_6(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), *(type *)&ARGS_TYPE(a)->arg[6].value, (void *)ARGS_TYPE(a)->arg[6].value)
#define GET_ARG_7(type,a)	__builtin_choose_expr(sizeof(type) <= sizeof(uintptr_t), *(type *)&ARGS_TYPE(a)->arg[7].value, (void *)ARGS_TYPE(a)->arg[7].value)
#define GET_ARG(type,a)		GET_ARG_0(type,a)

#define SET_ARG_0(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(uintptr_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[0].value) = v, (void)0)
#define SET_ARG_1(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(uintptr_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[1].value) = v, (void)0)
#define SET_ARG_2(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(uintptr_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[2].value) = v, (void)0)
#define SET_ARG_3(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(uintptr_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[3].value) = v, (void)0)
#define SET_ARG_4(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(uintptr_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[4].value) = v, (void)0)
#define SET_ARG_5(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(uintptr_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[5].value) = v, (void)0)
#define SET_ARG_6(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(uintptr_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[6].value) = v, (void)0)
#define SET_ARG_7(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(uintptr_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[7].value) = v, (void)0)
#define SET_ARG(type,a)		SET_ARG_0(type,a)

#define GET_ARRAY_0(type,a)	((type *)ARGS_TYPE(a)->arg[0].value)
#define GET_ARRAY_1(type,a)	((type *)ARGS_TYPE(a)->arg[1].value)
#define GET_ARRAY_2(type,a)	((type *)ARGS_TYPE(a)->arg[2].value)
#define GET_ARRAY_3(type,a)	((type *)ARGS_TYPE(a)->arg[3].value)
#define GET_ARRAY_4(type,a)	((type *)ARGS_TYPE(a)->arg[4].value)
#define GET_ARRAY_5(type,a)	((type *)ARGS_TYPE(a)->arg[5].value)
#define GET_ARRAY_6(type,a)	((type *)ARGS_TYPE(a)->arg[6].value)
#define GET_ARRAY_7(type,a)	((type *)ARGS_TYPE(a)->arg[7].value)
#define GET_ARRAY(type, a)	GET_ARRAY_0(type,a)

#define LEN_ARRAY_0(a)		(ARGS_TYPE(a)->arg[0].nelem)
#define LEN_ARRAY_1(a)		(ARGS_TYPE(a)->arg[1].nelem)
#define LEN_ARRAY_2(a)		(ARGS_TYPE(a)->arg[2].nelem)
#define LEN_ARRAY_3(a)		(ARGS_TYPE(a)->arg[3].nelem)
#define LEN_ARRAY_4(a)		(ARGS_TYPE(a)->arg[4].nelem)
#define LEN_ARRAY_5(a)		(ARGS_TYPE(a)->arg[5].nelem)
#define LEN_ARRAY_6(a)		(ARGS_TYPE(a)->arg[6].nelem)
#define LEN_ARRAY_7(a)		(ARGS_TYPE(a)->arg[7].nelem)
#define LEN_ARRAY(a)		LEN_ARRAY_0(a)

#define SAFE_CAST(v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(uintptr_t), v, (void)0)

/**** generic functional type ****/

struct pfq_lang_function_descr;
struct pfq_lang_exec;
struct symtable;

extern int pfq_lang_symtable_register_functions  (const char *module, struct symtable *table, struct pfq_lang_function_descr *fun);
extern void pfq_lang_symtable_unregister_functions(const char *module, struct symtable *table, struct pfq_lang_function_descr *fun);


struct pfq_lang_functional_arg
{
        ptrdiff_t     value;
        size_t	      nelem;	/* > 0 array */
};


struct pfq_lang_functional
{
	void * run;					/* pointer to function */
	struct pfq_lang_functional_arg arg[8];		/* arguments */
	struct pfq_lang_functional *next;		/* kleisli composition */
};


typedef struct pfq_lang_functional * arguments_t;


/**** function prototypes ****/


typedef ActionQbuff  (*function_ptr_t)  (arguments_t, struct qbuff *);
typedef uint64_t      (*property_ptr_t) (arguments_t, struct qbuff *);
typedef bool	      (*predicate_ptr_t)(arguments_t, struct qbuff *);
typedef int	      (*init_ptr_t)	(arguments_t);
typedef int	      (*fini_ptr_t)	(arguments_t);

typedef struct
{
	struct pfq_lang_functional * fun;

} function_t;


typedef struct
{
	struct pfq_lang_functional * fun;

} predicate_t;


typedef struct
{
	struct pfq_lang_functional * fun;

} property_t;



struct pfq_lang_functional_node
{
	struct pfq_lang_functional fun;

	init_ptr_t	      init;
	fini_ptr_t	      fini;

	bool		      initialized;
};


struct pfq_lang_computation_tree
{
	size_t size;
	struct pfq_lang_functional_node *entry_point;
	struct pfq_lang_functional_node node[];
};


/* function descriptors */

struct pfq_lang_function_descr
{
	const char *    symbol;
	const char *	signature;
	void *		ptr;
	init_ptr_t	init;
	fini_ptr_t	fini;
};

/* class predicates */

static inline bool
is_drop(fanout_t a)
{
	return a.type == fanout_drop;
}

static inline bool
is_copy(fanout_t a)
{
	return a.type == fanout_copy;
}

static inline bool
is_steering(fanout_t a)
{
	return a.type == fanout_steer ||
	       a.type == fanout_double;
}

static inline bool
is_single_steering(fanout_t a)
{
	return a.type == fanout_steer;
}

static inline bool
is_double_steering(fanout_t a)
{
	return a.type == fanout_double;
}


static inline
bool fwd_to_kernel(struct qbuff *buff)
{
	return buff->to_kernel;
}


static inline ActionQbuff
eval_function(function_t f, struct qbuff * buff)
{
	struct pfq_lang_functional *fun = f.fun;
	while (fun) {

                fanout_t *a;
		buff =  ((function_ptr_t)fun->run)(fun, buff).qbuff;
		if (buff == NULL)
			return Pass(buff);

                a = &buff->monad->fanout;
                if (is_drop(*a))
                        return Pass(buff);
                fun = fun->next;
	}

	return Pass(buff);
}


extern struct pfq_lang_function_descr  filter_functions[];
extern struct pfq_lang_function_descr  bloom_functions[];
extern struct pfq_lang_function_descr  vlan_functions[];
extern struct pfq_lang_function_descr  forward_functions[];
extern struct pfq_lang_function_descr  steering_functions[];
extern struct pfq_lang_function_descr  predicate_functions[];
extern struct pfq_lang_function_descr  combinator_functions[];
extern struct pfq_lang_function_descr  property_functions[];
extern struct pfq_lang_function_descr  control_functions[];
extern struct pfq_lang_function_descr  misc_functions[];
extern struct pfq_lang_function_descr  dummy_functions[];


#endif /* PFQ_LANG_MODULE_H */
