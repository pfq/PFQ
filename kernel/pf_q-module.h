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

#ifndef PF_Q_MODULE_H
#define PF_Q_MODULE_H

#include <linux/kernel.h>
#include <linux/version.h>

#include <linux/pf_q.h>

#include <linux/skbuff.h>
#include <linux/ip.h>
#include <linux/icmp.h>
#include <linux/ipv6.h>
#include <linux/if_ether.h>
#include <linux/if_vlan.h>

#include <pf_q-sparse.h>
#include <pf_q-monad.h>
#include <pf_q-GC.h>

/**** macros ****/


#define JUST(x) 		((1ULL<<31) | x)
#define IS_JUST(x)		((1ULL<<31) & x)
#define FROM_JUST(x)		(~(1ULL<<31) & x)
#define NOTHING 		0

#define ARGS_TYPE(a)  		__builtin_choose_expr(__builtin_types_compatible_p(arguments_t, typeof(a)), a, (void)0)

#define EVAL_FUNCTION(f,  b) 	((function_ptr_t)f.fun->ptr)(f.fun,  b)
#define EVAL_PROPERTY(f,  b) 	((property_ptr_t)f.fun->ptr)(f.fun,  b)
#define EVAL_PREDICATE(f, b) 	((predicate_ptr_t)f.fun->ptr)(f.fun, b)


#define get_arg0(type,a) 	__builtin_choose_expr(sizeof(type) <= sizeof(uint64_t), *(type *)&ARGS_TYPE(a)->arg[0], (void *)ARGS_TYPE(a)->arg[0].value)
#define get_arg1(type,a) 	__builtin_choose_expr(sizeof(type) <= sizeof(uint64_t), *(type *)&ARGS_TYPE(a)->arg[1], (void *)ARGS_TYPE(a)->arg[1].value)
#define get_arg2(type,a) 	__builtin_choose_expr(sizeof(type) <= sizeof(uint64_t), *(type *)&ARGS_TYPE(a)->arg[2], (void *)ARGS_TYPE(a)->arg[2].value)
#define get_arg3(type,a) 	__builtin_choose_expr(sizeof(type) <= sizeof(uint64_t), *(type *)&ARGS_TYPE(a)->arg[3], (void *)ARGS_TYPE(a)->arg[3].value)
#define get_arg4(type,a) 	__builtin_choose_expr(sizeof(type) <= sizeof(uint64_t), *(type *)&ARGS_TYPE(a)->arg[4], (void *)ARGS_TYPE(a)->arg[4].value)
#define get_arg5(type,a) 	__builtin_choose_expr(sizeof(type) <= sizeof(uint64_t), *(type *)&ARGS_TYPE(a)->arg[5], (void *)ARGS_TYPE(a)->arg[5].value)
#define get_arg6(type,a) 	__builtin_choose_expr(sizeof(type) <= sizeof(uint64_t), *(type *)&ARGS_TYPE(a)->arg[6], (void *)ARGS_TYPE(a)->arg[6].value)
#define get_arg7(type,a) 	__builtin_choose_expr(sizeof(type) <= sizeof(uint64_t), *(type *)&ARGS_TYPE(a)->arg[7], (void *)ARGS_TYPE(a)->arg[7].value)

#define set_arg0(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(ptrdiff_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[0].value) = v, (void)0)
#define set_arg1(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(ptrdiff_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[1].value) = v, (void)0)
#define set_arg2(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(ptrdiff_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[2].value) = v, (void)0)
#define set_arg3(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(ptrdiff_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[3].value) = v, (void)0)
#define set_arg4(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(ptrdiff_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[4].value) = v, (void)0)
#define set_arg5(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(ptrdiff_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[5].value) = v, (void)0)
#define set_arg6(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(ptrdiff_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[6].value) = v, (void)0)
#define set_arg7(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(ptrdiff_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[7].value) = v, (void)0)

#define get_array0(type,a) 	((type *)ARGS_TYPE(a)->arg[0].value)
#define get_array1(type,a) 	((type *)ARGS_TYPE(a)->arg[1].value)
#define get_array2(type,a) 	((type *)ARGS_TYPE(a)->arg[2].value)
#define get_array3(type,a) 	((type *)ARGS_TYPE(a)->arg[3].value)
#define get_array4(type,a) 	((type *)ARGS_TYPE(a)->arg[4].value)
#define get_array5(type,a) 	((type *)ARGS_TYPE(a)->arg[5].value)
#define get_array6(type,a) 	((type *)ARGS_TYPE(a)->arg[6].value)
#define get_array7(type,a) 	((type *)ARGS_TYPE(a)->arg[7].value)

#define get_len_array0(a) 	(ARGS_TYPE(a)->arg[0].nelem)
#define get_len_array1(a) 	(ARGS_TYPE(a)->arg[1].nelem)
#define get_len_array2(a) 	(ARGS_TYPE(a)->arg[2].nelem)
#define get_len_array3(a) 	(ARGS_TYPE(a)->arg[3].nelem)
#define get_len_array4(a) 	(ARGS_TYPE(a)->arg[4].nelem)
#define get_len_array5(a) 	(ARGS_TYPE(a)->arg[5].nelem)
#define get_len_array6(a) 	(ARGS_TYPE(a)->arg[6].nelem)
#define get_len_array7(a) 	(ARGS_TYPE(a)->arg[7].nelem)

#define get_arg(type,a) 	get_arg0(type,a)
#define set_arg(type,a) 	set_arg0(type,a)
#define get_array(type, a) 	get_array0(type,a)
#define get_array_len(a) 	get_len_array0(a)

#define get_arg_num(n,type,a) 	__builtin_choose_expr(sizeof(type) <= sizeof(uint64_t), *(type *)&ARGS_TYPE(a)->arg[n], (void *)ARGS_TYPE(a)->arg[n].value)
#define set_arg_num(n,a, v)	__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(ptrdiff_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[n].value) = v, (void)0)
#define get_array_num(n,type,a) ((type *)ARGS_TYPE(a)->arg[n].value)
#define get_len_array_num(n,a) 	(ARGS_TYPE(a)->arg[n].nelem)

#define make_mask(prefix)       htonl(~((1ULL << (32-prefix)) - 1))


/**** generic functional type ****/


struct pfq_function_descr;
struct pfq_exec;

extern struct list_head pfq_lang_functions;
extern int pfq_symtable_register_functions  (const char *module, struct list_head *category, struct pfq_function_descr *fun);
extern int pfq_symtable_unregister_functions(const char *module, struct list_head *category, struct pfq_function_descr *fun);


struct pfq_functional_arg
{
        ptrdiff_t     value;
        size_t 	      nelem; 	/* > 0 array */
};


struct pfq_functional
{
	void *  ptr; 				/* pointer to function */
	struct pfq_functional_arg arg[8];       /* arguments */
};


typedef struct pfq_functional *  arguments_t;


/**** function prototypes ****/


typedef Action_SkBuff (*function_ptr_t) (arguments_t, SkBuff);
typedef uint64_t      (*property_ptr_t) (arguments_t, SkBuff);
typedef bool 	      (*predicate_ptr_t)(arguments_t, SkBuff);
typedef int 	      (*init_ptr_t) 	(arguments_t);
typedef int 	      (*fini_ptr_t) 	(arguments_t);

typedef struct
{
	struct pfq_functional * fun;

} function_t;


typedef struct
{
	struct pfq_functional * fun;

} predicate_t;


typedef struct
{
	struct pfq_functional * fun;

} property_t;


struct pfq_functional_node
{
 	struct pfq_functional fun;

 	init_ptr_t 	      init;
 	fini_ptr_t 	      fini;

	bool 		      initialized;

	struct pfq_functional_node *next;
};


struct pfq_computation_tree
{
        size_t size;
        struct pfq_functional_node *entry_point;
        struct pfq_functional_node node[];
};


/* function descriptors */

struct pfq_function_descr
{
        const char *    symbol;
        const char *	signature;
        void * 		ptr;
        init_ptr_t 	init;
        fini_ptr_t 	fini;
};

/* class predicates */

static inline bool
is_drop(fanout_t a)
{
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(3,9,0))
        BUILD_BUG_ON_MSG(sizeof(struct pfq_cb) > sizeof(((struct sk_buff *)0)->cb), "pfq control buffer overflow");
#endif
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
        return a.type == fanout_steer;
}

static inline
bool fwd_to_kernel(struct sk_buff *skb)
{
	return PFQ_CB(skb)->log->to_kernel != 0;
}


extern struct pfq_function_descr  filter_functions[];
extern struct pfq_function_descr  bloom_functions[];
extern struct pfq_function_descr  vlan_functions[];
extern struct pfq_function_descr  forward_functions[];
extern struct pfq_function_descr  steering_functions[];
extern struct pfq_function_descr  predicate_functions[];
extern struct pfq_function_descr  combinator_functions[];
extern struct pfq_function_descr  property_functions[];
extern struct pfq_function_descr  high_order_functions[];
extern struct pfq_function_descr  misc_functions[];
extern struct pfq_function_descr  dummy_functions[];


#endif /* PF_Q_MODULE_H */
