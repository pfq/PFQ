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

#ifndef _PF_Q_MODULE_H_
#define _PF_Q_MODULE_H_

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

#define get_arg(type,a) 	get_arg_0(type,a)
#define set_arg(type,a) 	set_arg_0(type,a)

#define get_arg_0(type,a) 	__builtin_choose_expr(sizeof(type) <= sizeof(uint64_t), *(type *)&ARGS_TYPE(a)->arg[0], (void *)ARGS_TYPE(a)->arg[0].value)
#define get_arg_1(type,a) 	__builtin_choose_expr(sizeof(type) <= sizeof(uint64_t), *(type *)&ARGS_TYPE(a)->arg[1], (void *)ARGS_TYPE(a)->arg[1].value)
#define get_arg_2(type,a) 	__builtin_choose_expr(sizeof(type) <= sizeof(uint64_t), *(type *)&ARGS_TYPE(a)->arg[2], (void *)ARGS_TYPE(a)->arg[2].value)
#define get_arg_3(type,a) 	__builtin_choose_expr(sizeof(type) <= sizeof(uint64_t), *(type *)&ARGS_TYPE(a)->arg[3], (void *)ARGS_TYPE(a)->arg[3].value)

#define set_arg_0(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(ptrdiff_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[0].value) = v, (void)0)
#define set_arg_1(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(ptrdiff_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[1].value) = v, (void)0)
#define set_arg_2(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(ptrdiff_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[2].value) = v, (void)0)
#define set_arg_3(a, v)		__builtin_choose_expr(sizeof(typeof(v)) <= sizeof(ptrdiff_t), *(typeof(v) *)(&ARGS_TYPE(a)->arg[3].value) = v, (void)0)

#define get_array_0(type,a) 	((type *)ARGS_TYPE(a)->arg[0].value)
#define get_array_1(type,a) 	((type *)ARGS_TYPE(a)->arg[1].value)
#define get_array_2(type,a) 	((type *)ARGS_TYPE(a)->arg[2].value)
#define get_array_3(type,a) 	((type *)ARGS_TYPE(a)->arg[3].value)

#define get_array_len_0(a) 	(ARGS_TYPE(a)->arg[0].nelem)
#define get_array_len_1(a) 	(ARGS_TYPE(a)->arg[1].nelem)
#define get_array_len_2(a) 	(ARGS_TYPE(a)->arg[2].nelem)
#define get_array_len_3(a) 	(ARGS_TYPE(a)->arg[3].nelem)

#define get_array(type, a) 	get_array_0(type,a)
#define get_array_len(a) 	get_array_len_0(a)

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
	struct pfq_functional_arg arg[4];       /* arguments */
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

#endif /* _PF_Q_MODULE_H_ */
