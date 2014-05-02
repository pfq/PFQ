/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#ifndef _PF_Q_ENGINE_H_
#define _PF_Q_ENGINE_H_

#include <linux/kernel.h>
#include <linux/skbuff.h>
#include <linux/pf_q.h>

#include <pf_q-module.h>


#define OF_TYPE(type,a)  	__builtin_choose_expr(__builtin_types_compatible_p(type, typeof(a)), a, (void)0)

#define ARGS_TYPE(a)  		__builtin_choose_expr(__builtin_types_compatible_p(arguments_t, typeof(a)), a, (void)0)
#define FUNCTIONAL_TYPE(a)  	__builtin_choose_expr(__builtin_types_compatible_p(struct pfq_functional *, typeof(a)), a, (void)0)

#define ARG_CAST(arg)  		__builtin_choose_expr(sizeof(arg)  <= sizeof(void *), (ptrdiff_t)arg, (void)0)

#define get_data(type,a) 	__builtin_choose_expr(sizeof(type) <= sizeof(ptrdiff_t), (type)ARGS_TYPE(a)->arg1, (void *)ARGS_TYPE(a)->arg1)
#define get_data2(type,a) 	__builtin_choose_expr(sizeof(type) <= sizeof(ptrdiff_t), (type)ARGS_TYPE(a)->arg2, (void *)ARGS_TYPE(a)->arg2)

#define get_predicate(a) 	({ predicate_t p = { (struct pfq_functional *)ARGS_TYPE(a)->arg2 }; p; })
#define get_predicate2(a) 	({ predicate_t p = { (struct pfq_functional *)ARGS_TYPE(a)->arg3 }; p; })
#define get_property(a) 	({ property_t  p = { (struct pfq_functional *)ARGS_TYPE(a)->arg2 }; p; })
#define get_property2(a) 	({ property_t  p = { (struct pfq_functional *)ARGS_TYPE(a)->arg3 }; p; })


#define make_function(function, argument) \
({\
        struct pfq_functional f = { .fun = OF_TYPE(function_ptr_t, function), .arg1 = ARG_CAST(argument), .arg2 = 0, .arg3 = 0 }; \
        f; \
})

#define make_high_order_function(function, pred) \
({\
        struct pfq_functional f = { .fun = OF_TYPE(function_ptr_t, function), .arg1 = 0, .arg2 = ARG_CAST(FUNCTIONAL_TYPE(pred)), .arg3 = 0 }; \
        f; \
})

#define make_property(property, argument) \
({\
        struct pfq_functional f = { .fun = OF_TYPE(property_ptr_t, property), .arg1 = ARG_CAST(argument), .arg2 = 0, .arg3 = 0 }; \
        f; \
})

#define make_predicate(predicate, argument) \
({\
        struct pfq_functional f = { .fun = OF_TYPE(predicate_ptr_t, predicate), .arg1 = ARG_CAST(argument), .arg2 = 0, .arg3 = 0 }; \
        f; \
})

#define make_high_order_predicate(predicate, argument, property) \
({\
        struct pfq_functional f = { .fun = OF_TYPE(predicate_ptr_t, predicate), .arg1 = ARG_CAST(argument), .arg2 = ARG_CAST(FUNCTIONAL_TYPE(property)), .arg3 = 0 }; \
        f; \
})

#define make_combinator(comb, pred1, pred2) \
({\
        struct pfq_functional f = { .fun = OF_TYPE(predicate_ptr_t, comb), .arg1 = 0, .arg2 = ARG_CAST(FUNCTIONAL_TYPE(pred1)), .arg3 = ARG_CAST(FUNCTIONAL_TYPE(pred2)) }; \
        f; \
})

static inline struct sk_buff *
eval_function(function_t f, struct sk_buff *skb)
{
	return ((function_ptr_t)f.ptr->fun)(f.ptr,skb);
}


static inline bool
eval_predicate(predicate_t p, struct sk_buff const *skb)
{
	return ((predicate_ptr_t)p.ptr->fun)(p.ptr,skb);
}


static inline uint64_t
eval_property(property_t p, struct sk_buff const *skb)
{
	return ((property_ptr_t)p.ptr->fun)(p.ptr,skb);
}


extern int pfq_computation_compile (struct pfq_computation_descr const *descr, computation_t *comp, void *context);

extern computation_t * pfq_computation_alloc(struct pfq_computation_descr const *);
extern void * pfq_context_alloc(struct pfq_computation_descr const *);
extern char * strdup_user(const char __user *str);

extern struct sk_buff *pfq_run(int gid, computation_t *prg, struct sk_buff *skb);

extern void pr_devel_functional_descr(struct pfq_functional_descr const *, int);
extern void pr_devel_computation_descr(struct pfq_computation_descr const *);


#endif /* _PF_Q_ENGINE_H_ */
