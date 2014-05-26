/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#define make_function(function, argument) \
({\
        struct pfq_functional f = { .ptr  = ASSERT_TYPE(function_ptr_t, function), \
 				    .arg1 = ARG_CAST(argument), \
 				    .arg2 = 0, \
 				    .arg3 = 0 }; \
        f; \
})

#define make_high_order_function(function, pred) \
({\
        struct pfq_functional f = { .ptr  = ASSERT_TYPE(function_ptr_t, function), \
 				    .arg1 = 0, \
 				    .arg2 = ARG_CAST(FUNCTIONAL_TYPE(pred)),\
 				    .arg3 = 0 }; \
        f; \
})

#define make_property(property, argument) \
({\
        struct pfq_functional f = { .ptr  = ASSERT_TYPE(property_ptr_t, property), \
 				    .arg1 = ARG_CAST(argument), \
 				    .arg2 = 0, \
 				    .arg3 = 0 }; \
        f; \
})

#define make_predicate(predicate, argument) \
({\
        struct pfq_functional f = { .ptr  = ASSERT_TYPE(predicate_ptr_t, predicate), \
 				    .arg1 = ARG_CAST(argument), \
 				    .arg2 = 0, \
 				    .arg3 = 0 }; \
        f; \
})

#define make_high_order_predicate(predicate, argument, property) \
({\
        struct pfq_functional f = { .ptr  = ASSERT_TYPE(predicate_ptr_t, predicate), \
 				    .arg1 = ARG_CAST(argument), \
 				    .arg2 = ARG_CAST(FUNCTIONAL_TYPE(property)), \
 				    .arg3 = 0 }; \
        f; \
})

#define make_combinator(comb, pred1, pred2) \
({\
        struct pfq_functional f = { .ptr  = ASSERT_TYPE(predicate_ptr_t, comb), \
 				    .arg1 = 0, \
 				    .arg2 = ARG_CAST(FUNCTIONAL_TYPE(pred1)),\
 				    .arg3 = ARG_CAST(FUNCTIONAL_TYPE(pred2)) }; \
        f; \
})


extern int pfq_computation_rtlink(struct pfq_computation_descr const *descr, computation_t *comp, void *context);
extern int pfq_computation_init(computation_t *comp);
extern int pfq_computation_fini(computation_t *comp);

extern computation_t * pfq_computation_alloc(struct pfq_computation_descr const *);
extern void * pfq_context_alloc(struct pfq_computation_descr const *);
extern char * strdup_user(const char __user *str);

extern struct sk_buff *pfq_run(int gid, computation_t *prg, struct sk_buff *skb);

extern void pr_devel_functional_descr(struct pfq_functional_descr const *, int);
extern void pr_devel_computation_descr(struct pfq_computation_descr const *);


#endif /* _PF_Q_ENGINE_H_ */
