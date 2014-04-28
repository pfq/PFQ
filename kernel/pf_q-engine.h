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

#include <linux/pf_q-module.h>


#define BOOLEAN_EXPR_CAST(ptr)  \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(ptr), predicate_t *),    (boolean_expression_t *)ptr, \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(ptr), combinator_t *),   (boolean_expression_t *)ptr, \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(ptr), boolean_expr_t *), (boolean_expression_t *)ptr, \
        (void)0)))


/**** predicate_t : expession_t *****/

typedef struct predicate
{
        boolean_expression_t  	_eval;

        predicate_ptr_t  	fun;
        arguments_t      	args;

} predicate_t;


extern bool predicate_eval(predicate_t *this, struct sk_buff *skb);

static inline predicate_t
make_predicate(predicate_ptr_t fun, const void *arg)
{
        predicate_t p = { ._eval = { .ptr = (boolean_eval_ptr_t)predicate_eval },
                          .fun   = fun,
                          .args  = { .data = arg, .pred = NULL }
                        };
        return p;
}

/**** combinator_t : expession_t *****/

typedef struct combinator
{
        boolean_expression_t    _eval;

        combinator_ptr_t        fun;

        boolean_expression_t *  left;
        boolean_expression_t *  right;

} combinator_t;

bool combinator_eval(combinator_t *this, struct sk_buff *skb);

static inline combinator_t
make_combinator(combinator_ptr_t fun, boolean_expression_t *p1, boolean_expression_t *p2)
{
        combinator_t p = {
                           ._eval = { .ptr = (boolean_eval_ptr_t)combinator_eval },
                           .fun   = fun,
                           .left  = p1,
                           .right = p2
                        };
        return p;
}

/**** boolean expression: predicate or combinator *****/

typedef union
{
        predicate_t     pred;
        combinator_t    comb;

} boolean_expr_t;


/**** function_t  *****/

typedef struct
{
        function_ptr_t  eval;
        arguments_t     args;

} function_t;

static inline function_t
make_function(function_ptr_t fun, const void *arg)
{
        function_t f = { .eval = fun, .args = { .data = arg, .pred = NULL }  };
        return f;
}

static inline function_t
make_high_order_function(function_ptr_t fun, boolean_expression_t *expr)
{
        function_t f = { .eval = fun, .args = { .data = NULL, .pred = expr } };
        return f;
}

static inline struct sk_buff *
function_eval(function_t *this, struct sk_buff *skb)
{
	return this->eval(&this->args, skb);
}

/***** functional_t *****/

typedef struct pfq_functional
{
        union
        {
                function_t              fun;
                boolean_expr_t          expr;
        };

        struct pfq_functional *left;
        struct pfq_functional *right;

} functional_t;


typedef struct pfq_computation
{
        size_t          size;
        functional_t    *entry_point;
        functional_t    fun[];

} computation_t;


#endif /* _PF_Q_ENGINE_H_ */
