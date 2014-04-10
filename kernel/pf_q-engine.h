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


#define EXPR_CAST(ptr)  \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(ptr), predicate_t *),    (expression_t *)ptr, \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(ptr), combinator_t *),   (expression_t *)ptr, \
        __builtin_choose_expr(__builtin_types_compatible_p(typeof(ptr), boolean_expr_t *), (expression_t *)ptr, \
        (void)0)))


/**** expression_t: polymorphic expression *****/

typedef struct _expression
{
        bool (*ptr)(struct sk_buff *skb, struct _expression *this);

} expression_t;

typedef bool (*expression_ptr_t)(struct sk_buff *skb, struct _expression *);



/**** expression argument: expression + argument *****/

typedef struct expression_arg
{
        argument_t   arg;
        expression_t *expr;

} expression_arg_t;


/**** predicate_t : expession_t *****/

typedef struct predicate
{
        expression_t     _eval;

        predicate_ptr_t  fun;
        argument_t       arg;

} predicate_t;


extern bool predicate_eval(struct sk_buff *skb, predicate_t *this);

static inline predicate_t
make_predicate(predicate_ptr_t fun, const void *arg)
{
        predicate_t p = { ._eval = { .ptr = (expression_ptr_t)predicate_eval },
                          .fun   = fun,
                          .arg   = { .arg = arg }
                        };
        return p;
}

/**** combinator_t : expession_t *****/

typedef struct combinator
{
        expression_t            _eval;

        combinator_ptr_t        fun;
        expression_t *          left;
        expression_t *          right;

} combinator_t;

bool combinator_eval(struct sk_buff *skb, combinator_t *this);

static inline combinator_t
make_combinator(combinator_ptr_t fun, expression_t *p1, expression_t *p2)
{
        combinator_t p = {
                           ._eval = { .ptr = (expression_ptr_t)combinator_eval },
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
        argument_t      arg;

} function_t;


static inline function_t
make_function(function_ptr_t fun, const void *arg)
{
        function_t f = { .eval = fun, .arg = { .arg = arg }  };
        return f;
}


static inline function_t
make_high_order_function(function_ptr_t fun, expression_t *expr)
{
        function_t f = { .eval = fun, .arg = { .expr = expr } };
        return f;
}


/***** functional_t *****/

typedef struct pfq_callable
{
        union
        {
                function_t              fun;
                boolean_expr_t          expr;
        };

        struct pfq_callable *left;
        struct pfq_callable *right;

} functional_t;


typedef struct pfq_computation
{
        size_t          size;
        size_t          entry_point;
        functional_t    fun[];

} computation_t;



#endif /* _PF_Q_ENGINE_H_ */
