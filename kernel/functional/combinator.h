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

#ifndef _FUNCTIONAL_COMBINATOR_H_
#define _FUNCTIONAL_COMBINATOR_H_


#include <pf_q-engine.h>

#include "predicate.h"
#include "inline.h"

#ifdef PFQ_USE_INLINE_FUN

/* combinator functions */

#define INLINE_or 			100
#define INLINE_and 			101
#define INLINE_xor 			102


#define CASE_COMBINATOR(f, call, skb) \
	case INLINE_ ## f: return f(call->left, call->right, skb)


#define RETURN_EVAL_COMBINATOR(call, skb) \
	switch((ptrdiff_t)call->fun) \
	{ 	\
		CASE_COMBINATOR(or,  call, skb);\
		CASE_COMBINATOR(and, call, skb);\
		CASE_COMBINATOR(xor, call, skb);\
	}

#endif

static inline
bool or(boolean_expression_t *p1, boolean_expression_t *p2, struct sk_buff const *skb)
{
        return p1->ptr(p1, skb) || p2->ptr(p2, skb);
}


static inline
bool and(boolean_expression_t *p1, boolean_expression_t *p2, struct sk_buff const *skb)
{
        return p1->ptr(p1, skb) && p2->ptr(p2, skb);
}


static inline
bool xor(boolean_expression_t *p1, boolean_expression_t *p2, struct sk_buff const *skb)
{
        return p1->ptr(p1, skb) != p2->ptr(p2, skb);
}


#endif /* _FUNCTIONAL_COMBINATOR_H_ */
