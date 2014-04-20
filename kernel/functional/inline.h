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

#include <pf_q-engine.h>
#include <pf_q-predicate.h>

/* high order functions */

#define INLINE_mark 		1
#define INLINE_conditional 	2
#define INLINE_when 		3
#define INLINE_unless 		4



#define CASE(f, call, skb) \
	case INLINE_ ## f: 	return f(call->fun.arg, skb)


#define APPLY_INLINE(call, skb) \
	switch((ptrdiff_t)call->fun.eval) \
	{ 	\
		CASE(mark, call, skb);\
		CASE(conditional, call, skb);\
		CASE(when, call, skb);\
		CASE(unless, call, skb);\
		CASE(id, call, skb);\
	}

static inline struct sk_buff *
mark(argument_t a, struct sk_buff *skb)
{
	const unsigned long *value = argument_as(unsigned long, a);
	set_state(skb, *value);
	return skb;
}

static inline struct sk_buff *
conditional(argument_t a, struct sk_buff *skb)
{
        expression_t * expr = expression(a);
        PFQ_CB(skb)->right = expr->ptr(skb, expr);
        return skb;
}

static inline struct sk_buff *
when(argument_t a, struct sk_buff *skb)
{
        expression_t * expr = expression(a);
        PFQ_CB(skb)->right = expr->ptr(skb, expr);
        return skb;
}

static inline struct sk_buff *
unless(argument_t a, struct sk_buff *skb)
{
        expression_t * expr = expression(a);
        PFQ_CB(skb)->right = !expr->ptr(skb, expr);
        return skb;
}

