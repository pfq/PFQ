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

#ifndef _FUNCTIONAL_INLINE_H_
#define _FUNCTIONAL_INLINE_H_

#include <pf_q-engine.h>

#include "predicate.h"

#ifdef PFQ_USE_INLINE_FUN

/* high order functions */

#define INLINE_mark 			1
#define INLINE_conditional 		2
#define INLINE_when 			3
#define INLINE_unless 			4

/* filter functions */

#define INLINE_id 			5
#define INLINE_filter_ip        	6
#define INLINE_filter_ip6       	7
#define INLINE_filter_udp       	8
#define INLINE_filter_tcp       	9
#define INLINE_filter_icmp      	10
#define INLINE_filter_flow      	11
#define INLINE_filter_vlan      	12

/* forward functions */

#define INLINE_forward_drop            	13
#define INLINE_forward_broadcast       	14
#define INLINE_forward_kernel 	       	15

#define CASE_APPLY(f, call, skb) \
	case INLINE_ ## f: return f(call->fun.arg, skb)

#define IF_INLINED_RETURN(call, skb) \
	switch((ptrdiff_t)call->fun.eval) \
	{ 	\
		CASE_APPLY(mark, call, skb);\
		CASE_APPLY(conditional, call, skb);\
		CASE_APPLY(when, call, skb);\
		CASE_APPLY(unless, call, skb);\
		CASE_APPLY(id, call, skb);\
		\
		CASE_APPLY(filter_ip, call, skb);\
		CASE_APPLY(filter_ip6, call, skb);\
		CASE_APPLY(filter_udp, call, skb);\
		CASE_APPLY(filter_tcp, call, skb);\
		CASE_APPLY(filter_icmp, call, skb);\
		CASE_APPLY(filter_flow, call, skb);\
		CASE_APPLY(filter_vlan, call, skb);\
		\
		CASE_APPLY(forward_drop, call, skb);\
		CASE_APPLY(forward_broadcast, call, skb);\
		CASE_APPLY(forward_kernel, call, skb);\
	}


#define INLINE_FUN_ADDR(fun) 	(void *)INLINE_ ## fun
#else
#define INLINE_FUN_ADDR(fun) 	&fun
#endif


/* high order functions */

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

static inline struct sk_buff *
id(argument_t a, struct sk_buff *skb)
{
        return skb;
}

/* filter functions */

static inline struct sk_buff *
filter_ip(argument_t a, struct sk_buff *skb)
{
        return is_ip(skb) ? skb : drop(skb);
}


static inline struct sk_buff *
filter_ip6(argument_t a, struct sk_buff *skb)
{
        return is_ip6(skb) ? skb : drop(skb);
}


static inline struct sk_buff *
filter_udp(argument_t a, struct sk_buff *skb)
{
        return is_udp(skb) ? skb : drop(skb);
}


static inline struct sk_buff *
filter_tcp(argument_t a, struct sk_buff *skb)
{
        return is_tcp(skb) ? skb : drop(skb);
}


static inline struct sk_buff *
filter_icmp(argument_t a, struct sk_buff *skb)
{
        return is_icmp(skb) ? skb : drop(skb);
}


static inline struct sk_buff *
filter_flow(argument_t a, struct sk_buff *skb)
{
        return is_flow(skb) ? skb : drop(skb);
}


static inline struct sk_buff *
filter_vlan(argument_t a, struct sk_buff *skb)
{
        return has_vlan(skb) ? skb : drop(skb);
}

/* forward functions */

static inline struct sk_buff *
forward_drop(argument_t a, struct sk_buff *skb)
{
        return drop(skb);
}

static inline struct sk_buff *
forward_broadcast(argument_t a, struct sk_buff *skb)
{
        return broadcast(skb);
}

static inline struct sk_buff *
forward_kernel(argument_t a, struct sk_buff *skb)
{
        return to_kernel(drop(skb));
}

#endif /* _FUNCTIONAL_INLINE_H_ */
