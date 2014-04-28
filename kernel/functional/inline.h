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


#define INLINE_conditional 		1
#define INLINE_mark 			2
#define INLINE_when 			3
#define INLINE_unless 			4

/* filter functions */

#define INLINE_id 			5
#define INLINE_filter_ip        	6
#define INLINE_filter_ip6       	7
#define INLINE_filter_udp       	8
#define INLINE_filter_icmp      	9
#define INLINE_filter_tcp       	10
#define INLINE_filter_udp6       	11
#define INLINE_filter_tcp6       	12
#define INLINE_filter_icmp6      	13
#define INLINE_filter_flow      	14
#define INLINE_filter_vlan      	15

/* forward functions */

#define INLINE_forward_drop            	16
#define INLINE_forward_broadcast       	17
#define INLINE_forward_kernel 	       	18
#define INLINE_forward_class 		19


#define CASE_APPLY(f, call, skb) \
	case INLINE_ ## f: return f(&call->fun.args, skb)

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
		CASE_APPLY(filter_udp, call, skb);\
		CASE_APPLY(filter_tcp, call, skb);\
		CASE_APPLY(filter_icmp, call, skb);\
		CASE_APPLY(filter_ip6, call, skb);\
		CASE_APPLY(filter_udp6, call, skb);\
		CASE_APPLY(filter_tcp6, call, skb);\
		CASE_APPLY(filter_icmp6, call, skb);\
		CASE_APPLY(filter_flow, call, skb);\
		CASE_APPLY(filter_vlan, call, skb);\
		\
		CASE_APPLY(forward_drop, call, skb);\
		CASE_APPLY(forward_broadcast, call, skb);\
		CASE_APPLY(forward_kernel, call, skb);\
		CASE_APPLY(forward_class, call, skb);\
	}


#define INLINE_FUN(fun) 	(void *)INLINE_ ## fun

#else
#define INLINE_FUN(fun) 	&fun
#endif


/* high order functions */

static inline struct sk_buff *
mark(arguments_t *a, struct sk_buff *skb)
{
	const unsigned long *value = get_data(unsigned long, a);
	set_state(skb, *value);
	return skb;
}

static inline struct sk_buff *
conditional(arguments_t *a, struct sk_buff *skb)
{
        boolean_expression_t * expr = get_predicate(a);
        PFQ_CB(skb)->action.right = expr->ptr(skb, expr);
        return skb;
}

static inline struct sk_buff *
when(arguments_t *a, struct sk_buff *skb)
{
        boolean_expression_t * expr = get_predicate(a);
        PFQ_CB(skb)->action.right = expr->ptr(skb, expr);
        return skb;
}

static inline struct sk_buff *
unless(arguments_t *a, struct sk_buff *skb)
{
        boolean_expression_t * expr = get_predicate(a);
        PFQ_CB(skb)->action.right = !expr->ptr(skb, expr);
        return skb;
}

static inline struct sk_buff *
id(arguments_t *a, struct sk_buff *skb)
{
        return skb;
}

/* filter functions */

static inline struct sk_buff *
filter_ip(arguments_t *a, struct sk_buff *skb)
{
        return is_ip(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_ip6(arguments_t *a, struct sk_buff *skb)
{
        return is_ip6(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_udp(arguments_t *a, struct sk_buff *skb)
{
        return is_udp(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_udp6(arguments_t *a, struct sk_buff *skb)
{
        return is_udp6(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_tcp(arguments_t *a, struct sk_buff *skb)
{
        return is_tcp(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_tcp6(arguments_t *a, struct sk_buff *skb)
{
        return is_tcp6(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_icmp(arguments_t *a, struct sk_buff *skb)
{
        return is_icmp(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_icmp6(arguments_t *a, struct sk_buff *skb)
{
        return is_icmp6(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_flow(arguments_t *a, struct sk_buff *skb)
{
        return is_flow(skb) ? skb : drop(skb);
}

static inline struct sk_buff *
filter_vlan(arguments_t *a, struct sk_buff *skb)
{
        return has_vlan(skb) ? skb : drop(skb);
}


/* forward functions */

static inline struct sk_buff *
forward_drop(arguments_t *a, struct sk_buff *skb)
{
        return drop(skb);
}

static inline struct sk_buff *
forward_broadcast(arguments_t *a, struct sk_buff *skb)
{
        return broadcast(skb);
}

static inline struct sk_buff *
forward_kernel(arguments_t *a, struct sk_buff *skb)
{
        return to_kernel(drop(skb));
}

static inline struct sk_buff *
forward_class(arguments_t *a, struct sk_buff *skb)
{
        const int *c = get_data(int, a);

        if (!c) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] forward class: internal error!\n");
                return skb;
        }

        return class(skb, (1ULL << *c));
}


#endif /* _FUNCTIONAL_INLINE_H_ */
