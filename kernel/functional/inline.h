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

#ifndef _FUNCTIONAL_INLINE_H_
#define _FUNCTIONAL_INLINE_H_

#include <pf_q-module.h>

/* INLINE_FUN macro */

#ifdef PFQ_USE_INLINE_FUN
#define INLINE_FUN(fun) 	(void *)INLINE_ ## fun
#else
#define INLINE_FUN(fun) 	&fun
#endif

#ifdef PFQ_USE_INLINE_FUN

/* high order functions */

#define INLINE_unit 	       		1
#define INLINE_conditional 		2
#define INLINE_mark 			3
#define INLINE_when 			4
#define INLINE_unless 			5

/* filter functions */

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

#define INLINE_forward_drop         	16
#define INLINE_forward_broadcast    	17
#define INLINE_forward_kernel 	    	18
#define INLINE_forward_class 		19


#define APPLY(f, skb) ({ \
	typeof(((function_ptr_t)f.fun->ptr)(f.fun, skb)) ret; \
	if ((size_t)f.fun->ptr < 1000) { \
		ret = 0; \
		if (printk_ratelimit()) \
			printk(KERN_INFO "[PFQ] inline function: internal error! (vaddr = %p)\n", f.fun->ptr); \
	} \
	else { \
        	ret = ((function_ptr_t)f.fun->ptr)(f.fun, skb); \
	} \
        ret; \
	})

#define CASE_INLINE(name, f, skb) \
	case INLINE_ ## name: ret = name(f.fun, skb); break;

#define EVAL_FUNCTION(f, skb) ({\
	typeof(((function_ptr_t)f.fun->ptr)(f.fun, skb)) ret; \
	switch((ptrdiff_t)f.fun->ptr) \
	{ 	\
		CASE_INLINE(unit, f, skb);\
		CASE_INLINE(mark, f, skb);\
		CASE_INLINE(conditional, f, skb);\
		CASE_INLINE(when, f, skb);\
		CASE_INLINE(unless, f, skb);\
		\
		CASE_INLINE(filter_ip, f, skb);\
		CASE_INLINE(filter_udp, f, skb);\
		CASE_INLINE(filter_tcp, f, skb);\
		CASE_INLINE(filter_icmp, f, skb);\
		CASE_INLINE(filter_ip6, f, skb);\
		CASE_INLINE(filter_udp6, f, skb);\
		CASE_INLINE(filter_tcp6, f, skb);\
		CASE_INLINE(filter_icmp6, f, skb);\
		CASE_INLINE(filter_flow, f, skb);\
		CASE_INLINE(filter_vlan, f, skb);\
		\
		CASE_INLINE(forward_drop, f, skb);\
		CASE_INLINE(forward_broadcast, f, skb);\
		CASE_INLINE(forward_kernel, f, skb);\
		CASE_INLINE(forward_class, f, skb);\
		default: ret = APPLY(f, skb); \
	} \
	ret; })

#define EVAL_PROPERTY(f, skb) 	((property_ptr_t)f.fun->ptr)(f.fun, skb)
#define EVAL_PREDICATE(f, skb) 	((predicate_ptr_t)f.fun->ptr)(f.fun, skb)

#else

#define EVAL_FUNCTION(f, skb) 	((function_ptr_t)f.fun->ptr)(f.fun, skb)
#define EVAL_PROPERTY(f, skb) 	((property_ptr_t)f.fun->ptr)(f.fun, skb)
#define EVAL_PREDICATE(f, skb) 	((predicate_ptr_t)f.fun->ptr)(f.fun, skb)

#endif


#endif /* _FUNCTIONAL_INLINE_H_ */
