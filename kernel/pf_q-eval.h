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


#ifndef _PF_Q_EVAL_H_
#define _PF_Q_EVAL_H_

#include <pf_q-module.h>

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


#endif /* _PF_Q_EVAL_H_ */
