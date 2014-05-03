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

#ifndef _FUNCTIONAL_CONDITIONAL_H_
#define _FUNCTIONAL_CONDITIONAL_H_

#include <pf_q-engine.h>

#include "predicate.h"


static inline struct sk_buff *
conditional(arguments_t args, struct sk_buff *skb)
{
        predicate_t expr = get_predicate(args);
        PFQ_CB(skb)->action.right = EVAL_PREDICATE(expr, skb);

        return skb;
}

static inline struct sk_buff *
when(arguments_t args, struct sk_buff *skb)
{
        predicate_t expr = get_predicate(args);
        PFQ_CB(skb)->action.right = EVAL_PREDICATE(expr, skb);
        return skb;
}

static inline struct sk_buff *
unless(arguments_t args, struct sk_buff *skb)
{
        predicate_t expr = get_predicate(args);
        PFQ_CB(skb)->action.right = !EVAL_PREDICATE(expr, skb);
        return skb;
}


#endif /* _FUNCTIONAL_CONDITIONAL_H_ */
