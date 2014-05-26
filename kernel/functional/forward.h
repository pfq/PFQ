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

#ifndef _FUNCTIONAL_FORWARD_H_
#define _FUNCTIONAL_FORWARD_H_

#include <pf_q-module.h>

#include "predicate.h"


static inline struct sk_buff *
forward_drop(arguments_t args, struct sk_buff *skb)
{
        return drop(skb);
}

static inline struct sk_buff *
forward_broadcast(arguments_t args, struct sk_buff *skb)
{
        return broadcast(skb);
}

static inline struct sk_buff *
forward_kernel(arguments_t args, struct sk_buff *skb)
{
        return to_kernel(skb);
}

static inline struct sk_buff *
forward_class(arguments_t args, struct sk_buff *skb)
{
        const int c = get_data(int, args);

        if (!c) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] forward class: internal error!\n");
                return skb;
        }

        return class(skb, (1ULL << c));
}


#endif /* _FUNCTIONAL_FORWARD_H_ */
