/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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


static inline Action_SkBuff
forward_drop(arguments_t args, SkBuff b)
{
        return Drop(b);
}

static inline Action_SkBuff
forward_broadcast(arguments_t args, SkBuff b)
{
        return Broadcast(b);
}

static inline Action_SkBuff
forward_to_kernel(arguments_t args, SkBuff b)
{
        return Pass(to_kernel(b));
}

static inline Action_SkBuff
forward_class(arguments_t args, SkBuff b)
{
        const int c = get_arg(int, args);

        if (!c) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] forward class: internal error!\n");
                return Pass(b);
        }

        return Pass(class(b, (1ULL << c)));
}

static inline Action_SkBuff
forward_deliver(arguments_t args, SkBuff b)
{
        const int c = get_arg(int, args);

        if (!c) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] deliver: internal error!\n");
                return Pass(b);
        }

        return Deliver(b, (1ULL << c));
}

#endif /* _FUNCTIONAL_FORWARD_H_ */
