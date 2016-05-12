/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PFQ_LANG_COMBINATOR_H
#define PFQ_LANG_COMBINATOR_H

#include <engine/lang/predicate.h>


static inline
bool not(arguments_t args, SkBuff b)
{
	predicate_t p1 = GET_ARG(predicate_t, args);

        return !EVAL_PREDICATE(p1,b);
}

static inline
bool or(arguments_t args, SkBuff b)
{
	predicate_t p1 = GET_ARG_0(predicate_t, args);
	predicate_t p2 = GET_ARG_1(predicate_t, args);

        return EVAL_PREDICATE(p1,b) || EVAL_PREDICATE(p2, b);
}


static inline
bool and(arguments_t args, SkBuff b)
{
	predicate_t p1 = GET_ARG_0(predicate_t, args);
	predicate_t p2 = GET_ARG_1(predicate_t, args);

        return EVAL_PREDICATE(p1, b) && EVAL_PREDICATE(p2, b);
}


static inline
bool xor(arguments_t args, SkBuff b)
{
	predicate_t p1 = GET_ARG_0(predicate_t, args);
	predicate_t p2 = GET_ARG_1(predicate_t, args);

        return EVAL_PREDICATE(p1, b) != EVAL_PREDICATE(p2, b);
}


#endif /* PFQ_LANG_COMBINATOR_H */
