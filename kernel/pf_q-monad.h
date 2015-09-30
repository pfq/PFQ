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

#ifndef PF_Q_MONAD_H
#define PF_Q_MONAD_H

#include <pf_q-group.h>
#include <pf_q-skbuff.h>
#include <pf_q-define.h>
#include <pf_q-GC.h>
#include <pf_q-sparse.h>

/* The Action monad */

#define MakeAction(type, name) \
typedef struct \
{ \
	type	name; \
} Action ## type;


MakeAction(SkBuff, skb);


/* fanout: actions types */

enum fanout
{
        fanout_drop  = 0,
        fanout_copy  = 1,
        fanout_steer = 2
};


typedef struct
{
        unsigned long	class_mask;
        uint32_t	hash;
        uint8_t		type;

} fanout_t;


/* Action monad */

struct pfq_monad
{
        struct pfq_group	*group;
        uint32_t		state;
        fanout_t		fanout;
};


/* Fanout constructors */

static inline
ActionSkBuff
Pass(SkBuff skb)
{
        return (ActionSkBuff){skb};
}

static inline
ActionSkBuff
Drop(SkBuff skb)
{
        PFQ_CB(skb)->monad->fanout.type = fanout_drop;
        return (ActionSkBuff){skb};
}

static inline
ActionSkBuff
Copy(SkBuff skb)
{
        PFQ_CB(skb)->monad->fanout.type = fanout_copy;
        return (ActionSkBuff){skb};
}

static inline
ActionSkBuff
Broadcast(SkBuff skb)
{
        fanout_t * a  = &PFQ_CB(skb)->monad->fanout;
        a->class_mask = Q_CLASS_ANY;
        a->type       = fanout_copy;
        return (ActionSkBuff){skb};
}

static inline
ActionSkBuff
Steering(SkBuff skb, uint32_t hash)
{
        fanout_t * a = &PFQ_CB(skb)->monad->fanout;
        a->type  = fanout_steer;
        a->hash  = hash;
        return (ActionSkBuff){skb};
}

static inline
ActionSkBuff
Deliver(SkBuff skb, unsigned long class_mask)
{
        fanout_t * a  = &PFQ_CB(skb)->monad->fanout;
        a->class_mask = class_mask;
        a->type       = fanout_copy;
        return (ActionSkBuff){skb};
}

static inline
ActionSkBuff
Dispatch(SkBuff skb, unsigned long class_mask, uint32_t hash)
{
        fanout_t * a  = &PFQ_CB(skb)->monad->fanout;
        a->class_mask = class_mask;
        a->type       = fanout_steer;
        a->hash       = hash;
        return (ActionSkBuff){skb};
}

/* to_kernel: set the skb to be passed to kernel */

static inline
SkBuff
to_kernel(SkBuff skb)
{
        PFQ_CB(skb)->log->to_kernel = true;
        return skb;
}


static inline
SkBuff
class(SkBuff skb, uint64_t class_mask)
{
        PFQ_CB(skb)->monad->fanout.class_mask = class_mask;
        return skb;
}

/* utility function: counter */

static inline
struct pfq_group_counters * get_group_counters(SkBuff skb)
{
	return this_cpu_ptr(PFQ_CB(skb)->monad->group->counters);
}


/* utility function: mark, volatile state, persistent state */


static inline
uint32_t get_mark(SkBuff skb)
{
        return skb->mark;
}

static inline
void set_mark(SkBuff skb, uint32_t value)
{
        skb->mark = value;
}



static inline
uint32_t get_state(SkBuff skb)
{
        return PFQ_CB(skb)->monad->state;
}

static inline
void set_state(SkBuff skb, uint32_t state)
{
        PFQ_CB(skb)->monad->state = state;
}


static inline
struct pfq_group_stats *get_group_stats(SkBuff skb)
{
	return this_cpu_ptr(PFQ_CB(skb)->monad->group->stats);
}


#endif /* PF_Q_MONAD_H */
