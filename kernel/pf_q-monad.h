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

#ifndef _PF_Q_MONAD_H_
#define _PF_Q_MONAD_H_

#include <pf_q-group.h>
#include <pf_q-skbuff.h>
#include <pf_q-macro.h>
#include <pf_q-GC.h>
#include <pf_q-sparse.h>

/* The Action monad */

#define MakeAction(type) \
typedef struct \
{ \
	type 	value; \
} Action_ ## type;


MakeAction(SkBuff);


/* fanout: actions types */

enum fanout
{
        fanout_drop  = 0,
        fanout_copy  = 1,
        fanout_steer = 2
};


typedef struct
{
        unsigned long 	class_mask;
        uint32_t 	hash;
        uint8_t  	type;

} fanout_t;


/* Action monad */

struct pfq_monad
{
        fanout_t 		fanout;
        unsigned long 		state;
        struct pfq_group	*group;
};


/* Fanout constructors */

static inline
Action_SkBuff
Pass(SkBuff b)
{
	Action_SkBuff ret = { b };
        return ret;
}

static inline
Action_SkBuff
Drop(SkBuff b)
{
	Action_SkBuff ret = { b };
        PFQ_CB(b.skb)->monad->fanout.type = fanout_drop;
        return ret;
}


static inline
Action_SkBuff
Copy(SkBuff b)
{
	Action_SkBuff ret = { b };
        PFQ_CB(b.skb)->monad->fanout.type = fanout_copy;
        return ret;
}

static inline
Action_SkBuff
Broadcast(SkBuff b)
{
	Action_SkBuff ret = { b };
        fanout_t * a  = &PFQ_CB(b.skb)->monad->fanout;
        a->class_mask = Q_CLASS_ANY;
        a->type       = fanout_copy;
        return ret;
}

static inline
Action_SkBuff
Steering(SkBuff b, uint32_t hash)
{
	Action_SkBuff ret = { b };
        fanout_t * a = &PFQ_CB(b.skb)->monad->fanout;
        a->type  = fanout_steer;
        a->hash  = hash;
        return ret;
}

static inline
Action_SkBuff
Deliver(SkBuff b, unsigned long class_mask)
{
	Action_SkBuff ret = { b };
        fanout_t * a  = &PFQ_CB(b.skb)->monad->fanout;
        a->class_mask = class_mask;
        a->type       = fanout_copy;
        return ret;
}

static inline
Action_SkBuff
Dispatch(SkBuff b, unsigned long class_mask, uint32_t hash)
{
	Action_SkBuff ret = { b };
        fanout_t * a  = &PFQ_CB(b.skb)->monad->fanout;
        a->class_mask = class_mask;
        a->type       = fanout_steer;
        a->hash       = hash;
        return ret;
}

/* to_kernel: set the skb to be passed to kernel */

static inline
SkBuff
to_kernel(SkBuff b)
{
        PFQ_CB(b.skb)->log->to_kernel = true;
        return b;
}

static inline
SkBuff
class(SkBuff b, uint64_t class_mask)
{
        PFQ_CB(b.skb)->monad->fanout.class_mask = class_mask;
        return b;
}

/* utility function: counter */

static inline
sparse_counter_t * get_counter(SkBuff b, int n)
{
        if (n < 0 || n >= Q_MAX_COUNTERS)
                return NULL;

	return & PFQ_CB(b.skb)->monad->group->context.counter[n];
}


/* utility function: mark, volatile state, persistent state */


static inline
unsigned long get_mark(SkBuff b)
{
        return PFQ_CB(b.skb)->mark;
}

static inline
void set_mark(SkBuff b, unsigned long mark)
{
        PFQ_CB(b.skb)->mark = mark;
}


static inline
unsigned long get_state(SkBuff b)
{
        return PFQ_CB(b.skb)->monad->state;
}

static inline
void set_state(SkBuff b, unsigned long state)
{
        PFQ_CB(b.skb)->monad->state = state;
}


static inline void *
__get_persistent(SkBuff b, int n)
{
	struct pfq_group_persistent *ctx = &PFQ_CB(b.skb)->monad->group->context;
	spin_lock(&ctx->persistent[n].lock);
	return ctx->persistent[n].memory;
}

#define get_persistent(type, b, n) __builtin_choose_expr(sizeof(type) <= 64, __get_persistent(b, n) , (void)0)

static inline
void put_persistent(SkBuff b, int n)
{
	struct pfq_group_persistent *ctx = &PFQ_CB(b.skb)->monad->group->context;
	spin_unlock(&ctx->persistent[n].lock);
}

#endif /* _PF_Q_MONAD_H_ */
