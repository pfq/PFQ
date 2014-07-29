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

#ifndef _PF_Q_MONAD_H_
#define _PF_Q_MONAD_H_

#include <pf_q-skbuff.h>
#include <pf_q-macro.h>
#include <pf_q-GC.h>

/* persistent state */


struct pergroup_context
{
        sparse_counter_t counter[Q_MAX_COUNTERS];

	struct _persistent {

		spinlock_t 	lock;
		char 		memory[Q_PERSISTENT_MEM];

	} persistent [Q_MAX_PERSISTENT];
};

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
        struct pergroup_context *persistent;
};


/* fanout: copy */

static inline
struct sk_buff *
copy(struct sk_buff *skb)
{
        fanout_t * a = & PFQ_CB(skb)->monad->fanout;
        a->type = fanout_copy;
        return skb;
}

/* drop: ignore this packet for the current group */

static inline
struct sk_buff *
drop(struct sk_buff *skb)
{
        fanout_t * a = & PFQ_CB(skb)->monad->fanout;
        a->type = fanout_drop;
        return skb;
}

/* class skb: specifies only the class for the packet */

static inline
struct sk_buff *
class(struct sk_buff *skb, uint64_t class_mask)
{
        fanout_t * a = & PFQ_CB(skb)->monad->fanout;
        a->class_mask = class_mask;
        return skb;
}

/* broadcast: broadcast the skb all the classes */

static inline
struct sk_buff *
broadcast(struct sk_buff *skb)
{
        fanout_t * a = & PFQ_CB(skb)->monad->fanout;
        a->type  = fanout_copy;
        a->class_mask = Q_CLASS_ANY;
        return skb;
}

/* steering skb: for this group, steer the skb across sockets (by means of hash) */

static inline
struct sk_buff *
steering(struct sk_buff *skb, uint32_t hash)
{
        fanout_t * a = & PFQ_CB(skb)->monad->fanout;
        a->type  = fanout_steer;
        a->hash  = hash;
        return skb;
}

/* deliver: for this group, deliver the skb to the sockets of the given classes */

static inline
struct sk_buff *
deliver(struct sk_buff *skb, unsigned long class_mask)
{
        fanout_t * a  = & PFQ_CB(skb)->monad->fanout;
        a->type       = fanout_copy;
        a->class_mask = class_mask;
        return skb;
}

/* class + steering: for this group, steer the skb across sockets of the given classes (by means of hash) */

static inline
struct sk_buff *
dispatch(struct sk_buff *skb, unsigned long class_mask, uint32_t hash)
{
        fanout_t * a = & PFQ_CB(skb)->monad->fanout;
        a->type  = fanout_steer;
        a->class_mask = class_mask;
        a->hash  = hash;
        return skb;
}

/* to_kernel: set the skb to be passed to kernel */

static inline
struct sk_buff *
to_kernel(struct sk_buff *skb)
{
        PFQ_CB(skb)->log->to_kernel = true;
        return skb;
}

/* utility function: counter */

static inline
sparse_counter_t * get_counter(struct sk_buff *skb, int n)
{
        if (n < 0 || n >= Q_MAX_COUNTERS)
                return NULL;

	return & PFQ_CB(skb)->monad->persistent->counter[n];
}


/* utility function: volatile state, persistent state */


static inline
unsigned long get_state(struct sk_buff const *skb)
{
        return PFQ_CB(skb)->monad->state;
}

static inline
void set_state(struct sk_buff *skb, unsigned long state)
{
        PFQ_CB(skb)->monad->state = state;
}

static inline void *
__get_persistent(struct sk_buff const *skb, int n)
{
	struct pergroup_context *ctx = PFQ_CB(skb)->monad->persistent;
	spin_lock(&ctx->persistent[n].lock);
	return ctx->persistent[n].memory;
}

#define get_persistent(type, skb, n) __builtin_choose_expr(sizeof(type) <= 64, __get_persistent(skb, n) , (void)0)

static inline
void put_persistent(struct sk_buff const *skb, int n)
{
	struct pergroup_context *ctx = PFQ_CB(skb)->monad->persistent;
	spin_unlock(&ctx->persistent[n].lock);
}

#endif /* _PF_Q_MONAD_H_ */
