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

/* actions types */

enum action
{
        action_drop  = 0,
        action_copy  = 1,
        action_steer = 2
};

/* action attributes */

enum action_attr
{
        attr_stolen      = 0x1,
        attr_to_kernel 	 = 0x2
};


#define Q_PERSISTENT_MEM 	64

struct pergroup_context
{
        sparse_counter_t counter[Q_MAX_COUNTERS];

	struct _persistent {

		spinlock_t 	lock;
		char 		memory[Q_PERSISTENT_MEM];

	} persistent [Q_MAX_PERSISTENT];
};


/* action: copy */

static inline
struct sk_buff *
copy(struct sk_buff *skb)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->type = action_copy;
        return skb;
}

/* drop: ignore this packet for the current group */

static inline
struct sk_buff *
drop(struct sk_buff *skb)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->type = action_drop;
        return skb;
}

/* class skb: specifies only the class for the packet */

static inline
struct sk_buff *
class(struct sk_buff *skb, uint64_t class_mask)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->class_mask = class_mask;
        return skb;
}

/* broadcast: broadcast the skb all the classes */

static inline
struct sk_buff *
broadcast(struct sk_buff *skb)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->type  = action_copy;
        a->class_mask = Q_CLASS_ANY;
        return skb;
}

/* steering skb: for this group, steer the skb across sockets (by means of hash) */

static inline
struct sk_buff *
steering(struct sk_buff *skb, uint32_t hash)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->type  = action_steer;
        a->hash  = hash;
        return skb;
}

/* deliver: for this group, deliver the skb to the sockets of the given classes */

static inline
struct sk_buff *
deliver(struct sk_buff *skb, unsigned long class_mask)
{
        action_t * a  = & PFQ_CB(skb)->action;
        a->type       = action_copy;
        a->class_mask = class_mask;
        return skb;
}

/* class + steering: for this group, steer the skb across sockets of the given classes (by means of hash) */

static inline
struct sk_buff *
dispatch(struct sk_buff *skb, unsigned long class_mask, uint32_t hash)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->type  = action_steer;
        a->class_mask = class_mask;
        a->hash  = hash;
        return skb;
}


/* steal packet: skb is stolen */

static inline
struct sk_buff *
steal(struct sk_buff *skb)
{
        action_t * a = & PFQ_CB(skb)->action;
        if (unlikely(a->attr & attr_to_kernel))
        {
                if (printk_ratelimit())
                        pr_devel("[PFQ] steal modifier applied to a packet returning to kernel!\n");
                return skb;
        }
        a->attr |= attr_stolen;
        return skb;
}

/* to_kernel: set the skb to be passed to kernel */

static inline
struct sk_buff *
to_kernel(struct sk_buff *skb)
{
        action_t * a = & PFQ_CB(skb)->action;
        if (unlikely(a->attr & attr_stolen))
        {
                if (printk_ratelimit())
                        pr_devel("[PFQ] to_kernel modifier applied to a stolen packet!\n");
                return skb;
        }
        a->attr |= attr_to_kernel;
        return skb;
}

/* utility function: counter */

static inline
sparse_counter_t * get_counter(struct sk_buff *skb, int n)
{
        struct pfq_cb *cb = PFQ_CB(skb);
        if (n < 0 || n >= Q_MAX_COUNTERS)
                return NULL;

        return & cb->ctx->counter[n];
}


/* utility function: volatile state, persistent state */


static inline
unsigned long get_state(struct sk_buff const *skb)
{
        return PFQ_CB(skb)->state;
}

static inline
void set_state(struct sk_buff *skb, unsigned long state)
{
        PFQ_CB(skb)->state = state;
}

static inline void *
__get_persistent(struct sk_buff const *skb, int n)
{
	struct pergroup_context *ctx = PFQ_CB(skb)->ctx;
	spin_lock(&ctx->persistent[n].lock);
	return ctx->persistent[n].memory;
}

#define get_persistent(type, skb, n) __builtin_choose_expr(sizeof(type) <= 64, __get_persistent(skb, n) , (void)0)

static inline
void put_persistent(struct sk_buff const *skb, int n)
{
	struct pergroup_context *ctx = PFQ_CB(skb)->ctx;
	spin_unlock(&ctx->persistent[n].lock);
}

#endif /* _PF_Q_MONAD_H_ */
