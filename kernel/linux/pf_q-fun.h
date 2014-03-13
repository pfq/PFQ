/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#ifndef _PF_Q_FUN_H_
#define _PF_Q_FUN_H_

#include <linux/pf_q.h>

#include <linux/skbuff.h>
#include <linux/ip.h>
#include <linux/icmp.h>
#include <linux/ipv6.h>
#include <linux/if_ether.h>
#include <linux/if_vlan.h>


struct pfq_function_descr;

extern int pfq_register_functions  (const char *module, struct pfq_function_descr *fun);
extern int pfq_unregister_functions(const char *module, struct pfq_function_descr *fun);


/* exec function */

typedef struct pfq_exec
{
        void *  fun_ptr;        /* pfq_function_t */
        void *  ctx_ptr;
        spinlock_t ctx_lock;

} pfq_exec_t;

struct pfq_exec_prog
{
        int size;
        pfq_exec_t fun[];
};

/* function context */

typedef struct { pfq_exec_t *data; } context_t;

/* monadic function */

typedef struct sk_buff *(*pfq_function_t)(context_t, struct sk_buff *);


struct pfq_function_descr
{
        const char *    name;
        pfq_function_t  function;
};

/* actions types */

enum action
{
        action_continue  = 0x00,
        action_drop      = 0x01,
        action_clone     = 0x02,
        action_dispatch  = 0x04,
        action_steal     = 0x08,
        action_to_kernel = 0x80,
};

typedef struct
{
        unsigned int  hash;
        unsigned int  class:16;
        unsigned int  step:8;
        unsigned int  type:8;

} action_t;


struct pfq_cb
{
        unsigned long group_mask;
        unsigned long state;

        action_t      action;

        char direct_skb;

        bool stolen_skb;
        bool send_to_kernel;
};

#define PFQ_CB(skb) ((struct pfq_cb *)(skb)->cb)


static inline bool
is_continue(action_t a)
{
        return a.type & action_continue;
}

static inline bool
is_drop(action_t a)
{
        return a.type & action_drop;
}

static inline bool
is_clone(action_t a)
{
        return a.type & action_clone;
}

static inline bool
is_steering(action_t a)
{
        return a.type & action_dispatch;
}

static inline bool
is_stolen(action_t a)
{
        return a.type & action_steal;
}

/* action: pass */

static inline
struct sk_buff *
cont(struct sk_buff *skb)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->type = action_continue;
        a->step = 1;
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

/* broadcast: for this group, broadcast the skb to sockets of the given classes */


static inline
struct sk_buff *
broadcast(struct sk_buff *skb, unsigned int class)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->type  = action_clone;
        a->class = class;
        return skb;
}

/* steering skb: for this group, dispatch the skb across sockets of the given classes (by means of hash) */

static inline
struct sk_buff *
steering(struct sk_buff *skb, unsigned int class, unsigned int hash)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->type  = action_dispatch;
        a->class = class;
        a->hash  = hash;
        return skb;
}

/* stolen packet: the skb is stolen by the function. (i.e. forwarded) */

static inline
struct sk_buff *
steal(struct sk_buff *skb)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->type = action_steal;
        return skb;
}

/*** modifiers ***/

/* to_kernel: set the skb to be passed to kernel */

static inline
struct sk_buff *
to_kernel(struct sk_buff *skb)
{
        action_t * a = & PFQ_CB(skb)->action;
        if (unlikely(a->type & action_steal))
        {
                if (printk_ratelimit())
                        pr_devel("[PFQ] to_kernel modifier applied to a stolen packet!\n");
                return skb;
        }
        a->type |= action_to_kernel;
        return skb;
}


static inline
struct sk_buff *
jump(struct sk_buff *skb, int step)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->type = action_continue;
        a->step = step;
        return skb;
}

/* utility functions */

static inline
const void * immutable_context(context_t ctx)
{
        return ctx.data->ctx_ptr;
}

static inline
void * unsafe_context(context_t ctx)
{
        return ctx.data->ctx_ptr;
}

static inline
void * get_context(context_t ctx)
{
        spin_lock(&ctx.data->ctx_lock);
        return ctx.data->ctx_ptr;
}


static inline
void put_context(context_t ctx)
{
        spin_unlock(&ctx.data->ctx_lock);
}


static inline
unsigned long get_state(struct sk_buff *skb)
{
        return PFQ_CB(skb)->state;
}


static inline
void set_state(struct sk_buff *skb, unsigned long state)
{
        PFQ_CB(skb)->state = state;
}




#endif /* _PF_Q_FUN_H_ */
