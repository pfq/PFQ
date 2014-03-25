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

#ifndef _PF_Q_MODULE_H_
#define _PF_Q_MODULE_H_

#include <linux/pf_q.h>
#include <linux/pf_q-sparse.h>

#include <linux/skbuff.h>
#include <linux/ip.h>
#include <linux/icmp.h>
#include <linux/ipv6.h>
#include <linux/if_ether.h>
#include <linux/if_vlan.h>

struct pfq_function_descr;
struct pfq_exec;

extern int pfq_register_functions  (const char *module, struct pfq_function_descr *fun);
extern int pfq_unregister_functions(const char *module, struct pfq_function_descr *fun);

/* function context */

typedef struct
{
        const void *data;
        size_t size;

} context_t;

/* monadic function */

typedef struct sk_buff *(*pfq_function_t)(context_t, struct sk_buff *);


struct pfq_function_descr
{
        const char *    name;
        pfq_function_t  function;
};


/* exec function */

typedef struct pfq_exec
{
        pfq_function_t fun_ptr;
        void *  ctx_ptr;
        size_t  ctx_size;

} pfq_exec_t;


struct pfq_exec_prog
{
        size_t size;
        pfq_exec_t fun[];
};


/* actions types */

enum action
{
        action_drop      = 0,
        action_continue  = 1,
        action_clone     = 2,
        action_dispatch  = 3
};

/* action attributes */

enum action_attr
{
        attr_break         = 0x1,
        attr_stolen        = 0x2,
        attr_ret_to_kernel = 0x4
};

/* action */

typedef struct
{
        uint32_t hash;
        uint16_t class;
        uint8_t  type;
        uint8_t  attr;

} action_t;

struct pfq_pergroup_context
{
        sparse_counter_t counter[Q_MAX_COUNTERS];
};

struct pfq_cb
{
        unsigned long group_mask;
        unsigned long state;

        action_t action;
        struct pfq_pergroup_context *ctx;

        char direct_skb;
};

#define PFQ_CB(skb) ((struct pfq_cb *)(skb)->cb)

/* predicates */

static inline bool
is_drop(action_t a)
{
        return a.type == action_drop;
}

static inline bool
is_continue(action_t a)
{
        return a.type == action_continue;
}

static inline bool
is_clone(action_t a)
{
        return a.type == action_clone;
}

static inline bool
is_steering(action_t a)
{
        return a.type == action_dispatch;
}

/* attributes */

static inline bool
has_stolen(action_t a)
{
        return a.attr & attr_stolen;
}

static inline bool
has_break(action_t a)
{
        return a.attr & attr_break;
}

static inline bool
has_ret_to_kernel(action_t a)
{
        return a.attr & attr_ret_to_kernel;
}

/* packet predicates */

static inline bool
is_stolen(struct sk_buff *skb)
{
        struct pfq_cb * cb = PFQ_CB(skb);
        return has_stolen(cb->action);
}

/* action: pass */

static inline
struct sk_buff *
cont(struct sk_buff *skb)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->type = action_continue;
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

/* class skb: for this packet specifies only the class */

static inline
struct sk_buff *
class(struct sk_buff *skb, uint16_t class)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->class = class;
        return skb;
}

/* broadcast: for this group, broadcast the skb to sockets of the given classes */

static inline
struct sk_buff *
broadcast(struct sk_buff *skb, uint16_t class)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->type  = action_clone;
        a->class = class;
        return skb;
}

/* steering skb: for this group, dispatch the skb across sockets (by means of hash) */

static inline
struct sk_buff *
steering(struct sk_buff *skb, uint32_t hash)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->type  = action_dispatch;
        a->hash  = hash;
        return skb;
}


/* class + steering: for this group, dispatch the skb across sockets of the given classes (by means of hash) */

static inline
struct sk_buff *
class_steering(struct sk_buff *skb, uint16_t class, uint32_t hash)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->type  = action_dispatch;
        a->class = class;
        a->hash  = hash;
        return skb;
}


/* steal packet: the skb is stolen by the function. (i.e. forwarded) */

static inline
struct sk_buff *
steal(struct sk_buff *skb)
{
        action_t * a = & PFQ_CB(skb)->action;
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
        a->attr |= attr_ret_to_kernel;
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

/* utility function: context */

static inline
const void * context_addr(context_t ctx)
{
        return ctx.data;
}

static inline
size_t context_size(context_t ctx)
{
        return ctx.size;
}

/* utility function: state */

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


#endif /* _PF_Q_MODULE_H_ */
