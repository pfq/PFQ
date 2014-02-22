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


struct sk_function_descr;

extern int pfq_register_functions  (const char *module, struct sk_function_descr *fun);

extern int pfq_unregister_functions(const char *module, struct sk_function_descr *fun);


enum action
{
        action_pass      = 0x00,
        action_drop      = 0x01,
        action_clone     = 0x02,
        action_dispatch  = 0x04,
        action_steal     = 0x08,
        action_skip      = 0x10,
        action_to_kernel = 0x80,
};


typedef struct
{
        unsigned int  hash:24;
        unsigned int  type:8;
        unsigned int  class;
} ret_t;


static inline bool
is_pass(ret_t ret)
{
        return ret.type & action_pass;
}

static inline bool
is_drop(ret_t ret)
{
        return ret.type & action_drop;
}

static inline bool
is_clone(ret_t ret)
{
        return ret.type & action_clone;
}

static inline bool
is_steering(ret_t ret)
{
        return ret.type & action_dispatch;
}

static inline bool
is_stolen(ret_t ret)
{
        return ret.type & action_steal;
}

static inline bool
is_skip(ret_t ret)
{
        return ret.type & action_skip;
}


typedef ret_t (*sk_function_t)(struct sk_buff *, ret_t);


struct sk_function_descr
{
        const char *      name;
        sk_function_t     function;
};


struct fun_context
{
        atomic_long_t   function;
        atomic_long_t   context;
        spinlock_t      lock;
};


struct pfq_annotation
{
        unsigned long group_mask;

        unsigned long state;

        struct fun_context * fun_ctx;

        int index;      /* call index */

        char direct_skb;

        bool stolen_skb;
        bool send_to_kernel;
};


static inline struct pfq_annotation *
pfq_skb_annotation(struct sk_buff *skb)
{
        return (struct pfq_annotation *)(skb->cb);
}


/* utility functions */

static inline
ret_t pfq_call(sk_function_t fun, struct sk_buff *skb, ret_t ret)
{
        pfq_skb_annotation(skb)->index++;
        return fun ? fun(skb, ret) : ret;
}


static inline
sk_function_t
get_next_function(struct sk_buff *skb)
{
        int index = pfq_skb_annotation(skb)->index;
        return (sk_function_t) atomic_long_read(& pfq_skb_annotation(skb)->fun_ctx[index+1].function);
}


static inline
void * get_unsafe_context(struct sk_buff *skb)
{
        int index = pfq_skb_annotation(skb)->index;
        return (void *)atomic_long_read(&pfq_skb_annotation(skb)->fun_ctx[index].context);
}


static inline
void * get_context(struct sk_buff *skb)
{
        int index = pfq_skb_annotation(skb)->index;
        spin_lock(&pfq_skb_annotation(skb)->fun_ctx[index].lock);
        return (void *)atomic_long_read(&pfq_skb_annotation(skb)->fun_ctx[index].context);
}


static inline
void put_context(struct sk_buff *skb)
{
        int index = pfq_skb_annotation(skb)->index;
        spin_unlock(&pfq_skb_annotation(skb)->fun_ctx[index].lock);
}


static inline
unsigned long get_state(struct sk_buff *skb)
{
        return pfq_skb_annotation(skb)->state;
}


static inline
void set_state(struct sk_buff *skb, unsigned long state)
{
        pfq_skb_annotation(skb)->state = state;
}


/* pass: no action */

static inline
ret_t pass(void)
{
        ret_t ret = { 0, action_pass, 0 };
        return ret;
}

/* drop: ignore the packet for the current group */

static inline
ret_t drop(void)
{
        ret_t ret = { 0, action_drop, 0 };
        return ret;
}

/* broadcast: for this group, broadcast the skb to sockets of the given classes */

static inline
ret_t broadcast(unsigned int cl)
{
        ret_t ret = { 0, action_clone, cl };
        return ret;
}


/* steering skb: for this group, dispatch the skb across sockets of the given classes (by means of hash) */

static inline
ret_t steering(unsigned int cl, unsigned int hash)
{
        ret_t ret = { hash ^ (hash >> 8), action_dispatch, cl };
        return ret;
}


/* stolen packet: the skb is stolen by the steering function. (i.e. forwarded) */

static inline
ret_t stolen(void)
{
        ret_t ret = { 0, action_steal, 0 };
        return ret;
}


/*** modifiers ***/


/* to_kernel: set the skb to be passed to kernel */


static inline
ret_t to_kernel(ret_t ret)
{
        if (unlikely(ret.type & action_steal))
        {
                if (printk_ratelimit())
                        pr_devel("[PFQ] to_kernel modifier applied to a stolen packet!\n");
                return ret;
        }
        ret.type |= action_to_kernel;
        return ret;
}


static inline
ret_t clear_skip(ret_t ret)
{
        ret.type &= ~action_skip;
        return ret;
}


static inline
ret_t skip(ret_t ret)
{
        ret.type |= action_skip;
        return ret;
}


#endif /* _PF_Q_FUN_H_ */
