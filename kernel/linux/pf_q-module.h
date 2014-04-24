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

#include <linux/kernel.h>
#include <linux/version.h>

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

extern struct list_head pfq_monadic_cat;
extern struct list_head pfq_predicate_cat;

extern int pfq_symtable_register_functions  (const char *module, struct list_head *category, struct pfq_function_descr *fun);
extern int pfq_symtable_unregister_functions(const char *module, struct list_head *category, struct pfq_function_descr *fun);


/**** argument_t: note, the size of the arg is @ (size_t *)addr - 1; ****/

typedef union
{
        void const              *arg;
        struct _expression      *expr;

} argument_t;


#define argument_as(type, a)   __builtin_choose_expr(__builtin_types_compatible_p(argument_t, typeof(a)), (type *)a.arg,  (void)0)
#define expression(a)          __builtin_choose_expr(__builtin_types_compatible_p(argument_t, typeof(a)), a.expr, (void)0)


/**** expression_t: polymorphic expression *****/

typedef struct _expression
{
        bool (*ptr)(struct sk_buff const *skb, struct _expression *this);

} expression_t;

typedef bool (*expression_ptr_t)(struct sk_buff const *skb, struct _expression *);


/**** functional engine ****/

typedef struct sk_buff *(*function_ptr_t)(argument_t, struct sk_buff *);
typedef bool (*predicate_ptr_t)(argument_t, struct sk_buff const *);
typedef bool (*combinator_ptr_t)(expression_t *expr1, expression_t *expr2, struct sk_buff const *);


/* monadic function */

struct pfq_function_descr
{
        const char *    symbol;
        void * 	ptr;
};

struct pfq_monadic_fun_descr
{
        const char *    symbol;
        function_ptr_t ptr;
};

struct pfq_predicate_fun_descr
{
        const char * symbol;
        predicate_ptr_t ptr;
};

struct pfq_combinator_fun_descr
{
        const char * symbol;
        combinator_ptr_t ptr;
};


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
        attr_stolen        = 0x1,
        attr_ret_to_kernel = 0x2
};

/* action */

typedef struct
{
        unsigned long class_mask;
        uint32_t hash;
        uint8_t  type;
        uint8_t  attr;
	bool right;

} action_t;

struct pfq_pergroup_context
{
        sparse_counter_t counter[Q_MAX_COUNTERS];
};

struct pfq_cb
{
        action_t action;

        uint8_t  direct_skb;

        unsigned long group_mask;
        unsigned long state;

        struct pfq_pergroup_context *ctx;

} __attribute__((packed));

#define PFQ_CB(skb) ((struct pfq_cb *)(skb)->cb)

/* class predicates */

static inline bool
is_drop(action_t a)
{
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(3,9,0))
        BUILD_BUG_ON_MSG(sizeof(struct pfq_cb) > sizeof(((struct sk_buff *)0)->cb), "pfq control buffer overflow");
#endif
        return a.type == action_drop;
}

static inline bool
is_copy(action_t a)
{
        return a.type == action_copy;
}

static inline bool
is_steering(action_t a)
{
        return a.type == action_steer;
}

/* packet predicates */

static inline bool
is_stolen(struct sk_buff *skb)
{
        struct pfq_cb * cb = PFQ_CB(skb);
        return cb->action.attr & attr_stolen;
}

/* action: pass */

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

/* class + steering: for this group, steer the skb across sockets of the given classes (by means of hash) */

static inline
struct sk_buff *
class_steering(struct sk_buff *skb, unsigned long class_mask, uint32_t hash)
{
        action_t * a = & PFQ_CB(skb)->action;
        a->type  = action_steer;
        a->class_mask = class_mask;
        a->hash  = hash;
        return skb;
}


/* steal packet: skb is stolen by the function. (i.e. forwarded) */

static inline
struct sk_buff *
steal(struct sk_buff *skb)
{
        action_t * a = & PFQ_CB(skb)->action;
        if (unlikely(a->attr & attr_ret_to_kernel))
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

/* utility function: state */

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


#endif /* _PF_Q_MODULE_H_ */
