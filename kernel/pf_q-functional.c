/***************************************************************
 *
 * (C) 2014 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#include <linux/kernel.h>
#include <linux/printk.h>

#include <linux/pf_q.h>
#include <linux/pf_q-module.h>

#include <asm/uaccess.h>

#include <pf_q-group.h>
#include <pf_q-functional.h>
#include <pf_q-symtable.h>


void pfq_functional_pr_devel(struct pfq_functional_descr const *descr, int index)
{
        static char *fun_type[] = { "fun", "hfun", "hfun2", "pred", "comb" };

        char *name = strdup_user(descr->symbol);

        pr_devel("%d: %s { %s arg:%zu l_idx:%d r_idx:%d }\n"
                        , index
                        , fun_type[descr->type % 5]
                        , name
                        , descr->arg_size
                        , descr->l_index
                        , descr->r_index);

        kfree(name);
}


void pfq_computation_pr_devel(struct pfq_computation_descr const *descr)
{
        int n;
        pr_devel("computation size:%zu entry_point:%zu\n", descr->size, descr->entry_point);
        for(n = 0; n < descr->size; n++)
        {
                pfq_functional_pr_devel(&descr->fun[n], n);
        }
}


char *
strdup_user(const char __user *str)
{
        size_t len = strlen_user(str);
        char * ret = (char *)kmalloc(len, GFP_KERNEL);
        if (!ret)
                return NULL;
        if (copy_from_user(ret, str, len)) {
                kfree(ret);
                return NULL;
        }
        return ret;
}


static inline struct sk_buff *
pfq_apply(functional_t *call, struct sk_buff *skb)
{
        PFQ_CB(skb)->right = true;
        return call->fun.eval(skb, call->fun.arg);
}


static inline struct sk_buff *
pfq_bind(struct sk_buff *skb, computation_t *prg)
{
        functional_t *fun = &prg->fun[prg->entry_point];

        while (fun)
        {
                action_t *a;

                skb = pfq_apply(fun, skb);
                if (skb == NULL)
                        return NULL;

                a = &PFQ_CB(skb)->action;

                if (is_drop(*a) || has_stop(*a))
                        return skb;

                fun = PFQ_CB(skb)->right ? fun->right : fun->left;
        }

        return skb;
}


struct sk_buff *
pfq_run(int gid, computation_t *prg, struct sk_buff *skb)
{
        struct pfq_group * g = pfq_get_group(gid);
        struct pfq_cb *cb = PFQ_CB(skb);

        if (g == NULL)
                return NULL;

        cb->ctx = &g->ctx;

        cb->action.class_mask = Q_CLASS_DEFAULT;
        cb->action.type       = action_copy;
        cb->action.attr       = 0;

        return pfq_bind(skb, prg);
}


computation_t *
pfq_computation_alloc (struct pfq_computation_descr const *descr)
{
        computation_t * c = kmalloc(sizeof(size_t) + descr->size * sizeof(functional_t), GFP_KERNEL);
        c->size = descr->size;
        return c;
}


void *
pfq_context_alloc(struct pfq_computation_descr const *descr)
{
        size_t size = 0, n = 0, *s;
        void *r;

        for(; n < descr->size; n++)
        {
                size += sizeof(size_t) + ALIGN(descr->fun[n].arg_size, 8);
        }

        r = kmalloc(size, GFP_KERNEL);
        if (r == NULL)
                return NULL;

        s = (size_t *)r;

        for(n = 0; n < descr->size; n++)
        {
                *s = descr->fun[n].arg_size;
                s = (size_t *)((char *)(s+1) + ALIGN(descr->fun[n].arg_size, 8));
        }

        return r;
}


static void *
pfq_context_get(void **ctxptr, size_t size)
{
        size_t *s = *(size_t **)ctxptr;

        *ctxptr = (char *)(s+1) + ALIGN(size, 8);

        if (*s != size || size == 0)
                return NULL;

        return s+1;
}


int
pfq_computation_compile (struct pfq_computation_descr const *descr, computation_t *comp, void *context)
{
        size_t n = 0;

        comp->size = descr->size;

        for(; n < descr->size; n++)
        {
                switch(descr->fun[n].type)
                {
                        case pfq_monadic_fun: {

                                void * arg = pfq_context_get(&context, descr->fun[n].arg_size);
                                if (descr->fun[n].arg_size != 0 && arg == NULL)
                                        return -1;

                                comp->fun[n].fun = make_function(NULL, arg);

                        } break;


                        case pfq_high_order_fun: {

                                pfq_context_get(&context, 0);

                                comp->fun[n].fun = make_high_order_function(NULL, NULL);

                        } break;


                        case pfq_predicate_fun: {

                                void * arg = pfq_context_get(&context, descr->fun[n].arg_size);
                                if (descr->fun[n].arg_size != 0 && arg == NULL)
                                        return -1;

                                comp->fun[n].expr.pred = make_predicate(NULL, arg);

                        } break;


                        case pfq_combinator_fun: {

                                pfq_context_get(&context, 0);

                                comp->fun[n].expr.comb = make_combinator(NULL, NULL, NULL);

                        } break;
                }
        }

        return 0;
}

