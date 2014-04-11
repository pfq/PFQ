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


static void *
context_get(void **ctxptr, size_t size)
{
        size_t *s = *(size_t **)ctxptr;

        *ctxptr = (char *)(s+1) + ALIGN(size, 8);

        if (*s != size || size == 0)
                return NULL;

        return s+1;
}


static void *
pod_user(void **ctxptr, void const __user *arg, size_t size)
{
        void *ret;

        if (arg == NULL)
                return NULL;

        ret = context_get(ctxptr, size);
        if (ret == NULL)
                return NULL;

        if (copy_from_user(ret, arg, size)) {
                pr_devel("[PFQ] pod_user error!\n");
                return NULL;
        }

        return ret;
}


void pr_devel_functional_descr(struct pfq_functional_descr const *descr, int index)
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


void pr_devel_computation_descr(struct pfq_computation_descr const *descr)
{
        int n;
        pr_devel("computation size:%zu entry_point:%zu\n", descr->size, descr->entry_point);
        for(n = 0; n < descr->size; n++)
        {
                pr_devel_functional_descr(&descr->fun[n], n);
        }
}


char *
strdup_user(const char __user *str)
{
        size_t len = strlen_user(str);
        char *ret;

        if (len == 0)
                return NULL;
        ret = (char *)kmalloc(len, GFP_KERNEL);
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
        functional_t *fun = prg->entry_point;

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
        if (r == NULL) {
                pr_devel("[PFQ] context_alloc: could not allocate %zu bytes!\n", size);
                return NULL;
        }
        s = (size_t *)r;

        for(n = 0; n < descr->size; n++)
        {
                *s = descr->fun[n].arg_size;
                s = (size_t *)((char *)(s+1) + ALIGN(descr->fun[n].arg_size, 8));
        }

        return r;
}


static inline
bool is_monadic_function(enum pfq_functional_type type)
{
        return type == pfq_monadic_fun ||
               type == pfq_high_order_fun;
}


static inline
int validate_function_type(enum pfq_functional_type type)
{
        if (type != pfq_monadic_fun &&
            type != pfq_high_order_fun &&
            type != pfq_predicate_fun &&
            type != pfq_combinator_fun) {
                pr_devel("[PFQ] computation: unknown function type!\n");
                return -EPERM;
        }

        return 0;
}


static int
validate_computation_descr(struct pfq_computation_descr const *descr)
{
        /* entry point */

        size_t ep, n;

        ep = descr->entry_point;

        if (ep > descr->size) {
                pr_devel("[PFQ] computation: invalid entry_point!\n");
                return -EPERM;
        }

        if (descr->fun[ep].type != pfq_monadic_fun &&
            descr->fun[ep].type != pfq_high_order_fun) {
                pr_devel("[PFQ] computation: invalid entry!\n");
                return -EPERM;
        }

        for(n = 0; n < descr->size; n++)
        {
                if (validate_function_type(descr->fun[n].type))
                        return -EPERM;

                if (descr->fun[n].symbol == NULL) {
                        printk(KERN_INFO "[PFQ] computation: NULL symbol!\n");
                        return -EPERM;
                }

                if ((size_t)descr->fun[n].arg_ptr == descr->fun[n].arg_size) {
                        pr_devel("[PFQ] computation: argument ptr/size mismatch!\n");
                        return -EPERM;
                }

                /* out-of-range l_index and r_index mark the function as leaf */
        }

        return 0;
}


int
pfq_computation_compile (struct pfq_computation_descr const *descr, computation_t *comp, void *context)
{
        size_t n = 0;

        /* validate the computation descriptors */

        if (validate_computation_descr(descr) < 0) {
                return -EPERM;
        }

        /* size */

        comp->size = descr->size;

        /* entry point */

        comp->entry_point = &comp->fun[descr->entry_point];

        /* functional_t */

        for(; n < descr->size; n++)
        {
                switch(descr->fun[n].type)
                {
                        case pfq_monadic_fun: {

                                function_ptr_t ptr;
                                char * symbol;
                                void * arg = NULL;

                                if (descr->fun[n].arg_size) {

                                        arg = pod_user(&context, descr->fun[n].arg_ptr, descr->fun[n].arg_size);

                                        if (arg == NULL) {
                                                pr_devel("[PFQ] computation: internal error!\n");
                                                return -EFAULT;
                                        }
                                }

                                symbol = strdup_user(descr->fun[n].symbol);
                                if (symbol == NULL) {
                                        pr_devel("[PFQ] computation: strdup!\n");
                                        return -EFAULT;
                                }

                                ptr = pfq_symtable_resolve(&pfq_monadic_cat, symbol);
                                if (ptr == NULL) {
                                        printk(KERN_INFO "[PFQ] computation: '%s' no such function!\n", symbol);
                                        kfree(symbol);
                                        return -EFAULT;
                                }

                                kfree(symbol);

                                comp->fun[n].fun = make_function(ptr, arg);

                                if (descr->fun[n].r_index < descr->size) {
                                        size_t r = descr->fun[n].r_index;
                                        if (!is_monadic_function(descr->fun[r].type)) {
                                                pr_devel("[PFQ] computation: right path link to non monadic function!\n");
                                                return -EFAULT;
                                        }

                                        comp->fun[n].right = &comp->fun[r];
                                }
                                else {
                                        comp->fun[n].right = NULL;
                                }

                                if (descr->fun[n].l_index < descr->size) {
                                        size_t l = descr->fun[n].l_index;
                                        if (!is_monadic_function(descr->fun[l].type)) {
                                                pr_devel("[PFQ] computation: left path link to non monadic function!\n");
                                                return -EFAULT;
                                        }

                                        comp->fun[n].left  = &comp->fun[l];
                                }
                                else {
                                        comp->fun[n].left  = NULL;
                                }

                        } break;


                        // case pfq_high_order_fun: {

                        //         context_get(&context, 0);

                        //         comp->fun[n].fun = make_high_order_function(NULL, NULL);

                        // } break;


                        // case pfq_predicate_fun: {

                        //         void * arg = context_get(&context, descr->fun[n].arg_size);
                        //         if (descr->fun[n].arg_size != 0 && arg == NULL)
                        //                 return -1;

                        //         comp->fun[n].expr.pred = make_predicate(NULL, arg);

                        // } break;


                        // case pfq_combinator_fun: {

                        //         context_get(&context, 0);

                        //         comp->fun[n].expr.comb = make_combinator(NULL, NULL, NULL);

                        // } break;

                        default: {

                                pr_debug("[PFQ] computation_compile: invalid function!\n");
                                return -EPERM;
                        }
                }
        }

        return 0;
}

