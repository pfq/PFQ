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

#include <functional/inline.h>

#include <pf_q-group.h>
#include <pf_q-functional.h>
#include <pf_q-symtable.h>


static void *
pod_memory_get(void **ptr, size_t size)
{
        size_t *s;

       	if (size == 0)
       		return NULL;

        s = *(size_t **)ptr;

        if (*s != size) {
                pr_devel("[PFQ] pod_user: memory slot is %zu!\n", *s);
                return NULL;
	}

        *ptr = (char *)(s+1) + ALIGN(size, 8);

        return s+1;
}


static void *
pod_user(void **ptr, void const __user *arg, size_t size)
{
        void *ret;

        if (arg == NULL || size == 0) {
                pr_devel("[PFQ] pod_user: __user ptr/size error!\n");
                return NULL;
	}

        ret = pod_memory_get(ptr, size);
        if (ret == NULL) {
                pr_devel("[PFQ] pod_user: could not get memory (%zu)!\n", size);
                return NULL;
	}

        if (copy_from_user(ret, arg, size)) {
                pr_devel("[PFQ] pod_user error!\n");
                return NULL;
        }

        return ret;
}


void pr_devel_functional_descr(struct pfq_functional_descr const *descr, int index)
{
        char *name;

       	if (descr->symbol == NULL)
       		return ;

        name = strdup_user(descr->symbol);

        switch (descr->type)
        {
        case pfq_monadic_fun:
                pr_devel("%d  fun: %s aptr:%p asize:%zu fun:%d left:%d right:%d\n"
                                , index
                                , name
                                , descr->arg_ptr
                                , descr->arg_size
                                , descr->fun
                                , descr->left
                                , descr->right);
                break;
        case pfq_high_order_fun:
                pr_devel("%d hfun: %s aptr:%p asize:%zu fun:%d left:%d right:%d\n"
                                , index
                                , name
                                , descr->arg_ptr
                                , descr->arg_size
                                , descr->fun
                                , descr->left
                                , descr->right);
                break;
        case pfq_predicate_fun:
                pr_devel("%d pred: %s aptr:%p asize:%zu fun:%d left:%d right:%d\n"
                                , index
                                , name
                                , descr->arg_ptr
                                , descr->arg_size
                                , descr->fun
                                , descr->left
                                , descr->right);
                break;
        case pfq_combinator_fun:
                pr_devel("%d comb: %s aptr:%p asize:%zu fun:%d left:%d right:%d\n"
                                , index
                                , name
                                , descr->arg_ptr
                                , descr->arg_size
                                , descr->fun
                                , descr->left
                                , descr->right);
                break;
        }

        kfree(name);
}


void pr_devel_computation_descr(struct pfq_computation_descr const *descr)
{
        int n;
        pr_devel("[PFQ] computation size:%zu entry_point:%zu\n", descr->size, descr->entry_point);
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
	ptrdiff_t infun = (ptrdiff_t)call->fun.eval;

        PFQ_CB(skb)->action.right = true;

#ifdef PFQ_USE_INLINE_FUN

	IF_INLINED_RETURN(call, skb);

	if ((size_t)infun < 1000) {
		pr_devel("[PFQ] internal error: inline function %td ???\n", infun);
		return skb;
	}
#else
	(void)infun;
#endif

	return call->fun.eval(&call->fun.args, skb);
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

                if (is_drop(*a))
                        return skb;

                fun = PFQ_CB(skb)->action.right ? fun->right : fun->left;
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
        	if (descr->fun[n].arg_ptr && descr->fun[n].arg_size)
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
        	if (descr->fun[n].arg_ptr && descr->fun[n].arg_size) {
                	*s = descr->fun[n].arg_size;
                	s = (size_t *)((char *)(s+1) + ALIGN(descr->fun[n].arg_size, 8));
		}
        }

        return r;
}


static inline
bool is_monadic_function(enum pfq_functional_type type)
{
        return type == pfq_monadic_fun || type == pfq_high_order_fun;
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

        if (ep >= descr->size) {
                pr_devel("[PFQ] computation: invalid entry_point!\n");
                return -EPERM;
        }

        if (descr->fun[ep].type != pfq_monadic_fun &&
                        descr->fun[ep].type != pfq_high_order_fun) {
                pr_devel("[PFQ] %zu: invalid entry_point!\n", ep);
                return -EPERM;
        }

        for(n = 0; n < descr->size; n++)
        {
                if (descr->fun[n].symbol == NULL) {
                        printk(KERN_INFO "[PFQ] %zu: NULL symbol!\n", n);
                        return -EPERM;
                }

                switch(descr->fun[n].type)
                {
                case pfq_monadic_fun: {

                        if ((descr->fun[n].arg_ptr == NULL) != (descr->fun[n].arg_size == 0)) {
                                pr_devel("[PFQ] %zu: argument ptr/size mismatch!\n", n);
                                return -EPERM;
                        }

                } break;

                case pfq_high_order_fun: {

                        size_t pindex = descr->fun[n].fun;

                        if (pindex >= descr->size) {
                                pr_devel("[PFQ] %zu: high-order function: predicate out-of-range!\n", n);
                                return -EPERM;
                        }

                        if (descr->fun[pindex].type != pfq_predicate_fun &&
                            descr->fun[pindex].type != pfq_combinator_fun ) {
                                pr_devel("[PFQ] %zu: high-order function: bad predicate!\n", n);
                                return -EPERM;
                        }

                } break;

                case pfq_predicate_fun: {

                        if ((descr->fun[n].arg_ptr == NULL) != (descr->fun[n].arg_size == 0)) {
                                pr_devel("[PFQ] %zu: argument ptr/size mismatch!\n", n);
                                return -EPERM;
                        }

                } break;

                case pfq_combinator_fun: {

                        size_t left  = descr->fun[n].left;
                        size_t right = descr->fun[n].right;

                        if (left >= descr->size) {
                                pr_devel("[PFQ] %zu: combinator: left predicate out-of-range!\n", n);
                                return -EPERM;
                        }
                        if (right >= descr->size) {
                                pr_devel("[PFQ] %zu: combinator: right predicate out-of-range!\n", n);
                                return -EPERM;
                        }

                        if (descr->fun[left].type != pfq_predicate_fun &&
                            descr->fun[left].type != pfq_combinator_fun ) {
                                pr_devel("[PFQ] %zu: combinator: bad left predicate!\n", n);
                                return -EPERM;
                        }

                        if (descr->fun[right].type != pfq_predicate_fun &&
                            descr->fun[right].type != pfq_combinator_fun ) {
                                pr_devel("[PFQ] %zu: combinator: bad right predicate!\n", n);
                                return -EPERM;
                        }

                }break;

                default: {
                        return -EPERM;
                }
                }
        }

        return 0;
}


static void *
resolve_user_symbol(struct list_head *cat, const char __user *symb, uint64_t *prop)
{
        const char *symbol;
        void *addr;

        symbol = strdup_user(symb);
        if (symbol == NULL) {
                pr_devel("[PFQ] resove_symbol: strdup!\n");
                return NULL;
        }

        addr = pfq_symtable_resolve(cat, symbol);
        if (addr == NULL) {
                printk(KERN_INFO "[PFQ] resolve_symbol: '%s' no such function!\n", symbol);
                return NULL;
        }

        *prop = pfq_symtable_get_properties(cat, symbol);

        kfree(symbol);
        return addr;
}



static int
get_functional_by_index(struct pfq_computation_descr const *descr, computation_t *comp, int index, functional_t **ret)
{
        if (index >= 0 && index < descr->size) {

                if (!is_monadic_function(descr->fun[index].type))
                        return -EINVAL;

                *ret = &comp->fun[index];
        }
        else {
        	*ret = NULL;
	}

	return 0;
}


int
pfq_computation_compile (struct pfq_computation_descr const *descr, computation_t *comp, void *context)
{
        size_t n;

        /* validate the computation descriptors */

        if (validate_computation_descr(descr) < 0)
                return -EPERM;

        /* size */

        comp->size = descr->size;

        /* entry point */

        comp->entry_point = &comp->fun[descr->entry_point];

        /* functional_t */

        for(n = 0; n < descr->size; n++)
        {
                switch(descr->fun[n].type)
                {
                case pfq_monadic_fun: {

                        uint64_t properties;
                        function_ptr_t ptr;
                        void * arg = NULL;

                        if (descr->fun[n].arg_size) {

                                arg = pod_user(&context, descr->fun[n].arg_ptr, descr->fun[n].arg_size);
                                if (arg == NULL) {
                                        pr_devel("[PFQ] %zu: fun internal error!\n", n);
                                        return -EPERM;
                                }
                        }

                        ptr = resolve_user_symbol(&pfq_monadic_cat, descr->fun[n].symbol, &properties);
                        if (ptr == NULL ||
                		(properties & FUN_ACTION) == 0 ||
                                ( (properties & FUN_WITH_ARG) && descr->fun[n].arg_ptr == NULL) ||
                                (!(properties & FUN_WITH_ARG) && descr->fun[n].arg_ptr != NULL)
                        		) {
                                printk(KERN_INFO "[PFQ] %zu: bad descriptor!\n", n);
                                return -EPERM;
                        }

                        comp->fun[n].fun = make_function(ptr, arg);

			if (get_functional_by_index(descr, comp, descr->fun[n].right, &comp->fun[n].right) < 0) {

                                pr_devel("[PFQ] %zu: right path link to pure function!\n", n);
                                return -EINVAL;
			}

                        if (get_functional_by_index(descr, comp, descr->fun[n].left, &comp->fun[n].left) < 0) {

                                pr_devel("[PFQ] %zu: left path link to pure function!\n", n);
                                return -EINVAL;
                        }


                } break;


                case pfq_high_order_fun: {

                        uint64_t properties;
                        function_ptr_t ptr;

                        size_t pindex = descr->fun[n].fun;

                        ptr = resolve_user_symbol(&pfq_monadic_cat, descr->fun[n].symbol, &properties);
                        if (ptr == NULL ||
                	    	(properties & FUN_ACTION) == 0 ||
                                (properties & FUN_WITH_PREDICATE) == 0 )
			{
                                printk(KERN_INFO "[PFQ] %zu: bad descriptor!\n", n);
                                return -EPERM;
                        }

                        comp->fun[n].fun = make_high_order_function(ptr, BOOLEAN_EXPR_CAST(&comp->fun[pindex].expr));

			if (get_functional_by_index(descr, comp, descr->fun[n].right, &comp->fun[n].right) < 0) {

                                pr_devel("[PFQ] %zu: right path link to pure function!\n", n);
                                return -EINVAL;
			}

                        if (get_functional_by_index(descr, comp, descr->fun[n].left, &comp->fun[n].left) < 0) {

                                pr_devel("[PFQ] %zu: left path link to pure function!\n", n);
                                return -EINVAL;
			}

                } break;


                case pfq_predicate_fun: {

                        uint64_t properties;
                        predicate_ptr_t ptr;
                        void * arg = NULL;

                        if (descr->fun[n].arg_size) {

                                arg = pod_user(&context, descr->fun[n].arg_ptr, descr->fun[n].arg_size);
                                if (arg == NULL) {
                                        pr_devel("[PFQ] %zu: pred internal error!\n", n);
                                        return -EPERM;
                                }
                        }

                        ptr = resolve_user_symbol(&pfq_predicate_cat, descr->fun[n].symbol, &properties);
                        if (ptr == NULL ||
                	    	(properties & FUN_PREDICATE) == 0 ||
                                ( (properties & FUN_WITH_ARG) && descr->fun[n].arg_ptr == NULL) ||
                                (!(properties & FUN_WITH_ARG) && descr->fun[n].arg_ptr != NULL))
			{
                                printk(KERN_INFO "[PFQ] %zu: bad descriptor!\n", n);
                                return -EPERM;
                        }

                        comp->fun[n].expr.pred = make_predicate(ptr, arg);

                        comp->fun[n].right = NULL;
                        comp->fun[n].left  = NULL;

                } break;

                case pfq_combinator_fun: {

                        uint64_t properties;
                        combinator_ptr_t ptr;
                        size_t left, right;

                        ptr = resolve_user_symbol(&pfq_predicate_cat, descr->fun[n].symbol, &properties);
                        if (ptr == NULL ||
                	    	(properties & FUN_COMBINATOR) == 0)
			{
                                printk(KERN_INFO "[PFQ] %zu: bad descriptor!\n", n);
                                return -EPERM;
                        }

                        left  = descr->fun[n].left;
                        right = descr->fun[n].right;

                        comp->fun[n].expr.comb = make_combinator(ptr, BOOLEAN_EXPR_CAST(&comp->fun[left].expr),
                                                                      BOOLEAN_EXPR_CAST(&comp->fun[right].expr));

                        comp->fun[n].right = NULL;
                        comp->fun[n].left  = NULL;

                } break;

                default: {
                        pr_debug("[PFQ] computation_compile: invalid function!\n");
                        return -EPERM;
                }
                }
        }

        return 0;
}

