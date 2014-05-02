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
#include <linux/pf_q-functional.h>

#include <asm/uaccess.h>

#include <pf_q-group.h>
#include <pf_q-engine.h>
#include <pf_q-symtable.h>

#include <functional/inline.h>
#include <functional/combinator.h>
#include <functional/predicate.h>


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
	const char *fun_name[] =
	{
		[pfq_monadic_fun] 	= "fun",
		[pfq_high_order_fun] 	= "hfun",
		[pfq_predicate_fun] 	= "pred",
		[pfq_combinator_fun] 	= "comb",
		[pfq_property_fun] 	= "prop"
	};

        char *name;

       	if (descr->symbol == NULL)
       		return ;

        name = strdup_user(descr->symbol);

	if (descr->arg_ptr)
	{
		pr_devel("%d  %s:%s aptr:%p asize:%zu fun:%d left:%d right:%d\n"
				, index
				, fun_name[descr->type % (sizeof(fun_name)/sizeof(fun_name[0]))]
				, name
				, descr->arg_ptr
				, descr->arg_size
				, descr->fun
				, descr->left
				, descr->right);
	}
	else
	{
		pr_devel("%d  %s:%s fun:%d left:%d right:%d\n"
				, index
				, fun_name[descr->type % (sizeof(fun_name)/sizeof(fun_name[0]))]
				, name
				, descr->fun
				, descr->left
				, descr->right);
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
pfq_apply(struct pfq_functional *call, struct sk_buff *skb)
{
	function_t fun = { call };
        PFQ_CB(skb)->action.right = true;

#ifdef PFQ_USE_INLINE_FUN
	IF_INLINED_RETURN(call, skb);
#endif
	return eval_function(fun, skb);
}


static inline struct sk_buff *
pfq_bind(struct sk_buff *skb, computation_t *prg)
{
        struct pfq_functional_node *node = prg->entry_point;

        while (node)
        {
                action_t *a;

                skb = pfq_apply(&node->fun, skb);
                if (skb == NULL)
                        return NULL;

                a = &PFQ_CB(skb)->action;

                if (is_drop(*a))
                        return skb;

                node = PFQ_CB(skb)->action.right ? node->right : node->left;
        }

        return skb;
}


struct sk_buff *
pfq_run(int gid, computation_t *prg, struct sk_buff *skb)
{
        struct pfq_group * g = pfq_get_group(gid);
        struct pfq_cb *cb = PFQ_CB(skb);

#ifdef PFQ_LANG_PROFILE
	static uint64_t nrun, total;
	uint64_t stop, start;
#endif
        if (g == NULL)
                return NULL;

        cb->ctx = &g->ctx;

        cb->action.class_mask = Q_CLASS_DEFAULT;
        cb->action.type       = action_copy;
        cb->action.attr       = 0;

#ifdef PFQ_LANG_PROFILE
	start = get_cycles();

	skb =
#else
	return
#endif

	pfq_bind(skb, prg);

#ifdef PFQ_LANG_PROFILE

	stop = get_cycles();
	total += (stop-start);

	if ((nrun++ % 16) == 0)
		printk(KERN_INFO "[PFQ] run: %llu\n", total/nrun);

	return skb;
#endif

}


computation_t *
pfq_computation_alloc (struct pfq_computation_descr const *descr)
{
        computation_t * c = kmalloc(sizeof(size_t) + descr->size * sizeof(struct pfq_functional_node), GFP_KERNEL);
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
        	if (descr->fun[n].arg_ptr && (descr->fun[n].arg_size > 8))
                	size += sizeof(size_t) + ALIGN(descr->fun[n].arg_size, 8);
        }

        r = kmalloc(size, GFP_KERNEL);
        if (r == NULL) {
                pr_devel("[PFQ] context_alloc: could not allocate %zu bytes!\n", size);
                return NULL;
        }

        pr_devel("[PFQ] context_alloc: %zu bytes allocated.\n", size);

        s = (size_t *)r;

        for(n = 0; n < descr->size; n++)
        {
        	if (descr->fun[n].arg_ptr && (descr->fun[n].arg_size > 8)) {
                	*s = descr->fun[n].arg_size;
                	s = (size_t *)((char *)(s+1) + ALIGN(descr->fun[n].arg_size, 8));
		}
        }

        return r;
}

/*** function checks... ***/

static
bool check_pure_function(struct pfq_computation_descr const *descr, int index)
{
        if (index >= descr->size) {
                pr_devel("[PFQ] %d: pure_function check: invalid entry_point!\n", index);
                return false;
        }

	if (descr->fun[index].type == pfq_predicate_fun  ||
	    descr->fun[index].type == pfq_combinator_fun ||
	    descr->fun[index].type == pfq_property_fun)
	    	return true;

	return false;
}

static
bool check_monadic_function(struct pfq_computation_descr const *descr, int index)
{
        if (index >= descr->size) {
                pr_devel("[PFQ] %d: monadic_function check: invalid entry_point!\n", index);
                return false;
        }

	if (descr->fun[index].type == pfq_monadic_fun ||
	    descr->fun[index].type == pfq_high_order_fun)
	    	return true;

	return false;
}

static
bool check_predicate_function(struct pfq_computation_descr const *descr, int index)
{
        if (index >= descr->size) {
                pr_devel("[PFQ] %d: predicate_function check: invalid entry_point!\n", index);
                return false;
        }

	if (descr->fun[index].type == pfq_predicate_fun ||
	    descr->fun[index].type == pfq_combinator_fun)
	    	return true;

	return false;
}

static
bool check_argument(struct pfq_computation_descr const *descr, int index)
{
        if (index >= descr->size) {
                pr_devel("[PFQ] %d: argument check: invalid entry_point!\n", index);
                return false;
        }

	if ((descr->fun[index].arg_ptr == NULL) != (descr->fun[index].arg_size == 0)) {
		pr_devel("[PFQ] %d: argument ptr/size mismatch!\n", index);
		return false;
	}

	return true;
}


static int
validate_computation_descr(struct pfq_computation_descr const *descr)
{
        /* entry point */

        int n;

	if (!check_monadic_function(descr, descr->entry_point)) {
                pr_devel("[PFQ] %d: computation: invalid entry_point!\n", (int)descr->entry_point);
                return -EPERM;
        }

        for(n = 0; n < descr->size; n++)
        {
                if (descr->fun[n].symbol == NULL) {
                        printk(KERN_INFO "[PFQ] %d: NULL symbol!\n", n);
                        return -EPERM;
                }

                switch(descr->fun[n].type)
                {
                case pfq_monadic_fun: {

			if (!check_argument(descr, n)) {
                                pr_devel("[PFQ] %d: monadic function error!\n", n);
				return -EPERM;
			}

                } break;

                case pfq_high_order_fun: {

                        int pindex = descr->fun[n].fun;

			if (!check_predicate_function(descr, pindex)) {
                                pr_devel("[PFQ] %d: high-order function error!\n", n);
				return -EPERM;
			}

                } break;

                case pfq_predicate_fun: {

                        int pindex = descr->fun[n].fun;

			if (!check_argument(descr, n)) {
                                pr_devel("[PFQ] %d: predicate function error!\n", n);
				return -EPERM;
			}

                        if (pindex == -1)
                        	return 0;

			if (!check_pure_function(descr, pindex)) {
                                pr_devel("[PFQ] %d: predicate function error!\n", n);
                                return -EPERM;
			}

                } break;

                case pfq_combinator_fun: {

                        int left  = descr->fun[n].left;
                        int right = descr->fun[n].right;

			if (!check_predicate_function(descr, left)) {
                                pr_devel("[PFQ] %d: combinator function error!\n", n);
                                return -EPERM;
			}

			if (!check_predicate_function(descr, right)) {
                                pr_devel("[PFQ] %d: combinator function error!\n", n);
                                return -EPERM;
			}
                }break;

                case pfq_property_fun: {

                        int pindex = descr->fun[n].fun;

			if (!check_argument(descr, n)) {
                                pr_devel("[PFQ] %d: property function error!\n", n);
				return -EPERM;
			}

                        if (pindex == -1)
                        	return 0;

			if (!check_pure_function(descr, pindex)) {
                                pr_devel("[PFQ] %d: property function error!\n", n);
                                return -EPERM;
			}

		}; break;

                default:
                        pr_devel("[PFQ] %d: unsupported function type!\n", n);
                        return -EPERM;
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
get_functional_by_index(struct pfq_computation_descr const *descr, computation_t *comp, int index, struct pfq_functional_node **ret)
{
        if (index >= 0 && index < descr->size) {
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


        for(n = 0; n < descr->size; n++)
        {
                switch(descr->fun[n].type)
                {
                case pfq_monadic_fun: {

                        uint64_t properties;
                        function_ptr_t ptr;

                        ptr = resolve_user_symbol(&pfq_monadic_cat, descr->fun[n].symbol, &properties);
                        if (ptr == NULL ||
                		(properties & FUN_ACTION) == 0 ||
                                ( (properties & FUN_ARG_DATA) && descr->fun[n].arg_ptr == NULL) ||
                                (!(properties & FUN_ARG_DATA) && descr->fun[n].arg_ptr != NULL)
                        		) {
                                printk(KERN_INFO "[PFQ] %zu: nad descriptor!\n", n);
                                return -EPERM;
                        }

                        if (descr->fun[n].arg_size > 8) {

				void *arg  = pod_user(&context, descr->fun[n].arg_ptr, descr->fun[n].arg_size);
                                if (arg == NULL) {
                                        pr_devel("[PFQ] %zu: fun internal error!\n", n);
                                        return -EPERM;
                                }

                        	comp->fun[n].fun = make_function(ptr, arg);
                        }
			else {
				ptrdiff_t arg;

        			if (copy_from_user(&arg, descr->fun[n].arg_ptr, descr->fun[n].arg_size)) {
                                        pr_devel("[PFQ] %zu: fun internal error!\n", n);
                			return -EPERM;
        			}

                        	comp->fun[n].fun = make_function(ptr, arg);
			}


			if (get_functional_by_index(descr, comp, descr->fun[n].right, &comp->fun[n].right) < 0) {

                                pr_devel("[PFQ] %zu: right path out of range!\n", n);
                                return -EINVAL;
			}

                        if (get_functional_by_index(descr, comp, descr->fun[n].left, &comp->fun[n].left) < 0) {

                                pr_devel("[PFQ] %zu: left path out of range!\n", n);
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
                                (properties & FUN_ARG_FUN) == 0 )
			{
                                printk(KERN_INFO "[PFQ] %zu: bad descriptor!\n", n);
                                return -EPERM;
                        }

                        comp->fun[n].fun = make_high_order_function(ptr, &comp->fun[pindex].fun);

			if (get_functional_by_index(descr, comp, descr->fun[n].right, &comp->fun[n].right) < 0) {

                                pr_devel("[PFQ] %zu: right path out of range!\n", n);
                                return -EINVAL;
			}

                        if (get_functional_by_index(descr, comp, descr->fun[n].left, &comp->fun[n].left) < 0) {

                                pr_devel("[PFQ] %zu: left path out of range!\n", n);
                                return -EINVAL;
			}

                } break;


                case pfq_predicate_fun: {

                        uint64_t properties;

                        predicate_ptr_t ptr;


                        ptr = resolve_user_symbol(&pfq_predicate_cat, descr->fun[n].symbol, &properties);
                        if (ptr == NULL ||
                	    	(properties & FUN_PREDICATE) == 0 ||
                                ( (properties & FUN_ARG_DATA) && descr->fun[n].arg_ptr == NULL) ||
                                (!(properties & FUN_ARG_DATA) && descr->fun[n].arg_ptr != NULL))
			{
                                printk(KERN_INFO "[PFQ] %zu: bad descriptor!\n", n);
                                return -EPERM;
                        }

                        if (descr->fun[n].arg_size > 8) {

                        	void * arg =  pod_user(&context, descr->fun[n].arg_ptr, descr->fun[n].arg_size);
                                if (arg == NULL) {
                                        pr_devel("[PFQ] %zu: pred internal error!\n", n);
                                        return -EPERM;
                                }

				if (descr->fun[n].fun != -1) {

					size_t pindex = descr->fun[n].fun;
					comp->fun[n].fun = make_high_order_predicate(ptr, arg, &comp->fun[pindex].fun);
				}
				else {
					comp->fun[n].fun = make_predicate(ptr, arg);
				}


                        } else {

				ptrdiff_t arg;

        			if (copy_from_user(&arg, descr->fun[n].arg_ptr, descr->fun[n].arg_size)) {
                                        pr_devel("[PFQ] %zu: fun internal error!\n", n);
                			return -EPERM;
        			}

				if (descr->fun[n].fun != -1) {

					size_t pindex = descr->fun[n].fun;
					comp->fun[n].fun = make_high_order_predicate(ptr, arg, &comp->fun[pindex].fun);
				}
				else {
					comp->fun[n].fun = make_predicate(ptr, arg);
				}
			}


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

                        comp->fun[n].fun = make_combinator(ptr, &comp->fun[left].fun, &comp->fun[right].fun);

                        comp->fun[n].right = NULL;
                        comp->fun[n].left  = NULL;

                } break;

                case pfq_property_fun: {

                        uint64_t properties;
                        property_ptr_t ptr;



                        ptr = resolve_user_symbol(&pfq_property_cat, descr->fun[n].symbol, &properties);
                        if (ptr == NULL ||
                	    	(properties & FUN_PROPERTY) == 0 ||
                                ( (properties & FUN_ARG_DATA) && descr->fun[n].arg_ptr == NULL) ||
                                (!(properties & FUN_ARG_DATA) && descr->fun[n].arg_ptr != NULL))
			{
                                printk(KERN_INFO "[PFQ] %zu: bad descriptor!\n", n);
                                return -EPERM;
                        }

                        if (descr->fun[n].arg_size > 8) {

                        	void * arg = pod_user(&context, descr->fun[n].arg_ptr, descr->fun[n].arg_size);
                                if (arg == NULL) {
                                        pr_devel("[PFQ] %zu: pred internal error!\n", n);
                                        return -EPERM;
                                }

                        	comp->fun[n].fun = make_property(ptr, arg);
                        } else {

				ptrdiff_t arg;

        			if (copy_from_user(&arg, descr->fun[n].arg_ptr, descr->fun[n].arg_size)) {
                                        pr_devel("[PFQ] %zu: fun internal error!\n", n);
                			return -EPERM;
        			}

                        	comp->fun[n].fun = make_property(ptr, arg);
			}


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

