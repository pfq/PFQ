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

#include <asm/uaccess.h>

#include <pf_q-group.h>
#include <pf_q-engine.h>
#include <pf_q-symtable.h>
#include <pf_q-module.h>
#include <pf_q-signature.h>

#include <functional/inline.h>
#include <functional/headers.h>

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


void pr_devel_functional_node(struct pfq_functional_node const *node, size_t index)
{
	char buffer[256];
        size_t n, len = 0;

	len += sprintf(buffer + len, "%zu@%p: %pF [", index, node, node->fun.ptr);

	for(n = 0; n < sizeof(node->fun.arg)/sizeof(node->fun.arg[0]); n++)
	{
		if ((node->fun.arg[n] & 0xffffLLU) == (node->fun.arg[n]))
			len += sprintf(buffer + len, "%lld ",(int64_t)node->fun.arg[n]);
		else
			len += sprintf(buffer + len, "%p ",(void *)node->fun.arg[n]);
	}

	if (node->left || node->right)
		len += sprintf(buffer + len, "] -> l:%p r:%p", node->left, node->right);
	else
		len += sprintf(buffer + len, "]");

	pr_devel("%s\n", buffer);
}


void pr_devel_computation_tree(struct pfq_computation_tree const *tree)
{
        size_t n;
        pr_devel("[PFQ] computation tree size:%zu entry_point:%p\n", tree->size, tree->entry_point);
        for(n = 0; n < tree->size; n++)
        {
                pr_devel_functional_node(&tree->node[n], n);
        }
}


void pr_devel_functional_descr(struct pfq_functional_descr const *descr, size_t index)
{
	char buffer[256];

        char *symbol, *signature;
        size_t n, len = 0;

       	if (descr->symbol == NULL) {
		pr_devel("%zu   NULL :: ???\n", index);
       		return ;
	}

        symbol    = strdup_user(descr->symbol);
	signature = strdup_user(descr->signature);

	len += sprintf(buffer, "%zu   %s :: %s [", index, symbol, signature);

        for(n = 0; n < sizeof(descr->arg)/sizeof(descr->arg[0]); n++)
	{
		if (descr->arg[n].ptr)
		{
			if (descr->arg[n].size)
				len += sprintf(buffer + len, "ptr(%zu) ",  descr->arg[n].size);
			else  {
				char * tmp = strdup_user(descr->arg[n].ptr);
				len += sprintf(buffer + len, "'%s' ", tmp);
				kfree(tmp);
			}
		}
		else
		{
			if (descr->arg[n].size)
				len += sprintf(buffer + len, "link:%zu ",  descr->arg[n].size);
		}

	}

	if (descr->left != -1 && descr->right != -1)
		pr_devel("%s] tree:(%zu, %zu)\n", buffer, descr->left, descr->right);
	else
		pr_devel("%s] tree:(-,-)\n", buffer);

        kfree(symbol);
        kfree(signature);
}


void pr_devel_computation_descr(struct pfq_computation_descr const *descr)
{
        size_t n;
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

	return EVAL_FUNCTION(fun, skb);
}


static inline struct sk_buff *
pfq_bind(struct sk_buff *skb, struct pfq_computation_tree *prg)
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
pfq_run(int gid, struct pfq_computation_tree *prg, struct sk_buff *skb)
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

	if ((nrun++ % 1048576) == 0)
		printk(KERN_INFO "[PFQ] run: %llu\n", total/nrun);

	return skb;
#endif

}


struct pfq_computation_tree *
pfq_computation_alloc (struct pfq_computation_descr const *descr)
{
        struct pfq_computation_tree * c = kmalloc(sizeof(size_t) + descr->size * sizeof(struct pfq_functional_node), GFP_KERNEL);
        c->size = descr->size;
        return c;
}


void *
pfq_context_alloc(struct pfq_computation_descr const *descr)
{
        size_t size = 0, n = 0, *ptr;
        void *r;

        for(; n < descr->size; n++)
        {
        	struct pfq_functional_descr const * fun = &descr->fun[n];

        	int i;
        	for(i = 0; i < sizeof(fun->arg)/sizeof(fun->arg[0]); i++)
		{
			if (fun->arg[i].ptr) {

				size_t s = fun->arg[i].size == 0 ?  strlen_user(fun->arg[i].ptr)+1 :
					   fun->arg[i].size >  8 ?  fun->arg[i].size : 0;

				if (s) {
					size += sizeof(size_t) + ALIGN(s, 8);
				}
			}
		}
        }

        r = kmalloc(size, GFP_KERNEL);

        if (r == NULL) {
                pr_devel("[PFQ] context_alloc: could not allocate %zu bytes!\n", size);
                return NULL;
        }

        pr_devel("[PFQ] context_alloc: %zu bytes allocated.\n", size);

        ptr = (size_t *)r;

        for(n = 0; n < descr->size; n++)
        {
        	struct pfq_functional_descr const * fun = &descr->fun[n];

        	int i;
        	for(i = 0; i < sizeof(descr->fun[i].arg)/sizeof(descr->fun[i].arg[0]); i++)
		{
			if (fun->arg[i].ptr) {

				size_t s = fun->arg[i].size == 0 ?  strlen_user(fun->arg[i].ptr)+1 :
					   fun->arg[i].size >  8 ?  fun->arg[i].size : 0;

				if (s) {
					* ptr = s;
				  	  ptr = (size_t *)((char *)(ptr+1) + ALIGN(s, 8));
				}
			}
		}
        }

        return r;
}


size_t
number_of_arguments(struct pfq_functional_descr const *fun)
{
	size_t n = 0;
	int i;

	for(i = 0; i < sizeof(fun->arg)/sizeof(fun->arg[0]); i++)
	{
		if (fun->arg[i].ptr || fun->arg[i].size)
			n++;
	}

	return n;
}


static const char *
resolve_signature_by_user_symbol(const char __user *symb)
{
	struct symtable_entry *entry;
        const char *symbol;

        symbol = strdup_user(symb);
        if (symbol == NULL) {
                pr_devel("[PFQ] resolve_signature_by_symbol: strdup!\n");
                return NULL;
        }

        entry = pfq_symtable_search(&pfq_lang_functions, symbol);
        if (entry == NULL) {
                printk(KERN_INFO "[PFQ] resolve_signature_by_symbol: '%s' no such function!\n", symbol);
                kfree (symbol);
                return NULL;
        }

        kfree(symbol);
        return entry->signature;
}


static bool
function_signature_match(struct pfq_functional_descr const *fun, string_view_t fullsig, size_t index)
{
	const char *signature = strdup_user(fun->signature);
	string_view_t sig;
	size_t nargs;

	if (!signature) {
		pr_devel("[PFQ] %zu: signature_matches: strdup_user error!\n", index);
		return false;
	}

	nargs = number_of_arguments(fun);

	sig = pfq_signature_bind(make_string_view(signature), nargs);

	if (!pfq_signature_equal(sig, fullsig))
	{
		pr_devel("[PFQ] %zu: invalid function: %s (%zu args bound)!\n", index, signature, nargs);
		kfree (signature);
		return false;
	}

	kfree(signature);
	return true;
}



int
pfq_validate_computation_descr(struct pfq_computation_descr const *descr)
{
        size_t entry_point = descr->entry_point, n;

	if (entry_point >= descr->size) {
		pr_devel("[PFQ] %zu: entry_point: invalid function!\n", entry_point);
		return -EPERM;
	}

	/* check if functions are valid */

	for(n = 0; n < descr->size; n++)
	{
		struct pfq_functional_descr const * fun = &descr->fun[n];
		const char *signature, *real_signature;
		size_t nargs;
               	int i;

		if (fun->symbol == NULL) {
			printk(KERN_INFO "[PFQ] %zu: NULL symbol!\n", n);
			return -EPERM;
		}

		nargs = number_of_arguments(fun);

		/* check for valid signature */

		real_signature = resolve_signature_by_user_symbol(fun->symbol);
		if (!real_signature)
			return -EPERM;

		if (!function_signature_match(fun, pfq_signature_bind(make_string_view(real_signature), nargs), n)) {
			pr_devel("[PFQ] %zu: %s: invalid signature!\n", n, real_signature);
			return -EPERM;
		}

		/* check for valid entry_point */

		if (n == entry_point) {

			if (!function_signature_match(fun, make_string_view("SkBuff -> Action SkBuff"), n)) {
				pr_devel("[PFQ] %zu: %s: invalid signature!\n", n, real_signature);
				return -EPERM;
			}
		}

		/* check for valid function arguments */

        	for(i = 0; i < sizeof(fun->arg)/sizeof(fun->arg[0]); i++)
       		{
			if (fun->arg[i].ptr == 0 && fun->arg[i].size != 0) {

				size_t x = fun->arg[i].size;

				/* function argument */


				string_view_t farg = pfq_signature_arg(make_string_view(signature), i);
				if (x >= descr->size) {
					pr_devel("[PFQ] %zu: %s: invalid argument(%d): -> %zu!\n", n, real_signature, i, x);
					return -EPERM;
				}

				if (!function_signature_match(&descr->fun[x], farg, x)) {
					const char *expected = view_to_string(farg);
					pr_devel("[PFQ] %zu: %s: invalid argument(%d): expected signature: %s!\n", n, fun->signature, i, expected);
					kfree(expected);
					return -EPERM;
				}
			}
		}
	}

	return 0;
}

static void *
resolve_user_symbol(struct list_head *cat, const char __user *symb, const char **signature, init_ptr_t *init, fini_ptr_t *fini)
{
	struct symtable_entry *entry;
        const char *symbol;

        symbol = strdup_user(symb);
        if (symbol == NULL) {
                pr_devel("[PFQ] resove_symbol: strdup!\n");
                return NULL;
        }

        entry = pfq_symtable_search(cat, symbol);
        if (entry == NULL) {
                printk(KERN_INFO "[PFQ] resolve_symbol: '%s' no such function!\n", symbol);
                return NULL;
        }

        *signature = entry->signature;
	*init = entry->init;
	*fini = entry->fini;

        kfree(symbol);
        return entry->function;
}


int
pfq_computation_init(struct pfq_computation_tree *comp)
{
	size_t n;
	for (n = 0; n < comp->size; n++)
	{
		if (comp->node[n].init) {
			if (comp->node[n].init( &comp->node[n].fun ) < 0) {
				printk(KERN_INFO "[PFQ] computation_init: error in function (%zu)!\n", n);
				return -EPERM;
			}
		}
	}
 	return 0;
}

int
pfq_computation_fini(struct pfq_computation_tree *comp)
{
	size_t n;
	for (n = 0; n < comp->size; n++)
	{
		if (comp->node[n].fini) {
			if (comp->node[n].fini( &comp->node[n].fun ) < 0) {
				printk(KERN_INFO "[PFQ] computation_fini: error in function (%zu)!\n", n);
				return -EPERM;
			}
		}
	}
	return 0;
}


static struct pfq_functional_node *
get_functional_by_index(struct pfq_computation_descr const *descr, struct pfq_computation_tree *comp, int index)
{
        if (index >= 0 && index < descr->size) {
                return &comp->node[index];
        }

	return NULL;
}


int
pfq_computation_rtlink(struct pfq_computation_descr const *descr, struct pfq_computation_tree *comp, void *context)
{
	size_t n;

#if 0
        /* validate the computation descriptors */

        if (pfq_validate_computation_descr(descr) < 0)
                return -EPERM;
#endif

        /* size */

        comp->size = descr->size;

        /* entry point */

        comp->entry_point = &comp->node[descr->entry_point];

	/* link functions */

        for(n = 0; n < descr->size; n++)
        {
        	struct pfq_functional_descr const *fun;
 		const char *signature;
        	init_ptr_t init, fini;
		void *addr;
                int i;

                fun = &descr->fun[n];

		addr = resolve_user_symbol(&pfq_lang_functions, fun->symbol, &signature, &init, &fini);
		if (addr == NULL) {
        		printk(KERN_INFO "[PFQ] %zu: rtlink: bad descriptor!\n", n);
        		return -EPERM;
		}

		comp->node[n].fun.ptr = addr;
        	comp->node[n].init    = init;
        	comp->node[n].fini    = fini;

		comp->node[n].right = get_functional_by_index(descr, comp, descr->fun[n].right);
		comp->node[n].left  = get_functional_by_index(descr, comp, descr->fun[n].left);

		comp->node[n].fun.arg[0] = 0;
		comp->node[n].fun.arg[1] = 0;
		comp->node[n].fun.arg[2] = 0;
		comp->node[n].fun.arg[3] = 0;

        	for(i = 0; i < sizeof(fun->arg)/sizeof(fun->arg[0]); i++)
		{
			if (fun->arg[i].ptr) { /* data */

				if (fun->arg[i].size == 0) { /* string */

					char *str = pod_user(&context, fun->arg[i].ptr, strlen_user(fun->arg[i].ptr)+1);
					if (str == NULL) {
        					pr_devel("[PFQ] %zu: fun internal error!\n", n);
						return -EPERM;
					}

					comp->node[n].fun.arg[i] = (ptrdiff_t)str;

				}
				else if (fun->arg[i].size > 8) { /* big pod */

					char *pod = pod_user(&context, fun->arg[i].ptr, fun->arg[i].size);
					if (pod == NULL) {
        					pr_devel("[PFQ] %zu: fun internal error!\n", n);
						return -EPERM;
					}

					comp->node[n].fun.arg[i] = (ptrdiff_t)pod;

				}
				else { /* pod */

					ptrdiff_t arg = 0;

        				if (copy_from_user(&arg, fun->arg[i].ptr, fun->arg[i].size)) {
						pr_devel("[PFQ] %zu: fun internal error!\n", n);
						return -EPERM;
					}

        				comp->node[n].fun.arg[i] = arg;
				}
			}
			else if (fun->arg[i].size) {  /* function */

				comp->node[n].fun.arg[i] = (ptrdiff_t)get_functional_by_index(descr, comp, fun->arg[i].size);
			}
		}
	}

	return 0;
}


