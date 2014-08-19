/***************************************************************
 *
 * (C) 2014 Nicola Bonelli <nicola@pfq.io>
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
#include <pf_q-engine.h>

#include <functional/headers.h>


static inline bool is_arg_data(struct pfq_functional_arg_descr const *arg)
{
	return arg->ptr != 0 && arg->size != 0;
}

static inline bool is_arg_string(struct pfq_functional_arg_descr const *arg)
{
	return arg->ptr != 0 && arg->size == 0;
}

static inline bool is_arg_function(struct pfq_functional_arg_descr const *arg)
{
	return arg->ptr == 0 && arg->size != 0;
}


static const char *
signature_by_user_symbol(const char __user *symb)
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
                kfree (symbol);
                return NULL;
        }

        kfree(symbol);
        return entry->signature;
}

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


static void
pr_devel_functional_node(struct pfq_functional_node const *node, size_t index)
{
	char buffer[256];
        size_t n, len = 0;

	len += sprintf(buffer + len, "%zu@%p: %pF { ", index, node, node->fun.ptr);

	for(n = 0; n < sizeof(node->fun.arg)/sizeof(node->fun.arg[0]); n++)
	{
		if ((node->fun.arg[n].value & 0xffffLLU) == (node->fun.arg[n].value))
			len += sprintf(buffer + len, "%lld[%zu] ",(int64_t)node->fun.arg[n].value, node->fun.arg[n].nelem);
		else
			len += sprintf(buffer + len, "%p[%zu] ",(void *)node->fun.arg[n].value, node->fun.arg[n].nelem);
	}

	if (node->next)
		len += sprintf(buffer + len, "} -> next:%p", node->next);
	else
		len += sprintf(buffer + len, "}");

	pr_devel("%s\n", buffer);
}


void
pr_devel_computation_tree(struct pfq_computation_tree const *tree)
{
        size_t n;
        pr_devel("[PFQ] computation tree size:%zu entry_point:%p\n", tree->size, tree->entry_point);
        for(n = 0; n < tree->size; n++)
        {
                pr_devel_functional_node(&tree->node[n], n);
        }
}


static void
pr_devel_functional_descr(struct pfq_functional_descr const *descr, size_t index)
{
	char buffer[256];

        const char *symbol, *signature;
        size_t n, len = 0;

       	if (descr->symbol == NULL) {
		pr_devel("%zu   NULL :: ???\n", index);
       		return ;
	}

        symbol    = strdup_user(descr->symbol);
	signature = signature_by_user_symbol(descr->symbol);

	len += sprintf(buffer, "%zu   %s :: %s - [", index, symbol, signature);

        for(n = 0; n < sizeof(descr->arg)/sizeof(descr->arg[0]); n++)
	{
		if (descr->arg[n].ptr)
		{
			if (descr->arg[n].size) {

				if (descr->arg[n].nelem > 1)
					len += sprintf(buffer + len, "pod_%zu[%zu] ",  descr->arg[n].size, descr->arg[n].nelem);
				else
					len += sprintf(buffer + len, "pod_%zu ",  descr->arg[n].size);
			}
			else  { /* string */
				char * tmp = strdup_user(descr->arg[n].ptr);
				len += sprintf(buffer + len, "'%s' ", tmp);
				kfree(tmp);
			}
		}
		else
		{
			if (descr->arg[n].size)
				len += sprintf(buffer + len, "fun(%zu) ",  descr->arg[n].size);
		}

	}

	if (descr->next != -1)
		pr_devel("%s] next(%zu)\n", buffer, descr->next);
	else
		pr_devel("%s]\n", buffer);

        kfree(symbol);
}


void
pr_devel_computation_descr(struct pfq_computation_descr const *descr)
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


static inline Action_SkBuff
pfq_apply(struct pfq_functional *call, SkBuff b)
{
	function_t fun = { call };

	return EVAL_FUNCTION(fun, b);
}


static inline Action_SkBuff
pfq_bind(SkBuff b, struct pfq_computation_tree *prg)
{
        struct pfq_functional_node *node = prg->entry_point;

        while (node)
        {
                fanout_t *a;

                b = pfq_apply(&node->fun, b).value;
                if (b.skb == NULL)
                        return Pass(b);

                a = &PFQ_CB(b.skb)->monad->fanout;

                if (is_drop(*a))
                        return Pass(b);

                node = node->next;
        }

        return Pass(b);
}

Action_SkBuff
pfq_run(struct pfq_computation_tree *prg, SkBuff b)
{
#ifdef PFQ_LANG_PROFILE
	static uint64_t nrun, total;
	uint64_t stop, start;
#endif

#ifdef PFQ_LANG_PROFILE
	start = get_cycles();

	b =
#else
	return
#endif

	pfq_bind(b, prg);

#ifdef PFQ_LANG_PROFILE

	stop = get_cycles();
	total += (stop-start);

	if ((nrun++ % 1048576) == 0)
		printk(KERN_INFO "[PFQ] run: %llu\n", total/nrun);

	return b;
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
					   fun->arg[i].size  > 8 || fun->arg[i].nelem > 1 ?
					   	fun->arg[i].size * fun->arg[i].nelem : 0;

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

				size_t s = fun->arg[i].size == 0  ?  strlen_user(fun->arg[i].ptr)+1 :
					   fun->arg[i].size >  8  || fun->arg[i].nelem > 1  ?
					   	fun->arg[i].size * fun->arg[i].nelem : 0;

				if (s) {
					* ptr = s;
				  	  ptr = (size_t *)((char *)(ptr+1) + ALIGN(s, 8));
				}
			}
		}
        }

        return r;
}


static size_t
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



static bool
function_signature_match(struct pfq_functional_descr const *fun, string_view_t fullsig, size_t index)
{
	const char *signature = signature_by_user_symbol(fun->symbol);
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
		return false;
	}

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
		const char *signature;
		size_t nargs;
               	int i;

		if (fun->symbol == NULL) {
			printk(KERN_INFO "[PFQ] %zu: NULL symbol!\n", n);
			return -EPERM;
		}

		nargs = number_of_arguments(fun);

		/* get the signature */

		signature = signature_by_user_symbol(fun->symbol);
		if (!signature) {
                	printk(KERN_INFO "[PFQ] resolve_signature_by_symbol: '%s' no such function!\n", fun->symbol);
			return -EPERM;
		}

		/* check for valid signature/entry_point */

		if (n == entry_point || fun->next != -1 ) {  /* next != -1 means monadic function! */

			if (!function_signature_match(fun, make_string_view("SkBuff -> Action SkBuff"), n)) {
				pr_devel("[PFQ] %zu: %s: invalid signature!\n", n, signature);
				return -EPERM;
			}
		}

		/* check for valid function arguments */

        	for(i = 0; i < sizeof(fun->arg)/sizeof(fun->arg[0]); i++)
       		{
       			/* function */

			if (is_arg_function(&fun->arg[i])) {

				/* function argument */

				size_t x = fun->arg[i].size;

				string_view_t farg = pfq_signature_arg(make_string_view(signature), i);

				if (x >= descr->size) {
					pr_devel("[PFQ] %zu: %s: invalid argument(%d): -> %zu!\n", n, signature, i, x);
					return -EPERM;
				}

				if (!function_signature_match(&descr->fun[x], farg, x)) {
					const char *expected = view_to_string(farg);
					pr_devel("[PFQ] %zu: %s: invalid argument(%d): expected signature: %s!\n", n, signature, i, expected);
					kfree(expected);
					return -EPERM;
				}

			}

			/* nelem */

			if (fun->arg[i].nelem > 65536) {
				pr_devel("[PFQ] %zu: invalid argument (%d): number of array elements is %zu!\n", n, i, fun->arg[i].nelem);
				return -EPERM;
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

			comp->node[n].initialized = true;
		}
	}
 	return 0;
}

int
pfq_computation_fini(struct pfq_computation_tree *comp)
{
	size_t n;

	for (n = comp->size - 1; n < comp->size; n--)
	{
		if (comp->node[n].fini && comp->node[n].initialized) {


			if (comp->node[n].fini( &comp->node[n].fun ) < 0) {
				printk(KERN_INFO "[PFQ] computation_fini: error in function (%zu)!\n", n);
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

	/* validate the computation descriptors */

        if (pfq_validate_computation_descr(descr) < 0)
                return -EPERM;

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
		comp->node[n].next    = get_functional_by_index(descr, comp, descr->fun[n].next);

		comp->node[n].fun.arg[0].value = 0;
		comp->node[n].fun.arg[1].value = 0;
		comp->node[n].fun.arg[2].value = 0;
		comp->node[n].fun.arg[3].value = 0;

		comp->node[n].fun.arg[0].nelem = 0;
		comp->node[n].fun.arg[1].nelem = 0;
		comp->node[n].fun.arg[2].nelem = 0;
		comp->node[n].fun.arg[3].nelem = 0;

        	for(i = 0; i < sizeof(fun->arg)/sizeof(fun->arg[0]); i++)
		{
			if (is_arg_string(&fun->arg[i]))
			{
				char *str = pod_user(&context, fun->arg[i].ptr, strlen_user(fun->arg[i].ptr)+1);
				if (str == NULL) {
					pr_devel("[PFQ] %zu: fun internal error!\n", n);
					return -EPERM;
				}

				comp->node[n].fun.arg[i].value = (ptrdiff_t)str;
				comp->node[n].fun.arg[i].nelem = 0;
			}
			else if (is_arg_data(&fun->arg[i]))
			{
				if (fun->arg[i].size > 8 || fun->arg[i].nelem > 1) {

					char *pod = pod_user(&context, fun->arg[i].ptr, fun->arg[i].size * fun->arg[i].nelem);
					if (pod == NULL) {
						pr_devel("[PFQ] %zu: fun internal error!\n", n);
						return -EPERM;
					}

					comp->node[n].fun.arg[i].value = (ptrdiff_t)pod;
					comp->node[n].fun.arg[i].nelem = fun->arg[i].nelem;
				}
				else {
					ptrdiff_t arg = 0;

					if (copy_from_user(&arg, fun->arg[i].ptr, fun->arg[i].size)) {
						pr_devel("[PFQ] %zu: fun internal error!\n", n);
						return -EPERM;
					}

					comp->node[n].fun.arg[i].value = arg;
					comp->node[n].fun.arg[i].nelem = 0;
				}
			}
			else if (is_arg_function(&fun->arg[i]))
                        {
				comp->node[n].fun.arg[i].value = (ptrdiff_t)get_functional_by_index(descr, comp, fun->arg[i].size);
				comp->node[n].fun.arg[i].nelem = 0;
			}
		}
	}

	return 0;
}


