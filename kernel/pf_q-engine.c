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

#include <pragma/diagnostic_push>

#include <linux/kernel.h>
#include <linux/printk.h>
#include <linux/pf_q.h>
#include <asm/uaccess.h>

#include <pragma/diagnostic_pop>

#include <pf_q-group.h>
#include <pf_q-engine.h>
#include <pf_q-module.h>
#include <pf_q-symtable.h>
#include <pf_q-signature.h>
#include <pf_q-engine.h>

#include <functional/headers.h>


const char *
pfq_signature_by_user_symbol(const char __user *symb)
{
	struct symtable_entry *entry;
        const char *symbol;

        symbol = strdup_user(symb);
        if (symbol == NULL) {
                pr_devel("[PFQ] pfq_signature_by_user_symbol: strdup!\n");
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
        void *ret;

	if (size == 0)
		return NULL;

        ret = *ptr;

        *ptr += ALIGN(size, 8);

        return ret;
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
pfq_apply(struct pfq_functional *call, SkBuff skb)
{
	function_t fun = { call };
	return EVAL_FUNCTION(fun, skb);
}


static inline Action_SkBuff
pfq_bind(SkBuff skb, struct pfq_computation_tree *prg)
{
        struct pfq_functional_node *node = prg->entry_point;

        while (node)
        {
                fanout_t *a;

                skb = pfq_apply(&node->fun, skb).value;
                if (skb == NULL)
                        return Pass(skb);

                a = &PFQ_CB(skb)->monad->fanout;

                if (is_drop(*a))
                        return Pass(skb);

                node = node->next;
        }

        return Pass(skb);
}


Action_SkBuff
pfq_run(SkBuff skb, struct pfq_computation_tree *prg)
{
#ifdef PFQ_LANG_PROFILE
	static uint64_t nrun, total;
	uint64_t stop, start;
#endif

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
		printk(KERN_INFO "[PFQ] PFQ/lang run: %llu_tsc.\n", total/nrun);

	return skb;
#endif

}


struct pfq_computation_tree *
pfq_computation_alloc (struct pfq_computation_descr const *descr)
{
        struct pfq_computation_tree * c = kzalloc(sizeof(size_t) + descr->size * sizeof(struct pfq_functional_node),
						  GFP_KERNEL);
        c->size = descr->size;
        return c;
}


void *
pfq_context_alloc(struct pfq_computation_descr const *descr)
{
        size_t size = 0, n = 0;
        void *ret;

        for(; n < descr->size; n++)
        {
		struct pfq_functional_descr const * fun = &descr->fun[n];
		int i;

		for(i = 0; i < sizeof(fun->arg)/sizeof(fun->arg[0]); i++)
		{
			if (fun->arg[i].addr) {

				size_t s = is_arg_string(&fun->arg[i])     ?  strlen_user(fun->arg[i].addr) :
					   is_arg_vector(&fun->arg[i])	   ?  fun->arg[i].size * fun->arg[i].nelem :
					   is_arg_vector_str(&fun->arg[i]) ?  fun->arg[i].nelem * sizeof(char *) + strlen_user(fun->arg[i].addr) :
					   is_arg_data  (&fun->arg[i])	   ?  (fun->arg[i].size > 8 ? fun->arg[i].size : 0 ) : 0;

				size += ALIGN(s, 8);
			}
		}
        }

        ret = kmalloc(size, GFP_KERNEL);
        if (ret == NULL) {
                printk(KERN_INFO "[PFQ] context_alloc: could not allocate %zu bytes!\n", size);
                return NULL;
        }

        pr_devel("[PFQ] context_alloc: %zu bytes allocated.\n", size);
        return ret;
}


size_t
pfq_number_of_arguments(struct pfq_functional_descr const *fun)
{
	size_t n = 0;
	int i;

	for(i = 0; i < sizeof(fun->arg)/sizeof(fun->arg[0]); i++)
	{
		if (fun->arg[i].addr || fun->arg[i].size || fun->arg[i].nelem)
			n++;
	}

	return n;
}



static bool
function_signature_match(struct pfq_functional_descr const *fun, string_view_t fullsig, size_t index)
{
	const char *signature = pfq_signature_by_user_symbol(fun->symbol);
	string_view_t sig;
	size_t nargs;

	if (!signature) {
		pr_devel("[PFQ] %zu: signature_matches: strdup_user error!\n", index);
		return false;
	}

	nargs = pfq_number_of_arguments(fun);

	sig = pfq_signature_bind(make_string_view(signature), nargs);

	if (!pfq_signature_equal(sig, fullsig)) {

		pr_devel("[PFQ] %zu: invalid function: %s (%zu args bound)!\n", index, signature, nargs);
		return false;
	}

	return true;
}


static int
check_argument_descr(struct pfq_functional_arg_descr const *arg, string_view_t expected)
{
	if (is_arg_data(arg)) {
		ptrdiff_t size = pfq_signature_sizeof(expected);
		if (size != -1) {
			if (size != arg->size) {
				pr_devel("[PFQ] invalid argument: expected " SVIEW_FMT ", pod size = %zu (size mismatch)!\n", SVIEW_ARG(expected), arg->size);
				return -EPERM;
			}
		}
		return 0;
	}

	if (is_arg_vector(arg)) {

		string_view_t type;
		ptrdiff_t size;

		if (string_view_at(expected, 0) != '[') {
			pr_devel("[PFQ] invalid argument: expected " SVIEW_FMT ", got a vector!\n", SVIEW_ARG(expected));
			return -EPERM;
		}

		type = pfq_signature_remove_extent(expected);
                if(string_view_empty(type)) {
			pr_devel("[PFQ] invalid argument: expected a non empty vector!\n");
			return -EPERM;
		}

		size = pfq_signature_sizeof(type);
		if (size != -1) {
			if (size != arg->size) {
				pr_devel("[PFQ] invalid argument: expected " SVIEW_FMT ", pod size = %zu (size mismatch)!\n", SVIEW_ARG(type), arg->size);
				return -EPERM;
			}
		}

		return 0;
	}

	if (is_arg_string(arg)) {
		if (string_view_compare(expected, "String") != 0) {
			pr_devel("[PFQ] invalid argument: expected " SVIEW_FMT ", got String!\n", SVIEW_ARG(expected));
			return -EPERM;
		}

		return 0;
	}

	if (is_arg_vector_str(arg)) {
		if (string_view_compare(expected, "[String]") != 0) {
			pr_devel("[PFQ] invalid argument: expected " SVIEW_FMT ", got [String]!\n", SVIEW_ARG(expected));
			return -EPERM;
		}

		return 0;
	}

	return -EPERM;
}


int
pfq_check_computation_descr(struct pfq_computation_descr const *descr)
{
        size_t entry_point = descr->entry_point, n;

	if (entry_point >= descr->size) {
		printk(KERN_INFO "[PFQ] %zu: entry_point: invalid function!\n", entry_point);
		return -EPERM;
	}

	/* check if functions are valid */

        pr_devel("[PFQ] validating computation (%zu functions)\n", descr->size);

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

		nargs = pfq_number_of_arguments(fun);

		/* get the signature */

		signature = pfq_signature_by_user_symbol(fun->symbol);
		if (!signature) {
			printk(KERN_INFO "[PFQ] resolve_signature_by_symbol: '%s' no such function!\n", fun->symbol);
			return -EPERM;
		}

		/* check for valid signature/entry_point */

		if (n == entry_point || fun->next != -1 ) {  /* next != -1 means monadic function! */

			if (!function_signature_match(fun, make_string_view("SkBuff -> Action SkBuff"), n)) {
				printk(KERN_INFO "[PFQ] %zu: %s: invalid signature!\n", n, signature);
				return -EPERM;
			}
		}

		/* check for valid function arguments */

		for(i = 0; i < nargs; i++)
		{
			string_view_t sarg = pfq_signature_arg(make_string_view(signature), i);

			if (fun->arg[i].nelem > 65536 &&
			    fun->arg[i].nelem != -1) {
				printk(KERN_INFO "[PFQ] %zu: invalid argument (%d): number of array elements is %zu!\n",
				       n, i, fun->arg[i].nelem);
				return -EPERM;
			}

			if (is_arg_function(&fun->arg[i])) {
				size_t x = fun->arg[i].size;

				if (x >= descr->size) {
					printk(KERN_INFO "[PFQ] %zu: %s: invalid argument(%d): %zu!\n",
					       n, signature, i, x);
					return -EPERM;
				}

				if (!function_signature_match(&descr->fun[x], sarg, x)) {
					printk(KERN_INFO "[PFQ] %zu: %s: invalid argument(%d): expected signature "
					       SVIEW_FMT "!\n", n, signature, i, SVIEW_ARG(sarg));
					return -EPERM;
				}
				continue;
			}

			if (check_argument_descr(&fun->arg[i], sarg) != 0) {
				printk(KERN_INFO "[PFQ] %zu: invalid argument %d!\n", n, i);
				return -EPERM;
			}
		}
	}

	return 0;
}


static void *
resolve_user_symbol(struct list_head *cat, const char __user *symb, const char **signature,
		    init_ptr_t *init, fini_ptr_t *fini)
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
                pr_devel("[PFQ] resolve_symbol: '%s' no such function!\n", symbol);
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

			pr_devel("[PFQ] %zu: initializing computation %pF...\n", n, comp->node[n].init);

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

			pr_devel("[PFQ] %zu: finalizing computation %pF...\n", n, comp->node[n].fini);

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


/*
 * Prerequisite: valid computation (check by means of pfq_validate_computation_descr)
 */

int
pfq_computation_rtlink(struct pfq_computation_descr const *descr, struct pfq_computation_tree *comp, void *context)
{
	size_t n;

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
                size_t i;

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
			if (is_arg_string(&fun->arg[i])) {

				char *str = pod_user(&context, fun->arg[i].addr, strlen_user(fun->arg[i].addr));
				if (str == NULL) {
					printk(KERN_INFO "[PFQ] %zu: pod_user(1): internal error!\n", n);
					return -EPERM;
				}

				comp->node[n].fun.arg[i].value = (ptrdiff_t)str;
				comp->node[n].fun.arg[i].nelem = -1;
			}
			else if (is_arg_vector_str(&fun->arg[i])) {

				char **base_ptr, **ptr;
				char *str;
				size_t j;

				base_ptr = ptr = (char **)context;

				context += sizeof(char *) * fun->arg[i].nelem;

				str = pod_user(&context, fun->arg[i].addr, strlen_user(fun->arg[i].addr));
				if (str == NULL) {
					printk(KERN_INFO "[PFQ] %zu: pod_user(2): internal error!\n", n);
					return -EPERM;
				}

				for(j = 0; j < fun->arg[i].nelem; j++)
				{
					char *end;
					*(ptr++) = str;
					end = strchr(str, '\x1e');
					if (end == NULL)
						break;
					*end = '\0';
					str = end+1;
				}

				comp->node[n].fun.arg[i].value = (ptrdiff_t)base_ptr;
				comp->node[n].fun.arg[i].nelem = fun->arg[i].nelem;
			}
			else if (is_arg_data(&fun->arg[i])) {

				if (fun->arg[i].size > 8) {

					char *ptr = pod_user(&context, fun->arg[i].addr, fun->arg[i].size);
					if (ptr == NULL) {
						printk(KERN_INFO "[PFQ] %zu: pod_user(3): internal error!\n", n);
						return -EPERM;
					}

					comp->node[n].fun.arg[i].value = (ptrdiff_t)ptr;
					comp->node[n].fun.arg[i].nelem = -1;
				}
				else {
					ptrdiff_t arg = 0;

					if (copy_from_user(&arg, fun->arg[i].addr, fun->arg[i].size)) {
						printk(KERN_INFO "[PFQ] %zu: copy_from_user: internal error!\n", n);
						return -EPERM;
					}

					comp->node[n].fun.arg[i].value = arg;
					comp->node[n].fun.arg[i].nelem = -1;
				}

			}
			else if (is_arg_vector(&fun->arg[i])) {

				if (fun->arg[i].nelem > 0) {

					char *ptr = pod_user(&context, fun->arg[i].addr,
							     fun->arg[i].size * fun->arg[i].nelem);
					if (ptr == NULL) {
						printk(KERN_INFO "[PFQ] %zu: pod_user(4): internal error!\n", n);
						return -EPERM;
					}

					comp->node[n].fun.arg[i].value = (ptrdiff_t)ptr;
					comp->node[n].fun.arg[i].nelem = fun->arg[i].nelem;
				}
				else {  /* empty vector */

					comp->node[n].fun.arg[i].value = 0xdeadbeef;
					comp->node[n].fun.arg[i].nelem = 0;
				}
			}
			else if (is_arg_function(&fun->arg[i])) {

				comp->node[n].fun.arg[i].value = (ptrdiff_t)get_functional_by_index(descr, comp,
												    fun->arg[i].size);
				comp->node[n].fun.arg[i].nelem = -1;
			}
			else if (!is_arg_null(&fun->arg[i])) {

				printk(KERN_INFO "[PFQ] pfq_computation_rtlink: internal error@ function:%zu argument[%zu] => { %p, %zu, %zu }!\n",
				       n, i, (void __user *)fun->arg[i].addr, fun->arg[i].size, fun->arg[i].nelem);
				return -EPERM;
			}
		}
	}

	return 0;
}


