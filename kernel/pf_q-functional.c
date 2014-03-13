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

#include <asm/uaccess.h>

#include <linux/pf_q.h>
#include <linux/pf_q-fun.h>

#include <pf_q-functional.h>
#include <pf_q-factory.h>


struct sk_buff *
pfq_run(struct pfq_exec_prog *prg, struct sk_buff *skb)
{
        action_t * a = & PFQ_CB(skb)->action;
        int n = 0;

        a->step  = 1;
        a->class = Q_CLASS_DEFAULT;
        a->type  = action_continue;
        a->attr  = 0;

        for(;n >= 0 && n < prg->size;)
        {
                skb = pfq_bind(skb, &prg->fun[n]);
                if (skb == NULL)
                        return NULL;

                a = &PFQ_CB(skb)->action;

                if (a->type == action_drop || a->attr & attr_break)
                        return skb;

                n += a->step;
        }

        return skb;
}


size_t
pfq_full_context_size(const struct pfq_meta_prog *prog)
{
        size_t size = 0, n = 0;

        for(; n < prog->size; n++)
        {
                size += ALIGN(prog->fun[n].context.size, 8);
        }

        return size;
}


int
pfq_meta_prog_compile(const struct pfq_meta_prog *prog, struct pfq_exec_prog **exec, void **ctx)
{
        size_t mem = sizeof(int) + sizeof(pfq_exec_t) * prog->size;
        size_t cs  = pfq_full_context_size(prog);
        char * ptr;
        int n;

        *exec = (struct pfq_exec_prog *)kzalloc(mem, GFP_KERNEL);

        if (!*exec) {
                printk(KERN_INFO "[PFQ] meta_prog_compile: no memory (%zu bytes)\n", mem);
                return -ENOMEM;
        }

        *ctx = NULL;
        if (cs) {
                *ctx  = kmalloc(cs, GFP_KERNEL);
                if (!*ctx) {
                        printk(KERN_INFO "[PFQ] meta_prog_compile: no memory (%zu bytes)\n", mem);
                        return -ENOMEM;
                }
        }

        ptr = *ctx;

        (*exec)->size = prog->size;

        for(n = 0; n < prog->size; n++)
        {
                (*exec)->fun[n].fun_ptr = pfq_get_function(prog->fun[n].name);

                if (prog->fun[n].context.size) {
                        (*exec)->fun[n].ctx_ptr = ptr;
                        memcpy(ptr, prog->fun[n].context.addr, prog->fun[n].context.size);
                        ptr += ALIGN(prog->fun[n].context.size, 8);
                }
                else {
                        (*exec)->fun[n].ctx_ptr = NULL;
                }

                spin_lock_init(&(*exec)->fun[n].ctx_lock);

                if ((*exec)->fun[n].fun_ptr == NULL) {
                        pr_devel("[PFQ function error: '%s' unknown function!\n", prog->fun[n].name);
                        return -EINVAL;
                }
        }

        return 0;
}


struct pfq_meta_prog *
kzalloc_meta_prog(size_t size)
{
        size_t mem = sizeof(int) + sizeof(pfq_fun_t) * size;
        struct pfq_meta_prog *prog = (struct pfq_meta_prog *)kzalloc(mem, GFP_KERNEL);

        if (!prog) {
                printk(KERN_INFO "[PFQ] kmalloc_meta_prog: no memory (%zu bytes)\n", mem);
                return NULL;
        }

        prog->size = size;
        return prog;
}


void
kfree_meta_prog(struct pfq_meta_prog *prog)
{
        int n;
        for(n = 0; n < prog->size; n++)
        {
                kfree(prog->fun[n].name);
                kfree(prog->fun[n].context.addr);
        }
        kfree(prog);
}


char *strcat_user(const char __user *str)
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


int
copy_meta_prog_from_user(struct pfq_meta_prog *to, struct pfq_user_meta_prog *from)
{
        int n;

        to->size = from->size;
        for(n = 0; n < to->size; n++)
        {
                int csize;

                to->fun[n].name = strcat_user(from->fun[n].name);
                if (!to->fun[n].name)
                        return -ENOMEM;

                csize = from->fun[n].context.size;
                if (csize) {
                        to->fun[n].context.size = csize;
                        to->fun[n].context.addr = kmalloc(csize, GFP_KERNEL);
                        if (copy_from_user(to->fun[n].context.addr, from->fun[n].context.addr, csize))
                                return -EFAULT;
                }
        }

        return 0;
}


void
pfq_exec_prog_pr_devel(const struct pfq_exec_prog *prog, const void *ctx)
{
        int n;
        pr_devel("[PFQ] exec program @%p, context@%p:\n", prog, ctx);
        for(n = 0; n < prog->size; n++)
        {
                pr_devel("   %d: f:%p c:%p\n", n, prog->fun[n].fun_ptr, prog->fun[n].ctx_ptr);
        }
}


void
pfq_meta_prog_pr_devel(const struct pfq_meta_prog *prog)
{
        int n;
        pr_devel("[PFQ] meta program @%p:\n", prog);
        for(n = 0; n < prog->size; n++)
        {
                pr_devel("   %d: %s (%p,%d)\n", n, prog->fun[n].name, prog->fun[n].context.addr, prog->fun[n].context.size);
        }
}


