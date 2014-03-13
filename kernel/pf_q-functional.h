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

#ifndef _PF_Q_FUNCTIONAL_H_
#define _PF_Q_FUNCTIONAL_H_

#include <linux/pf_q.h>
#include <linux/pf_q-fun.h>


static inline size_t
pfq_meta_prog_memsize(size_t size)
{
        return sizeof(int) + sizeof(pfq_fun_t) * size;
}


extern struct pfq_meta_prog * kzalloc_meta_prog(size_t size);
void  kfree_meta_prog(struct pfq_meta_prog *prog);

extern int copy_meta_prog_from_user(struct pfq_meta_prog *to, struct pfq_user_meta_prog *from);

extern int pfq_meta_prog_compile(const struct pfq_meta_prog *prog, struct pfq_exec_prog **exec, void **ctx);

extern void  pfq_exec_prog_pr_devel(const struct pfq_exec_prog *prog, const void *ctx);
extern void  pfq_meta_prog_pr_devel(const struct pfq_meta_prog *prog);

#endif /* _PF_Q_FUNCTIONAL_H_ */
