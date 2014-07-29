/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola.bonelli@cnit.it>
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


#ifndef _PF_Q_ENGINE_H_
#define _PF_Q_ENGINE_H_

#include <linux/kernel.h>

struct pfq_computation_descr;
struct pfq_computation_tree;

extern int pfq_validate_computation_descr(struct pfq_computation_descr const *descr);

extern int pfq_computation_rtlink(struct pfq_computation_descr const *descr, struct pfq_computation_tree *comp, void *context);
extern int pfq_computation_init(struct pfq_computation_tree *comp);
extern int pfq_computation_fini(struct pfq_computation_tree *comp);

extern struct pfq_computation_tree * pfq_computation_alloc(struct pfq_computation_descr const *);
extern void * pfq_context_alloc(struct pfq_computation_descr const *);
extern char * strdup_user(const char __user *str);

extern struct sk_buff *pfq_run(struct pfq_computation_tree *prg, struct sk_buff *skb);

extern void pr_devel_computation_descr(struct pfq_computation_descr const *);
extern void pr_devel_computation_tree(struct pfq_computation_tree const *tree);

#endif /* _PF_Q_ENGINE_H_ */
