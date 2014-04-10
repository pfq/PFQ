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
#include <linux/pf_q-module.h>

#include <pf_q-engine.h>

extern computation_t * pfq_computation_alloc(struct pfq_computation_descr const *);

extern void * pfq_context_alloc(struct pfq_computation_descr const *);

extern int pfq_computation_compile (struct pfq_computation_descr const *descr, computation_t *comp, void *context);

extern void pfq_functional_pr_devel(struct pfq_functional_descr const *, int);

extern void pfq_computation_pr_devel(struct pfq_computation_descr const *);

extern char * strdup_user(const char __user *str);

extern struct sk_buff *pfq_run(int gid, computation_t *prg, struct sk_buff *skb);


#endif /* _PF_Q_FUNCTIONAL_H_ */
