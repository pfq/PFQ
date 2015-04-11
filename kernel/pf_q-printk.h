/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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


#ifndef PF_Q_PRINTK_H
#define PF_Q_PRINTK_H

#include <pf_q-group.h>

struct pfq_computation_descr;
struct pfq_computation_tree;
struct pfq_functional_node;

extern void   pr_devel_group(pfq_gid_t gid);
extern void   pr_devel_buffer(const unsigned char *buff, size_t len);
extern void   pr_devel_computation_descr(struct pfq_computation_descr const *);
extern void   pr_devel_computation_tree(struct pfq_computation_tree const *);
extern size_t snprintf_functional_node(char *buffer, size_t size, struct pfq_functional_node const *node, size_t index);


#endif /* PF_Q_PRINTK_H */
