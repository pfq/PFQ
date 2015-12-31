/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PF_Q_GROUP_H
#define PF_Q_GROUP_H

#include <pragma/diagnostic_push>
#include <linux/kernel.h>
#include <linux/delay.h>
#include <linux/pf_q.h>
#include <linux/filter.h>
#include <linux/spinlock.h>
#include <linux/semaphore.h>
#include <pragma/diagnostic_pop>

#include <pf_q-sock.h>
#include <pf_q-define.h>
#include <pf_q-sparse.h>
#include <pf_q-stats.h>
#include <pf_q-bpf.h>
#include <pf_q-types.h>



struct pfq_group
{
        int policy;                                     /* group policy */
        int pid;	                                /* process id */

	pfq_id_t owner;					/* owner's pfq id */

        atomic_long_t sock_mask[Q_CLASS_MAX];           /* for class: Q_CLASS_DEFAULT, Q_CLASS_USER_PLANE, Q_CLASS_CONTROL_PLANE etc... */

        atomic_long_t bp_filter;			/* struct sk_filter pointer */

        bool   vlan_filt;                               /* enable/disable vlan filtering */
        char   vid_filters[4096];                       /* vlan filters */

        atomic_long_t comp;                             /* struct pfq_lang_computation_tree *  (new functional program) */
        atomic_long_t comp_ctx;                         /* void *: storage context (new functional program) */

	struct pfq_group_stats __percpu *stats;
	struct pfq_group_counters __percpu *counters;
};


extern struct semaphore group_sem;

struct pfq_lang_computation_tree;

extern int  pfq_join_free_group(pfq_id_t id, unsigned long class_mask, int policy);
extern int  pfq_join_group(pfq_gid_t gid, pfq_id_t id, unsigned long class_mask, int policy);
extern int  pfq_leave_group(pfq_gid_t gid, pfq_id_t id);
extern int  pfq_set_group_prog(pfq_gid_t gid, struct pfq_lang_computation_tree *prog, void *ctx);
extern void pfq_leave_all_groups(pfq_id_t id);

extern unsigned long pfq_get_groups(pfq_id_t id);
extern unsigned long pfq_get_all_groups_mask(pfq_gid_t gid);

extern int  pfq_get_group_context(pfq_gid_t gid, int level, int size, void __user *context);
extern void pfq_set_group_filter(pfq_gid_t gid, struct sk_filter *filter);

extern struct pfq_group * pfq_get_group(pfq_gid_t gid);

extern bool pfq_vlan_filters_enabled(pfq_gid_t gid);
extern bool pfq_check_group_vlan_filter(pfq_gid_t gid, int vid);
extern bool pfq_toggle_group_vlan_filters(pfq_gid_t gid, bool value);
extern void pfq_set_group_vlan_filter(pfq_gid_t gid, bool value, int vid);

extern bool pfq_group_policy_access(pfq_gid_t gid, pfq_id_t id, int policy);
extern bool pfq_group_access(pfq_gid_t gid, pfq_id_t id);

extern int pfq_groups_init(void);
extern void pfq_groups_destruct(void);

static inline
bool pfq_has_joined_group(pfq_gid_t gid, pfq_id_t id)
{
        return pfq_get_all_groups_mask(gid) & (1L << (__force int)id);
}

static inline
bool pfq_group_is_free(pfq_gid_t gid)
{
	struct pfq_group * g;

	g = pfq_get_group(gid);
	if (g == NULL)
		return true;
	if (g->owner != (__force pfq_id_t)-1 || g->pid != 0)
		return false;
	return true;
}

#endif /* PF_Q_GROUP_H */
