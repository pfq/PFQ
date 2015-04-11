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

#ifndef PF_Q_GROUP_H
#define PF_Q_GROUP_H

#include <linux/kernel.h>
#include <linux/delay.h>
#include <linux/pf_q.h>
#include <linux/filter.h>
#include <linux/spinlock.h>
#include <linux/semaphore.h>

#include <pf_q-sock.h>
#include <pf_q-macro.h>
#include <pf_q-sparse.h>
#include <pf_q-stats.h>
#include <pf_q-bpf.h>

/* persistent state */

typedef struct
{
	int value;
} pfq_gid_t;


struct pfq_group_persistent
{
        sparse_counter_t counter[Q_MAX_COUNTERS];

	struct _persistent {

		spinlock_t 	lock;
		char 		memory[Q_GROUP_PERSIST_MEM];

	} persistent [Q_GROUP_PERSIST_DATA];
};


struct pfq_group
{
        int policy;                                     /* policy for the group */
        int pid;	                                /* process id for restricted/private group */

	pfq_id_t owner;					/* id of the owner */

        atomic_long_t sock_mask[Q_CLASS_MAX];           /* for class: Q_CLASS_DEFAULT, Q_CLASS_USER_PLANE, Q_CLASS_CONTROL_PLANE etc... */

        atomic_long_t bp_filter; 			/* struct sk_filter pointer */

        bool   vlan_filt;                               /* enable/disable vlan filtering */
        char   vid_filters[4096];                       /* vlan filters */

        atomic_long_t comp;                             /* struct pfq_computation_tree *  (new functional program) */
        atomic_long_t comp_ctx;                         /* void *: storage context (new functional program) */

	struct pfq_group_stats stats;

        struct pfq_group_persistent context;
};


extern struct semaphore group_sem;

struct pfq_computation_tree;

extern int  pfq_join_free_group(pfq_id_t id, unsigned long class_mask, int policy);
extern int  pfq_join_group(pfq_gid_t gid, pfq_id_t id, unsigned long class_mask, int policy);
extern int  pfq_leave_group(pfq_gid_t gid, pfq_id_t id);
extern int  pfq_set_group_prog(pfq_gid_t gid, struct pfq_computation_tree *prog, void *ctx);
extern void pfq_leave_all_groups(pfq_id_t id);

extern int pfq_check_group_id(pfq_gid_t gid, pfq_id_t id, const char *msg);

extern unsigned long pfq_get_groups(pfq_id_t id);
extern unsigned long pfq_get_all_groups_mask(pfq_gid_t gid);

extern int  pfq_get_group_context(pfq_gid_t gid, int level, int size, void __user *context);
extern void pfq_set_group_filter(pfq_gid_t gid, struct sk_filter *filter);

extern void pfq_dismiss_function(void *f);
extern struct pfq_group * pfq_get_group(pfq_gid_t gid);

extern bool pfq_vlan_filters_enabled(pfq_gid_t gid);
extern bool pfq_check_group_vlan_filter(pfq_gid_t gid, int vid);
extern bool pfq_toggle_group_vlan_filters(pfq_gid_t gid, bool value);
extern void pfq_set_group_vlan_filter(pfq_gid_t gid, bool value, int vid);

extern bool pfq_group_policy_access(pfq_gid_t gid, pfq_id_t id, int policy);
extern bool pfq_group_access(pfq_gid_t gid, pfq_id_t id);


static inline
bool pfq_has_joined_group(pfq_gid_t gid, pfq_id_t id)
{
        return pfq_get_all_groups_mask(gid) & (1L << id.value);
}

static inline
bool pfq_group_is_free(pfq_gid_t gid)
{
	struct pfq_group * g;

	g = pfq_get_group(gid);
	if (g == NULL)
		return true;
	if (g->owner.value != -1 || g->pid != 0)
		return false;
	return true;
}

#endif /* PF_Q_GROUP_H */
