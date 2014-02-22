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

#ifndef _PF_Q_GROUP_H_
#define _PF_Q_GROUP_H_

#include <linux/kernel.h>
#include <linux/delay.h>
#include <linux/pf_q.h>
#include <linux/filter.h>
#include <linux/spinlock.h>

#include <linux/pf_q-fun.h>

#include <pf_q-common.h>
#include <pf_q-sparse-counter.h>
#include <pf_q-functional.h>
#include <pf_q-bpf.h>


struct pfq_group
{
        int policy;                                     /* policy for the group */
        int pid;	                                /* process id for restricted/private group */

        atomic_long_t sock_mask[Q_CLASS_MAX];           /* for class: Q_CLASS_DATA, Q_CLASS_CONTROL, etc... */

        struct fun_context fun_ctx[Q_FUN_MAX+1];        /* sk_function_t, void *context pair */

        atomic_long_t filter; 				/* struct sk_filter pointer */

        bool   vlan_filt;                               /* enable/disable vlan filtering */
        char   vid_filters[4096];                       /* vlan filters */

        sparse_counter_t recv;
        sparse_counter_t lost;
        sparse_counter_t drop;
};


extern struct pfq_group pfq_groups[Q_MAX_GROUP];

int  pfq_join_free_group(int id, unsigned long class_mask, int policy);
int  pfq_join_group(int gid, int id, unsigned long class_mask, int policy);
int  pfq_leave_group(int gid, int id);
void pfq_leave_all_groups(int id);

unsigned long pfq_get_groups(int id);
unsigned long __pfq_get_all_groups_mask(int gid);

bool __pfq_group_access(int gid, int id, int policy, bool join);
int  __pfq_set_group_function(int gid, sk_function_t fun, int level);
int  __pfq_set_group_context(int gid, void *context, int level);
int  __pfq_get_group_context(int gid, int level, int size, void __user *context);
void __pfq_reset_group_functx(int gid);
void __pfq_set_group_filter(int gid, struct sk_filter *filter);
void __pfq_dismiss_function(sk_function_t f);


static inline
bool __pfq_vlan_filters_enabled(int gid)
{
        return pfq_groups[gid].vlan_filt;
}


static inline
bool __pfq_check_group_vlan_filter(int gid, int vid)
{
        return pfq_groups[gid].vid_filters[vid & 4095];
}


static inline
void __pfq_toggle_group_vlan_filters(int gid, bool value)
{
        if (value)
                memset(pfq_groups[gid].vid_filters, 0, 4096);

        smp_wmb();
        pfq_groups[gid].vlan_filt = value;
}


static inline
void __pfq_set_group_vlan_filter(int gid, bool value, int vid)
{
        pfq_groups[gid].vid_filters[vid & 4095] = value;
}


static inline
bool __pfq_group_is_empty(int gid)
{
        return __pfq_get_all_groups_mask(gid) == 0;
}


static inline
bool __pfq_has_joined_group(int gid, int id)
{
        return __pfq_get_all_groups_mask(gid) & (1L << id);
}

#endif /* _PF_Q_GROUP_H_ */
