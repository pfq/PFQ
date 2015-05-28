/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
 *             Andrea Di Pietro <andrea.dipietro@for.unipi.it>
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
#include <linux/bug.h>
#include <linux/module.h>
#include <linux/semaphore.h>
#include <linux/sched.h>

#include <pragma/diagnostic_pop>

#include <pf_q-group.h>
#include <pf_q-devmap.h>
#include <pf_q-bitops.h>
#include <pf_q-engine.h>


DEFINE_SEMAPHORE(group_sem);


static struct pfq_group pfq_groups[Q_MAX_GROUP];


void
pfq_groups_init(void)
{
	int n;
	for(n = 0; n < Q_MAX_GROUP; n++)
	{
		pfq_groups[n].pid = 0;
		pfq_groups[n].owner.value = -1;
		pfq_groups[n].policy = Q_POLICY_GROUP_UNDEFINED;
	}
}


static inline
bool __pfq_group_is_empty(pfq_gid_t gid)
{
        return pfq_get_all_groups_mask(gid) == 0;
}


bool
pfq_group_policy_access(pfq_gid_t gid, pfq_id_t id, int policy)
{
        struct pfq_group * g;

        g = pfq_get_group(gid);
        if (g == NULL)
                return false;

        switch(g->policy)
        {
        case Q_POLICY_GROUP_PRIVATE:
                return g->owner.value == id.value; // __pfq_has_joined_group(gid,id);

        case Q_POLICY_GROUP_RESTRICTED:
                return (policy == Q_POLICY_GROUP_RESTRICTED) && g->pid == current->tgid;

        case Q_POLICY_GROUP_SHARED:
                return policy == Q_POLICY_GROUP_SHARED;

        case Q_POLICY_GROUP_UNDEFINED:
                return true;
        }

        return false;
}


bool
pfq_group_access(pfq_gid_t gid, pfq_id_t id)
{
	struct pfq_group *g;

	g = pfq_get_group(gid);
	if (g == NULL)
		return false;

	switch(g->policy)
	{
	case Q_POLICY_GROUP_PRIVATE:
		return g->owner.value == id.value;

        case Q_POLICY_GROUP_RESTRICTED:
                return g->pid == current->tgid;

        case Q_POLICY_GROUP_SHARED:
        case Q_POLICY_GROUP_UNDEFINED:
                return true;
	}

	return false;
}


static void
__pfq_group_init(pfq_gid_t gid)
{
        struct pfq_group * g;
        int i;

	g = pfq_get_group(gid);
        if (g == NULL)
                return;

	g->pid = current->tgid;
        g->owner.value = -1;
        g->policy = Q_POLICY_GROUP_UNDEFINED;

        for(i = 0; i < Q_CLASS_MAX; i++)
        {
                atomic_long_set(&g->sock_mask[i], 0);
        }

        atomic_long_set(&g->bp_filter,0L);
        atomic_long_set(&g->comp,     0L);
        atomic_long_set(&g->comp_ctx, 0L);

	pfq_group_stats_reset(&g->stats);

        for(i = 0; i < Q_MAX_COUNTERS; i++)
        {
                sparse_set(&g->context.counter[i], 0);
        }

	for(i = 0; i < Q_GROUP_PERSIST_DATA; i++)
	{
		spin_lock_init(&g->context.persistent[i].lock);
		memset(g->context.persistent[i].memory, 0, sizeof(g->context.persistent[i].memory));
	}
}


static void
__pfq_group_free(pfq_gid_t gid)
{
        struct pfq_group * g;
        struct sk_filter *filter;
        struct pfq_computation_tree *old_comp;
        void *old_ctx;

	g = pfq_get_group(gid);
        if (g == NULL)
                return;

        /* remove this gid from demux matrix */

        pfq_devmap_update(map_reset, Q_ANY_DEVICE, Q_ANY_QUEUE, gid);

        g->pid = 0;
        g->owner.value = -1;
        g->policy = Q_POLICY_GROUP_UNDEFINED;

        filter   = (struct sk_filter *)atomic_long_xchg(&g->bp_filter, 0L);
        old_comp = (struct pfq_computation_tree *)atomic_long_xchg(&g->comp, 0L);
        old_ctx  = (void *)atomic_long_xchg(&g->comp_ctx, 0L);

        msleep(Q_GRACE_PERIOD);   /* sleeping is possible here: user-context */

	/* call fini on old computation */

	if (old_comp)
		pfq_computation_fini(old_comp);

	kfree(old_comp);
	kfree(old_ctx);

	if (filter)
		pfq_free_sk_filter(filter);

        g->vlan_filt = false;

        pr_devel("[PFQ] group %d destroyed.\n", gid.value);
}


static int
__pfq_join_group(pfq_gid_t gid, pfq_id_t id, unsigned long class_mask, int policy)
{
        struct pfq_group * g;
        unsigned long tmp = 0;
        unsigned long bit;

	g = pfq_get_group(gid);
        if (g == NULL)
                return -EINVAL;

	/* if this group is unused, initializes it */

        if (!g->pid)
                __pfq_group_init(gid);

        if (!pfq_group_policy_access(gid, id, policy)) {
                pr_devel("[PFQ] group gid=%d is not join-able with policy %d\n", gid.value, policy);
                return -EACCES;
        }

        pfq_bitwise_foreach(class_mask, bit,
        {
                 int class = pfq_ctz(bit);
                 tmp = atomic_long_read(&g->sock_mask[class]);
                 tmp |= 1L << id.value;
                 atomic_long_set(&g->sock_mask[class], tmp);
        })

	if (g->owner.value == -1)
		g->owner = id;

	if (g->policy == Q_POLICY_GROUP_UNDEFINED)
		g->policy = policy;

        return 0;
}


static int
__pfq_leave_group(pfq_gid_t gid, pfq_id_t id)
{
        struct pfq_group * g;
        unsigned long tmp;
        int i;

	g = pfq_get_group(gid);
        if (g == NULL)
                return -EINVAL;

	if (!g->pid)
		return -EPERM;

        for(i = 0; i < Q_CLASS_MAX; ++i)
        {
                tmp = atomic_long_read(&g->sock_mask[i]);
                tmp &= ~(1L << id.value);
                atomic_long_set(&g->sock_mask[i], tmp);
        }

        if (__pfq_group_is_empty(gid))
                __pfq_group_free(gid);

        return 0;
}


unsigned long
pfq_get_all_groups_mask(pfq_gid_t gid)
{
        struct pfq_group * g;
        unsigned long mask = 0;
        int i;

	g = pfq_get_group(gid);
        if (g == NULL)
                return mask;

        for(i = 0; i < Q_CLASS_MAX; ++i)
        {
                mask |= atomic_long_read(&g->sock_mask[i]);
        }
        return mask;
}


void
pfq_set_group_filter(pfq_gid_t gid, struct sk_filter *filter)
{
        struct pfq_group * g;
        struct sk_filter * old_filter;

	g = pfq_get_group(gid);
        if (g == NULL) {
                pfq_free_sk_filter(filter);
                return;
        }

        old_filter = (void *)atomic_long_xchg(&g->bp_filter, (long)filter);

        msleep(Q_GRACE_PERIOD);

	if (old_filter)
		pfq_free_sk_filter(old_filter);
}


int
pfq_set_group_prog(pfq_gid_t gid, struct pfq_computation_tree *comp, void *ctx)
{
        struct pfq_group * g;
        struct pfq_computation_tree *old_comp;
        void *old_ctx;

	g = pfq_get_group(gid);
        if (g == NULL)
                return -EINVAL;

        down(&group_sem);

        old_comp = (struct pfq_computation_tree *)atomic_long_xchg(&g->comp, (long)comp);
        old_ctx  = (void *)atomic_long_xchg(&g->comp_ctx, (long)ctx);

        msleep(Q_GRACE_PERIOD);   /* sleeping is possible here: user-context */

	/* call fini on old computation */

	if (old_comp)
		pfq_computation_fini(old_comp);

        /* free the old computation/context */

        kfree(old_comp);
        kfree(old_ctx);

        up(&group_sem);
        return 0;
}


int
pfq_join_group(pfq_gid_t gid, pfq_id_t id, unsigned long class_mask, int policy)
{
        struct pfq_group * g;
        int ret;

	g = pfq_get_group(gid);
        if (g == NULL)
                return -EINVAL;

        down(&group_sem);

        ret = __pfq_join_group(gid, id, class_mask, policy);

        up(&group_sem);
        return ret;
}


int
pfq_join_free_group(pfq_id_t id, unsigned long class_mask, int policy)
{
        int n = 0;

        down(&group_sem);
        for(; n < Q_MAX_ID; n++)
        {
		pfq_gid_t gid = {n};

                if(!pfq_get_group(gid)->pid) {
                        __pfq_join_group(gid, id, class_mask, policy);
                        up(&group_sem);
                        return n;
                }
        }
        up(&group_sem);
        return -EPERM;
}


int
pfq_leave_group(pfq_gid_t gid, pfq_id_t id)
{
        struct pfq_group * g;
        int ret;

	g = pfq_get_group(gid);
        if (g == NULL)
                return -EINVAL;

        down(&group_sem);
        ret = __pfq_leave_group(gid,id);
        up(&group_sem);
        return ret;
}


void
pfq_leave_all_groups(pfq_id_t id)
{
        int n = 0;
        down(&group_sem);
        for(; n < Q_MAX_ID; n++)
        {
		pfq_gid_t gid = {n};
                __pfq_leave_group(gid, id);
        }
        up(&group_sem);
}


unsigned long
pfq_get_groups(pfq_id_t id)
{
        unsigned long ret = 0;
        int n = 0;
        down(&group_sem);
        for(; n < Q_MAX_ID; n++)
        {
		pfq_gid_t gid = {n};
                unsigned long mask = pfq_get_all_groups_mask(gid);

                if(mask & (1L << id.value))
                        ret |= (1UL << n);
        }
        up(&group_sem);
        return ret;
}


struct pfq_group *
pfq_get_group(pfq_gid_t gid)
{
        if (gid.value < 0 || gid.value >= Q_MAX_GROUP)
                return NULL;
        return &pfq_groups[gid.value];
}


bool
pfq_vlan_filters_enabled(pfq_gid_t gid)
{
        struct pfq_group *g;

        g = pfq_get_group(gid);
        if (g == NULL)
		return false;
        return g->vlan_filt;
}


bool
pfq_check_group_vlan_filter(pfq_gid_t gid, int vid)
{
        struct pfq_group *g;

        g= pfq_get_group(gid);
        if (g == NULL)
                return false;

        return g->vid_filters[vid & 4095];
}


bool
pfq_toggle_group_vlan_filters(pfq_gid_t gid, bool value)
{
        struct pfq_group *g;

        g = pfq_get_group(gid);
        if (g == NULL)
                return false;

        if (value)
                memset(g->vid_filters, 0, 4096);

        smp_wmb();

        g->vlan_filt = value;
        return true;
}


void
pfq_set_group_vlan_filter(pfq_gid_t gid, bool value, int vid)
{
        struct pfq_group *g;

        g = pfq_get_group(gid);
        if (g == NULL)
                return;

        g->vid_filters[vid & 4095] = value;
}

