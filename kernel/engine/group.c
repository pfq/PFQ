/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#include <pfq/kcompat.h>
#include <pfq/thread.h>
#include <pfq/group.h>

#include <engine/lang/engine.h>
#include <engine/percpu.h>
#include <engine/devmap.h>
#include <engine/group.h>
#include <engine/bitops.h>
#include <engine/global.h>


void
pfq_group_lock(void)
{
        mutex_lock(&global->groups_lock);
}


void
pfq_group_unlock(void)
{
        mutex_unlock(&global->groups_lock);
}


int
pfq_groups_init(void)
{
	int n;
	for(n = 0; n < Q_MAX_GID; n++)
	{
		struct pfq_group * group = &global->groups[n];

		group->pid = 0;
		group->owner = Q_INVALID_ID;
		group->policy = Q_POLICY_GROUP_UNDEFINED;

		group->stats = alloc_percpu(pfq_group_stats_t);
		if (group->stats == NULL) {
			goto err;
		}

		group->counters = alloc_percpu(struct pfq_group_counters);
		if (group->counters == NULL) {
			goto err;
		}

		pfq_group_stats_reset(group->stats);
		pfq_group_counters_reset(group->counters);
	}

	return 0;
err:
	pfq_groups_destruct();

	return -ENOMEM;
}


void
pfq_groups_destruct(void)
{
	int n;
	for(n = 0; n < Q_MAX_GID; n++)
	{
		struct pfq_group * group = &global->groups[n];

		free_percpu(group->stats);
		free_percpu(group->counters);
		group->stats = NULL;
		group->counters = NULL;
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
        struct pfq_group * group;

        group = pfq_get_group(gid);
        if (group == NULL)
                return false;

        switch(group->policy)
        {
        case Q_POLICY_GROUP_PRIVATE:
                return group->owner == id; // __pfq_has_joined_group(gid,id);

        case Q_POLICY_GROUP_RESTRICTED:
                return (policy == Q_POLICY_GROUP_RESTRICTED) && group->pid == pfq_getpid();

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
	struct pfq_group *group;

	group = pfq_get_group(gid);
	if (group == NULL)
		return false;

	switch(group->policy)
	{
	case Q_POLICY_GROUP_PRIVATE:
		return group->owner == id;

        case Q_POLICY_GROUP_RESTRICTED:
                return group->pid == pfq_getpid();

        case Q_POLICY_GROUP_SHARED:
        case Q_POLICY_GROUP_UNDEFINED:
                return true;
	}

	return false;
}


static void
__pfq_group_init(pfq_gid_t gid)
{
        struct pfq_group * group;
        size_t i;

	group = pfq_get_group(gid);
        if (group == NULL)
                return;

	group->pid = pfq_getpid();
        group->owner = Q_INVALID_ID;
        group->policy = Q_POLICY_GROUP_UNDEFINED;

        for(i = 0; i < Q_CLASS_MAX; i++)
        {
                atomic_long_set(&group->sock_id[i], 0);
        }

	pfq_invalidate_percpu_eligible_mask((pfq_id_t __force)0);

        atomic_long_set(&group->bp_filter,0L);
        atomic_long_set(&group->comp,     0L);
        atomic_long_set(&group->comp_ctx, 0L);

	pfq_group_stats_reset(group->stats);
	pfq_group_counters_reset(group->counters);
}


static void
__pfq_group_free(pfq_gid_t gid)
{
        struct pfq_group * group;
        struct sk_filter *filter;
        struct pfq_lang_computation_tree *old_comp;
        void *old_ctx;

	group = pfq_get_group(gid);
        if (group == NULL)
                return;

        /* remove this gid from devmap matrix */

        pfq_devmap_update(map_reset, Q_ANY_DEVICE, Q_ANY_QUEUE, gid);

        group->pid    = 0;
        group->owner  = Q_INVALID_ID;
        group->policy = Q_POLICY_GROUP_UNDEFINED;

        filter   = (struct sk_filter *)atomic_long_xchg(&group->bp_filter, 0L);
        old_comp = (struct pfq_lang_computation_tree *)atomic_long_xchg(&group->comp, 0L);
        old_ctx  = (void *)atomic_long_xchg(&group->comp_ctx, 0L);

        msleep(Q_GRACE_PERIOD);   /* sleeping is possible here: user-context */

	/* finalize old computation */

	if (old_comp)
		pfq_lang_computation_destruct(old_comp);

	kfree(old_comp);
	kfree(old_ctx);

	if (filter)
		pfq_free_sk_filter(filter);

        group->vlan_filt = false;

        pr_devel("[PFQ] group gid=%d freed.\n", gid);
}


static int
__pfq_join_group(pfq_gid_t gid, pfq_id_t id, unsigned long class_mask, int policy)
{
        struct pfq_group * group;
        unsigned long tmp = 0;
        unsigned long bit;

	group = pfq_get_group(gid);
        if (group == NULL)
                return -EINVAL;

	/* if this group is unused, initializes it */

        if (!group->pid)
                __pfq_group_init(gid);

        if (!pfq_group_policy_access(gid, id, policy)) {
                pr_devel("[PFQ] group gid=%d is not join-able with policy %d\n", gid, policy);
                return -EACCES;
        }

        pfq_bitwise_foreach(class_mask, bit,
        {
                 int class = pfq_ctz(bit);
                 tmp = atomic_long_read(&group->sock_id[class]);
                 tmp |= 1L << (__force int)id;
                 atomic_long_set(&group->sock_id[class], tmp);
        })

	pfq_invalidate_percpu_eligible_mask(id);

	if (group->owner == Q_INVALID_ID)
		group->owner = id;

	if (group->policy == Q_POLICY_GROUP_UNDEFINED)
		group->policy = policy;

	pr_devel("[PFQ|%d] group %d, sock_ids { %lu %lu %lu %lu %lu...\n", id, gid,
		 atomic_long_read(&group->sock_id[0]),
		 atomic_long_read(&group->sock_id[1]),
		 atomic_long_read(&group->sock_id[2]),
		 atomic_long_read(&group->sock_id[3]),
		 atomic_long_read(&group->sock_id[4]));

        return 0;
}


static int
__pfq_leave_group(pfq_gid_t gid, pfq_id_t id)
{
        struct pfq_group * group;
        unsigned long tmp;
        size_t i;

	group = pfq_get_group(gid);
        if (group == NULL)
                return -EINVAL;

        for(i = 0; i < Q_CLASS_MAX; ++i)
        {
                tmp = atomic_long_read(&group->sock_id[i]);
                tmp &= ~(1L << (__force int)id);
                atomic_long_set(&group->sock_id[i], tmp);
        }

	pfq_invalidate_percpu_eligible_mask(id);

	if (group->pid && __pfq_group_is_empty(gid))
		__pfq_group_free(gid);

        return 0;
}


unsigned long
pfq_get_all_groups_mask(pfq_gid_t gid)
{
        struct pfq_group * group;
        unsigned long mask = 0;
        size_t i;

	group = pfq_get_group(gid);
        if (group == NULL)
                return mask;

        for(i = 0; i < Q_CLASS_MAX; ++i)
        {
                mask |= atomic_long_read(&group->sock_id[i]);
        }
        return mask;
}


void
pfq_set_group_filter(pfq_gid_t gid, struct sk_filter *filter)
{
        struct pfq_group * group;
        struct sk_filter * old_filter;

	group = pfq_get_group(gid);
        if (group == NULL) {
                pfq_free_sk_filter(filter);
                return;
        }

        old_filter = (void *)atomic_long_xchg(&group->bp_filter, (long)filter);

        msleep(Q_GRACE_PERIOD);

	if (old_filter)
		pfq_free_sk_filter(old_filter);
}


int
pfq_set_group_prog(pfq_gid_t gid, struct pfq_lang_computation_tree *comp, void *ctx)
{
        struct pfq_group * group;
        struct pfq_lang_computation_tree *old_comp;
        void *old_ctx;

	group = pfq_get_group(gid);
        if (group == NULL)
                return -EINVAL;

        mutex_lock(&global->groups_lock);

        old_comp = (struct pfq_lang_computation_tree *)atomic_long_xchg(&group->comp, (long)comp);
        old_ctx  = (void *)atomic_long_xchg(&group->comp_ctx, (long)ctx);

        msleep(Q_GRACE_PERIOD);   /* sleeping is possible here: user-context */

	/* call fini on old computation */

	if (old_comp)
		pfq_lang_computation_destruct(old_comp);

        /* free the old computation/context */

        kfree(old_comp);
        kfree(old_ctx);

        mutex_unlock(&global->groups_lock);
        return 0;
}


int
pfq_join_group(pfq_gid_t gid, pfq_id_t id, unsigned long class_mask, int policy)
{
        struct pfq_group * group;
        int ret;

	group = pfq_get_group(gid);
        if (group == NULL)
                return -EINVAL;

        mutex_lock(&global->groups_lock);

        ret = __pfq_join_group(gid, id, class_mask, policy);

        mutex_unlock(&global->groups_lock);
        return ret;
}


int
pfq_join_free_group(pfq_id_t id, unsigned long class_mask, int policy)
{
        int n = 0;

        mutex_lock(&global->groups_lock);
        for(; n < Q_MAX_ID; n++)
        {
		pfq_gid_t gid = (__force pfq_gid_t)n;

                if(!pfq_get_group(gid)->pid) {
                        __pfq_join_group(gid, id, class_mask, policy);
                        mutex_unlock(&global->groups_lock);
                        return n;
                }
        }
        mutex_unlock(&global->groups_lock);
        return -EPERM;
}


int
pfq_leave_group(pfq_gid_t gid, pfq_id_t id)
{
        struct pfq_group * group;
        int ret;

	group = pfq_get_group(gid);
        if (group == NULL)
                return -EINVAL;

        mutex_lock(&global->groups_lock);
        ret = __pfq_leave_group(gid,id);
        mutex_unlock(&global->groups_lock);
        return ret;
}


void
pfq_leave_all_groups(pfq_id_t id)
{
        int n = 0;

        mutex_lock(&global->groups_lock);
        for(; n < Q_MAX_ID; n++)
        {
		pfq_gid_t gid = (__force pfq_gid_t)n;
                __pfq_leave_group(gid, id);
        }
        mutex_unlock(&global->groups_lock);

        pr_devel("[PFQ|%d] all group left.\n", id);
}


unsigned long
pfq_get_groups(pfq_id_t id)
{
        unsigned long ret = 0;
        int n = 0;
        mutex_lock(&global->groups_lock);
        for(; n < Q_MAX_ID; n++)
        {
		pfq_gid_t gid = (__force pfq_gid_t)n;
                unsigned long mask = pfq_get_all_groups_mask(gid);

                if(mask & (1L << (__force int)id))
                        ret |= (1UL << n);
        }
        mutex_unlock(&global->groups_lock);
        return ret;
}


struct pfq_group *
pfq_get_group(pfq_gid_t gid)
{
        if ((__force int)gid < 0 ||
            (__force int)gid >= Q_MAX_GID)
                return NULL;
        return &global->groups[(__force int)gid];
}


bool
pfq_vlan_filters_enabled(pfq_gid_t gid)
{
        struct pfq_group *group;

        group = pfq_get_group(gid);
        if (group == NULL)
		return false;
        return group->vlan_filt;
}


bool
pfq_check_group_vlan_filter(pfq_gid_t gid, int vid)
{
        struct pfq_group *group;

        group= pfq_get_group(gid);
        if (group == NULL)
                return false;

        return group->vid_filters[vid & 4095];
}


bool
pfq_toggle_group_vlan_filters(pfq_gid_t gid, bool value)
{
        struct pfq_group *group;

        group = pfq_get_group(gid);
        if (group == NULL)
                return false;

        if (value)
                memset(group->vid_filters, 0, 4096);

        smp_wmb();

        group->vlan_filt = value;
        return true;
}


void
pfq_set_group_vlan_filter(pfq_gid_t gid, bool value, int vid)
{
        struct pfq_group *group;

        group = pfq_get_group(gid);
        if (group == NULL)
                return;

        group->vid_filters[vid & 4095] = value;
}

