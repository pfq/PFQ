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

#include <linux/kernel.h>
#include <linux/bug.h>
#include <linux/module.h>
#include <linux/semaphore.h>

#include <pf_q-group.h>
#include <pf_q-devmap.h>
#include <pf_q-bitops.h>


static DEFINE_SEMAPHORE(group_sem);

struct pfq_group pfq_groups[Q_MAX_GROUP];


bool
__pfq_group_access(int gid, int id, int policy, bool join)
{
        struct pfq_group * g = pfq_get_group(gid);
        if (!g) {
                pr_devel("[PFQ] get_group: invalid group id %d!\n", gid);
                return false;
        }

        if (__pfq_has_joined_group(gid,id))
                return true;

        switch(g->policy)
        {
        case Q_POLICY_GROUP_PRIVATE:
                return false;

        case Q_POLICY_GROUP_RESTRICTED:
                return (join == false || policy == Q_POLICY_GROUP_RESTRICTED) && g->pid == current->tgid;

        case Q_POLICY_GROUP_SHARED:
                return join == false || policy == Q_POLICY_GROUP_SHARED;

        case Q_POLICY_GROUP_UNDEFINED:
                return true;
        }

        return false;
}


static void
__pfq_group_init(int gid)
{
        struct pfq_group * g = pfq_get_group(gid);
        int i;

        if (!g) {
                pr_devel("[PFQ] get_group: invalid group id %d!\n", gid);
                return;
        }

        g->pid = -1;
        g->policy = Q_POLICY_GROUP_UNDEFINED;

        for(i = 0; i < Q_CLASS_MAX; i++)
        {
                atomic_long_set(&g->sock_mask[i], 0);
        }

        atomic_long_set(&g->filter,   0L);
        atomic_long_set(&g->comp,     0L);
        atomic_long_set(&g->comp_ctx, 0L);

        sparse_set(&g->recv, 0);
        sparse_set(&g->lost, 0);
        sparse_set(&g->drop, 0);

        for(i = 0; i < Q_MAX_COUNTERS; i++)
        {
                sparse_set(&g->ctx.counter[i], 0);
        }

	for(i = 0; i < Q_MAX_PERSISTENT; i++)
	{
		spin_lock_init(&g->ctx.persistent[i].lock);
		memset(g->ctx.persistent[i].memory, 0, sizeof(g->ctx.persistent[i].memory));
	}
}


static void
__pfq_group_free(int gid)
{
        struct pfq_group * g = pfq_get_group(gid);
        struct sk_filter *filter;
        struct pfq_computation_tree *old_comp;
        void *old_ctx;

        if (!g) {
                pr_devel("[PFQ] get_group: invalid group id %d!\n", gid);
                return;
        }

        /* remove this gid from demux matrix */

        pfq_devmap_update(map_reset, Q_ANY_DEVICE, Q_ANY_QUEUE, gid);

        g->pid = 0;
        g->policy = Q_POLICY_GROUP_UNDEFINED;

        filter   = (struct sk_filter *)atomic_long_xchg(&g->filter, 0L);
        old_comp = (struct pfq_computation_tree *)atomic_long_xchg(&g->comp, 0L);
        old_ctx  = (void *)atomic_long_xchg(&g->comp_ctx, 0L);

        msleep(Q_GRACE_PERIOD);   /* sleeping is possible here: user-context */

	/* call fini on old computation */

	if (old_comp)
 		pfq_computation_fini(old_comp);

        kfree(old_comp);
        kfree(old_ctx);

        pfq_free_sk_filter(filter);

        g->vlan_filt = false;

        pr_devel("[PFQ] group id:%d destroyed.\n", gid);
}


static int
__pfq_join_group(int gid, int id, unsigned long class_mask, int policy)
{
        struct pfq_group * g = pfq_get_group(gid);
        unsigned long tmp = 0;
        unsigned long bit;

        if (!g) {
                pr_devel("[PFQ] get_group: invalid group id %d!\n", gid);
                return -EINVAL;
        }

        if (!g->pid) {
                __pfq_group_init(gid);
        }

        if (!__pfq_group_access(gid, id, policy, true)) {
                pr_devel("[PFQ] gid:%d is not joinable with policy %d\n", gid, policy);
                return -1;
        }

        pfq_bitwise_foreach(class_mask, bit,
        {
                 int class = pfq_ctz(bit);
                 tmp = atomic_long_read(&g->sock_mask[class]);
                 tmp |= 1L << id;
                 atomic_long_set(&g->sock_mask[class], tmp);
        })

        g->policy = g->policy == Q_POLICY_GROUP_UNDEFINED ?  policy : g->policy;
        g->pid    = policy == Q_POLICY_GROUP_RESTRICTED ? current->tgid : -1;

        return 0;
}


static int
__pfq_leave_group(int gid, int id)
{
        struct pfq_group * g = pfq_get_group(gid);
        unsigned long tmp;
        int i;

        if (!g) {
                pr_devel("[PFQ] get_group: invalid group id %d!\n", gid);
                return -EINVAL;
        }

	if (!g->pid)
		return -1;

        for(i = 0; i < Q_CLASS_MAX; ++i)
        {
                tmp = atomic_long_read(&g->sock_mask[i]);
                tmp &= ~(1L << id);
                atomic_long_set(&g->sock_mask[i], tmp);
        }

        if (__pfq_group_is_empty(gid)) {
                __pfq_group_free(gid);
        }

        return 0;
}

unsigned long
__pfq_get_all_groups_mask(int gid)
{
        struct pfq_group * g = pfq_get_group(gid);
        unsigned long mask = 0;
        int i;

        if (!g) {
                pr_devel("[PFQ] get_group: invalid group id %d!\n", gid);
                return mask;
        }

        for(i = 0; i < Q_CLASS_MAX; ++i)
        {
                mask |= atomic_long_read(&g->sock_mask[i]);
        }
        return mask;
}


void __pfq_set_group_filter(int gid, struct sk_filter *filter)
{
        struct pfq_group * g = pfq_get_group(gid);
        struct sk_filter * old_filter;

        if (!g) {
                pr_devel("[PFQ] get_group: invalid group id %d!\n", gid);
                pfq_free_sk_filter(filter);
                return;
        }

        old_filter = (void *)atomic_long_xchg(& g->filter, (long)filter);

        msleep(Q_GRACE_PERIOD);

        pfq_free_sk_filter(old_filter);
}


void __pfq_dismiss_function(void *f)
{
        int n;

        for(n = 0; n < Q_MAX_GROUP; n++)
        {
                struct pfq_computation_tree *comp = (struct pfq_computation_tree *)atomic_long_read(&pfq_get_group(n)->comp);

                BUG_ON(comp != NULL);

#if 0
                if (comp) {
                        int n = 0;
                        for(; n < comp->size; n++)
                        {
                                if (comp->fun[n].fun.eval == f)
                                {
                                        comp->fun[n].fun.eval = NULL;
                                }
                        }
                }
#endif

        }

        printk(KERN_INFO "[PFQ] function @%p dismissed.\n", f);
}


int pfq_set_group_prog(int gid, struct pfq_computation_tree *comp, void *ctx)
{
        struct pfq_group * g = pfq_get_group(gid);
        struct pfq_computation_tree *old_comp;
        void *old_ctx;

        if (!g) {
                pr_devel("[PFQ] get_group: invalid group id %d!\n", gid);
                return -EINVAL;
        }

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
pfq_join_group(int gid, int id, unsigned long class_mask, int policy)
{
        struct pfq_group * g = pfq_get_group(gid);
        int ret;

        if (!g) {
                pr_devel("[PFQ] get_group: invalid group id %d!\n", gid);
                return -EINVAL;
        }

        down(&group_sem);

        ret = __pfq_join_group(gid, id, class_mask, policy);

        up(&group_sem);
        return ret;
}


int
pfq_join_free_group(int id, unsigned long class_mask, int policy)
{
        int n = 0;

        down(&group_sem);
        for(; n < Q_MAX_ID; n++)
        {
                if(!pfq_get_group(n)->pid)
                {
                        __pfq_join_group(n, id, class_mask, policy);
                        up(&group_sem);
                        return n;
                }
        }
        up(&group_sem);
        return -1;
}


int
pfq_leave_group(int gid, int id)
{
        struct pfq_group * g = pfq_get_group(gid);
        int ret;

        if (!g) {
                pr_devel("[PFQ] get_group: invalid group id %d!\n", gid);
                return -EINVAL;
        }

        down(&group_sem);
        ret = __pfq_leave_group(gid,id);
        up(&group_sem);
        return ret;
}


void
pfq_leave_all_groups(int id)
{
        int n = 0;
        down(&group_sem);
        for(; n < Q_MAX_ID; n++)
        {
                __pfq_leave_group(n, id);
        }
        up(&group_sem);
}


unsigned long
pfq_get_groups(int id)
{
        unsigned long ret = 0;
        int n = 0;
        down(&group_sem);
        for(; n < Q_MAX_ID; n++)
        {
                unsigned long mask = __pfq_get_all_groups_mask(n);
                if(mask & (1L << id))
                {
                        ret |= (1UL << n);
                }
        }
        up(&group_sem);
        return ret;
}


