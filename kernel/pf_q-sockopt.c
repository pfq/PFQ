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

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/version.h>

#include <linux/kthread.h>
#include <linux/pf_q.h>

#include <pf_q-thread.h>
#include <pf_q-common.h>
#include <pf_q-global.h>
#include <pf_q-sock.h>
#include <pf_q-group.h>
#include <pf_q-memory.h>
#include <pf_q-queue.h>
#include <pf_q-devmap.h>


extern atomic_t timestamp_toggle;


#define CHECK_GROUP(gid, msg) \
        if (gid < 0 || gid >= Q_MAX_GROUP) { \
        	    pr_devel("[PFQ|%d] " msg " error: invalid group (gid:%d)!\n", so->id, gid); \
                return -EINVAL; \
        } \

#define CHECK_GROUP_ACCES(gid, msg) \
        CHECK_GROUP(gid,msg); \
        if (!__pfq_has_joined_group(gid, so->id)) { \
        	    pr_devel("[PFQ|%d] " msg " error: permission denied (git:%d)!\n", so->id, gid); \
                return -EACCES; \
        }


int pfq_getsockopt(struct socket *sock,
                   int level, int optname,
                   char __user * optval, int __user * optlen)
{
        struct pfq_sock *so = pfq_sk(sock->sk);
        struct pfq_rx_opt * ro;
        struct pfq_tx_opt * to;
        int len;

        if (so == NULL)
                return -EFAULT;

        ro = &so->rx_opt;
        to = &so->tx_opt;

	if (get_user(len, optlen))
                return -EFAULT;
	if (len < 0)
		return -EINVAL;

        switch(optname)
        {

        case Q_SO_GROUP_JOIN:
            {
                    struct pfq_group_join group;

                    if (len != sizeof(group))
                            return -EINVAL;

		    if (copy_from_user(&group, optval, len))
                            return -EFAULT;

		    if (group.class_mask == 0) {
			    pr_devel("[PFQ|%d] join error: bad class_mask(%x)!\n", so->id, group.class_mask);
			    return -EINVAL;
		    }

                    if (group.gid == Q_ANY_GROUP) {

                            group.gid = pfq_join_free_group(so->id, group.class_mask, group.policy);
                            if (group.gid < 0)
                                    return -EFAULT;
                            if (copy_to_user(optval, &group, len))
                                    return -EFAULT;
                    }
                    else {
                            CHECK_GROUP(group.gid, "group join");

			    if (pfq_join_group(group.gid, so->id, group.class_mask, group.policy) < 0) {
				    pr_devel("[PFQ|%d] join error: permission denied (gid:%d)!\n", so->id, group.gid);
				    return -EACCES;
			    }
		    }
		    pr_devel("[PFQ|%d] join -> gid:%d class_mask:%x\n", so->id, group.gid, group.class_mask);
            } break;

        case Q_SO_GET_ID:
            {
                    if (len != sizeof(so->id))
                            return -EINVAL;
                    if (copy_to_user(optval, &so->id, sizeof(so->id)))
                            return -EFAULT;
            } break;

        case Q_SO_GET_STATUS:
            {
                    int enabled;
                    if (len != sizeof(int))
                            return -EINVAL;

                    enabled = so->mem_addr == NULL ? 0 : 1;

                    if (copy_to_user(optval, &enabled, sizeof(enabled)))
                            return -EFAULT;

            } break;

        case Q_SO_GET_STATS:
            {
                    struct pfq_stats stat;
                    if (len != sizeof(struct pfq_stats))
                            return -EINVAL;

                    stat.recv = sparse_read(&ro->stat.recv);
                    stat.lost = sparse_read(&ro->stat.lost);
                    stat.drop = sparse_read(&ro->stat.drop);

                    if (copy_to_user(optval, &stat, sizeof(stat)))
                            return -EFAULT;
            } break;

        case Q_SO_GET_RX_TSTAMP:
            {
                    if (len != sizeof(ro->tstamp))
                            return -EINVAL;
                    if (copy_to_user(optval, &ro->tstamp, sizeof(ro->tstamp)))
                            return -EFAULT;
            } break;

        case Q_SO_GET_QUEUE_MEM:
            {
                    if (len != sizeof(so->mem_size))
                            return -EINVAL;

                    if (copy_to_user(optval, &so->mem_size, sizeof(so->mem_size)))
                            return -EFAULT;
            } break;

        case Q_SO_GET_RX_CAPLEN:
            {
                    if (len != sizeof(ro->caplen))
                            return -EINVAL;
                    if (copy_to_user(optval, &ro->caplen, sizeof(ro->caplen)))
                            return -EFAULT;
            } break;

        case Q_SO_GET_TX_MAXLEN:
            {
                    if (len != sizeof(to->maxlen))
                            return -EINVAL;
                    if (copy_to_user(optval, &to->maxlen, sizeof(to->maxlen)))
                            return -EFAULT;
            } break;

        case Q_SO_GET_RX_SLOTS:
            {
                    if (len != sizeof(ro->size))
                            return -EINVAL;
                    if (copy_to_user(optval, &ro->size, sizeof(ro->size)))
                            return -EFAULT;
            } break;

        case Q_SO_GET_TX_SLOTS:
            {
                    if (len != sizeof(to->size))
                            return -EINVAL;
                    if (copy_to_user(optval, &to->size, sizeof(to->size)))
                            return -EFAULT;
            } break;

        case Q_SO_GET_RX_OFFSET:
            {
                    if (len != sizeof(ro->offset))
                            return -EINVAL;
                    if (copy_to_user(optval, &ro->offset, sizeof(ro->offset)))
                            return -EFAULT;
            } break;

        case Q_SO_GET_GROUPS:
            {
                    unsigned long grps;
                    if(len != sizeof(unsigned long))
                            return -EINVAL;
                    grps = pfq_get_groups(so->id);
                    if (copy_to_user(optval, &grps, sizeof(grps)))
                            return -EFAULT;
            } break;

        case Q_SO_GET_GROUP_STATS:
            {
                    struct pfq_stats stat;
                    int gid;

		    if (len != sizeof(stat))
                            return -EINVAL;

                    if (copy_from_user(&stat, optval, len))
                            return -EFAULT;

		    gid = (int)stat.recv;

                    CHECK_GROUP(gid, "group stat");

		    /* check whether the group is joinable.. */

		    if (!__pfq_group_access(gid, so->id, Q_GROUP_UNDEFINED, false)) {
                    	    pr_devel("[PFQ|%d] group stats error: permission denied (gid:%d)!\n", so->id, gid);
			    return -EACCES;
		    }

                    stat.recv = sparse_read(&pfq_groups[gid].recv);
                    stat.lost = sparse_read(&pfq_groups[gid].lost);
                    stat.drop = sparse_read(&pfq_groups[gid].drop);

                    if (copy_to_user(optval, &stat, sizeof(stat)))
                            return -EFAULT;
            } break;

	case Q_SO_GET_GROUP_CONTEXT:
	    {
		    struct pfq_group_context s;

		    if (len != sizeof(s))
			    return -EINVAL;

		    if (copy_from_user(&s, optval, len))
			    return -EFAULT;

                    CHECK_GROUP(s.gid, "group context");

                    if (!s.size || !s.context) {
                    	    pr_devel("[PFQ|%d] group context error: gid:%d invalid argument!\n", so->id, s.gid);
                            return -EFAULT;
                    }

		    /* check whether the group is joinable.. */

		    if (!__pfq_group_access(s.gid, so->id, Q_GROUP_UNDEFINED, false)) {
                    	    pr_devel("[PFQ|%d] group context error: permission denied (gid:%d)!\n", so->id, s.gid);
			    return -EACCES;
		    }

                    if (__pfq_get_group_context(s.gid, s.level, s.size, s.context) < 0) {
                    	pr_devel("[PFQ|%d] get context error: gid:%d error!\n", so->id, s.gid);
                            return -EFAULT;
                    }

	    } break;

        default:
            return -EFAULT;
        }

        return 0;
}



int pfq_setsockopt(struct socket *sock,
                   int level, int optname,
                   char __user * optval,
#if(LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,31))
                   unsigned
#endif
                   int optlen)
{
        struct pfq_sock *so = pfq_sk(sock->sk);
        struct pfq_rx_opt * ro;
        struct pfq_tx_opt * to;

        bool found = true;

        if (so == NULL)
                return -EINVAL;

        ro = &so->rx_opt;
        to = &so->tx_opt;

        switch(optname)
        {
        case Q_SO_TOGGLE_QUEUE:
            {
                    int active;
                    if (optlen != sizeof(active))
                            return -EINVAL;
                    if (copy_from_user(&active, optval, optlen))
                            return -EFAULT;

                    if (active)
                    {
                            if (!so->mem_addr)
                            {
                                    struct pfq_queue_hdr * queue;

                                    /* alloc queue memory */

                                    if (pfq_queue_alloc(so, pfq_queue_total_mem(so)) < 0)
                                    {
                                        return -ENOMEM;
                                    }

                                    /* so->mem_addr and so->mem_size are correctly configured */

                                    /* initialize queues headers */

                                    queue = (struct pfq_queue_hdr *)so->mem_addr;

                                    /* initialize rx queue header */

                                    queue->rx.data              = (1L << 24);
                                    queue->rx.poll_wait         = 0;
                                    queue->rx.size              = so->rx_opt.size;
                                    queue->rx.slot_size         = so->rx_opt.slot_size;

                                    /* TODO: initialize tx queue header */

                                    queue->tx.producer.index    = 0;
                                    queue->tx.producer.cache    = 0;
                                    queue->tx.consumer.index    = 0;
                                    queue->tx.consumer.cache    = 0;

                                    queue->tx.size_mask         = so->tx_opt.size - 1;
                                    queue->tx.max_len           = so->tx_opt.maxlen;
                                    queue->tx.size              = so->tx_opt.size;
                                    queue->tx.slot_size         = so->tx_opt.slot_size;

                                    /* update the queues base_addr */

                                    so->rx_opt.base_addr = so->mem_addr + sizeof(struct pfq_queue_hdr);
                                    so->tx_opt.base_addr = so->mem_addr + sizeof(struct pfq_queue_hdr) + pfq_queue_mpdb_mem(so);

                                    /* commit both the queues */

				    smp_wmb();

                                    so->rx_opt.queue_info = &queue->rx;
                                    so->tx_opt.queue_info = &queue->tx;

                                    pr_devel("[PFQ|%d] queue: rx_size:%d rx_slot_size:%d tx_size:%d tx_slot_size:%d\n", so->id, queue->rx.size,
                                                                                                                                queue->rx.slot_size,
                                                                                                                                queue->tx.size,
                                                                                                                                queue->tx.slot_size);
                            }
                    }
                    else
                    {
                        if (so->tx_opt.thread)
                        {
                                pr_devel("[PFQ|%d] stopping TX thread...\n", so->id);
                                kthread_stop(so->tx_opt.thread);
                                so->tx_opt.thread = NULL;
                        }

                        msleep(Q_GRACE_PERIOD);

                        pfq_queue_free(so);
                    }

            } break;

        case Q_SO_ADD_BINDING:
            {
                    struct pfq_binding bind;
                    if (optlen != sizeof(struct pfq_binding))
                            return -EINVAL;

                    if (copy_from_user(&bind, optval, optlen))
                            return -EFAULT;

                    CHECK_GROUP_ACCES(bind.gid, "add binding");

                    pfq_devmap_update(map_set, bind.if_index, bind.hw_queue, bind.gid);
            } break;

        case Q_SO_REMOVE_BINDING:
            {
                    struct pfq_binding bind;
                    if (optlen != sizeof(struct pfq_binding))
                            return -EINVAL;

		    if (copy_from_user(&bind, optval, optlen))
                            return -EFAULT;

                    CHECK_GROUP_ACCES(bind.gid, "remove binding");

                    pfq_devmap_update(map_reset, bind.if_index, bind.hw_queue, bind.gid);
            } break;

	case Q_SO_SET_RX_TSTAMP:
            {
                    int tstamp;
                    if (optlen != sizeof(so->rx_opt.tstamp))
                            return -EINVAL;

                    if (copy_from_user(&tstamp, optval, optlen))
                            return -EFAULT;
                    if (tstamp != 0 && tstamp != 1)
                            return -EINVAL;

                    /* update the timestamp_toggle counter */
                    atomic_add(tstamp - so->rx_opt.tstamp, &timestamp_toggle);
                    so->rx_opt.tstamp = tstamp;

                    pr_devel("[PFQ|%d] timestamp_toggle => %d\n", so->id, atomic_read(&timestamp_toggle));
            } break;

        case Q_SO_SET_RX_CAPLEN:
            {
                    size_t caplen;

                    if (optlen != sizeof(caplen))
                            return -EINVAL;
                    if (copy_from_user(&caplen, optval, optlen))
                            return -EFAULT;

                    so->rx_opt.caplen = min(caplen, (size_t)cap_len); /* cap_len: max capture length */

                    so->rx_opt.slot_size = MPDB_QUEUE_SLOT_SIZE(so->rx_opt.caplen);

                    pr_devel("[PFQ|%d] caplen:%zu -> slot_size:%zu\n",
                                    so->id, so->rx_opt.caplen, so->rx_opt.slot_size);
            } break;

        case Q_SO_SET_RX_SLOTS:
            {
                    size_t slots;

                    if (optlen != sizeof(slots))
                            return -EINVAL;
                    if (copy_from_user(&slots, optval, optlen))
                            return -EFAULT;

                    so->rx_opt.size = min(slots, (size_t)rx_queue_slots);

                    pr_devel("[PFQ|%d] rx_queue_slots:%zu\n", so->id, so->rx_opt.size);
            } break;


        case Q_SO_SET_TX_MAXLEN:
            {
                    size_t maxlen;

                    if (optlen != sizeof(maxlen))
                            return -EINVAL;
                    if (copy_from_user(&maxlen, optval, optlen))
                            return -EFAULT;

                    so->tx_opt.maxlen = min(maxlen, (size_t)max_len); /* cap_len: max capture length */

                    so->tx_opt.slot_size = SPSC_QUEUE_SLOT_SIZE(so->tx_opt.maxlen); /* max_len: max length */

                    pr_devel("[PFQ|%d] tx_slot_size:%zu\n", so->id, so->rx_opt.slot_size);
            } break;


        case Q_SO_SET_TX_SLOTS:
            {
                    size_t slots;

                    if (optlen != sizeof(slots))
                            return -EINVAL;
                    if (copy_from_user(&slots, optval, optlen))
                            return -EFAULT;

                    if (slots & (slots-1))
                            return -EINVAL;

                    so->tx_opt.size = min(slots,(size_t)tx_queue_slots);

                    pr_devel("[PFQ|%d] tx_queue_slots:%zu\n", so->id, so->tx_opt.size);
            } break;

        case Q_SO_SET_RX_OFFSET:
            {
                    if (optlen != sizeof(so->rx_opt.offset))
                            return -EINVAL;
                    if (copy_from_user(&so->rx_opt.offset, optval, optlen))
                            return -EFAULT;

                    pr_devel("[PFQ|%d] offset:%zu\n", so->id, so->rx_opt.offset);
            } break;

        case Q_SO_GROUP_LEAVE:
            {
                    int gid;
                    if (optlen != sizeof(gid))
                            return -EINVAL;

                    if (copy_from_user(&gid, optval, optlen))
                            return -EFAULT;

                    if (pfq_leave_group(gid, so->id) < 0) {
                            return -EFAULT;
                    }

                    pr_devel("[PFQ|%d] leave: gid:%d\n", so->id, gid);
            } break;

        case Q_SO_GROUP_RESET: /* functional */
            {
                    int gid;
                    if (optlen != sizeof(gid))
                            return -EINVAL;

                    if (copy_from_user(&gid, optval, optlen))
                            return -EFAULT;

                    CHECK_GROUP_ACCES(gid, "reset group");

                    __pfq_reset_group_functx(gid);

                    pr_devel("[PFQ|%d] reset group gid:%d\n", so->id, gid);
            } break;

	case Q_SO_GROUP_CONTEXT:
	    {
		    struct pfq_group_context s;
		    if (optlen != sizeof(s))
			    return -EINVAL;

		    if (copy_from_user(&s, optval, optlen))
			    return -EFAULT;

                    CHECK_GROUP_ACCES(s.gid, "group context");

		    if (s.size && s.context)
		    {
                    	void * context = kmalloc(s.size, GFP_KERNEL);
			if (context == NULL)
				return -ENOMEM;

			if(copy_from_user(context, s.context, s.size)) {
                         	kfree(context);
				return -EFAULT;
			}

			if (__pfq_set_group_context(s.gid, context, s.level) < 0) {
                    		pr_devel("[PFQ|%d] context error: gid:%d invalid level (%d)!\n", so->id, s.gid, s.level);
			        return -EINVAL;
                        }

			pr_devel("[PFQ|%d] context: gid:%d (context of %zu bytes set)\n", so->id, s.gid, s.size);
		    }
		    else { /* empty context */

			if (__pfq_set_group_context(s.gid, NULL, s.level) < 0) {
                    		pr_devel("[PFQ|%d] context error: gid:%d invalid level (%d)!\n", so->id, s.gid, s.level);
			        return -EINVAL;
                        }

			pr_devel("[PFQ|%d] context: gid:%d (empty context set)\n", so->id, s.gid);
		    }
	    } break;

 	case Q_SO_GROUP_FUN:
	    {
		    struct pfq_group_function s;

		    if (optlen != sizeof(s))
			    return -EINVAL;

		    if (copy_from_user(&s, optval, optlen))
			    return -EFAULT;

                    CHECK_GROUP_ACCES(s.gid, "group function");

		    if (s.name == NULL) {

			if (__pfq_set_group_function(s.gid, NULL, s.level) < 0) {
                    		pr_devel("[PFQ|%d] function error: gid:%d invalid level (%d)!\n", so->id, s.gid, s.level);
			        return -EINVAL;
                        }

                    	pr_devel("[PFQ|%d] function: gid:%d (NONE)\n", so->id, s.gid);
		    }
		    else {

                        char name[Q_FUN_NAME_LEN];
			sk_function_t fun;

                    	if (strncpy_from_user(name, s.name, Q_FUN_NAME_LEN-1) < 0)
				return -EFAULT;

			name[Q_FUN_NAME_LEN-1] = '\0';

			fun = pfq_get_function(name);
			if (fun == NULL) {
                    		pr_devel("[PFQ|%d] function error: gid:%d '%s' unknown function!\n", so->id, s.gid, name);
				return -EINVAL;
			}

			if (__pfq_set_group_function(s.gid, fun, s.level) < 0) {
                    		pr_devel("[PFQ|%d] function error: gid:%d invalid level (%d)!\n", so->id, s.gid, s.level);
			        return -EINVAL;
                        }

			pr_devel("[PFQ|%d] function gid:%d -> function '%s'\n", so->id, s.gid, name);
		    }
	    } break;

	case Q_SO_GROUP_FPROG:
	    {
		    struct pfq_fprog fprog;
		    if (optlen != sizeof(fprog))
			    return -EINVAL;

		    if (copy_from_user(&fprog, optval, optlen))
			    return -EFAULT;

                    CHECK_GROUP_ACCES(fprog.gid, "group fprog");

                    if (fprog.fcode.len > 0)  /* set the filter */
		    {
			struct sk_filter *filter = pfq_alloc_sk_filter(&fprog.fcode);
		 	if (filter == NULL)
			{
                    	    pr_devel("[PFQ|%d] fprog error: alloc_sk_filter for gid:%d\n", so->id, fprog.gid);
			    return -EINVAL;
			}

			__pfq_set_group_filter(fprog.gid, filter);

			pr_devel("[PFQ|%d] fprog: gid:%d (fprog len %d bytes)\n", so->id, fprog.gid, fprog.fcode.len);
		    }
		    else 	/* reset the filter */
		    {
			__pfq_set_group_filter(fprog.gid, NULL);

			pr_devel("[PFQ|%d] fprog: gid:%d (resetting filter)\n", so->id, fprog.gid);
		    }

	    } break;

        case Q_SO_GROUP_VLAN_FILT_TOGGLE:
            {
		    struct pfq_vlan_toggle vlan;

		    if (optlen != sizeof(vlan))
			    return -EINVAL;

		    if (copy_from_user(&vlan, optval, optlen))
			    return -EFAULT;

                    CHECK_GROUP_ACCES(vlan.gid, "group vlan filt toggle");

		    __pfq_toggle_group_vlan_filters(vlan.gid, vlan.toggle);

                    pr_devel("[PFQ|%d] vlan filters %s for gid:%d\n", so->id, (vlan.toggle ? "enabled" : "disabled"), vlan.gid);
            } break;

        case Q_SO_GROUP_VLAN_FILT:
            {
		    struct pfq_vlan_toggle filt;

		    if (optlen != sizeof(filt))
			    return -EINVAL;

		    if (copy_from_user(&filt, optval, optlen))
			    return -EFAULT;

                    CHECK_GROUP_ACCES(filt.gid, "group vlan filt");

                    if (filt.vid < -1 || filt.vid > 4094) {
                    	    pr_devel("[PFQ|%d] vlan_set error: gid:%d invalid vid:%d!\n", so->id, filt.gid, filt.vid);
			    return -EINVAL;
                    }

                    if (!__pfq_vlan_filters_enabled(filt.gid)) {
                    	    pr_devel("[PFQ|%d] vlan_set error: vlan filters disabled for gid:%d!\n", so->id, filt.gid);
			    return -EPERM;
                    }

                    if (filt.vid  == -1) /* any */
                    {
                        int i;
                        for(i = 1; i < 4095; i++)
                                __pfq_set_group_vlan_filter(filt.gid, filt.toggle, i);
                    }
                    else
                    {
                        __pfq_set_group_vlan_filter(filt.gid, filt.toggle, filt.vid);
                    }

                    pr_devel("[PFQ|%d] vlan_set filter vid %d for gid:%d\n", so->id, filt.vid, filt.gid);
            } break;

        case Q_SO_TX_THREAD_BIND:
        {
                struct pfq_binding info;

                if (optlen != sizeof(info))
                        return -EINVAL;
                if (copy_from_user(&info, optval, optlen))
                        return -EFAULT;

                to->if_index = info.if_index;
                to->hw_queue = info.hw_queue;

                pr_devel("[PFQ|%d] TX bind: if_index:%d hw_queue:%d\n", so->id, to->if_index, to->hw_queue);

        } break;

        case Q_SO_TX_THREAD_START:
        {
                int node;

                pr_devel("[PFQ|%d] starting TX thread...\n", so->id);

                if (to->thread)
                {
                        pr_devel("[PFQ|%d] TX thread already created on node %d!\n", so->id, to->cpu_index);
                        return -EPERM;
                }
                if (to->if_index == -1)
                {
                        pr_devel("[PFQ|%d] socket TX not bound to any device!\n", so->id);
                        return -EPERM;
                }
                if (to->queue_info == NULL)
                {
                        pr_devel("[PFQ|%d] socket not enabled!\n", so->id);
                        return -EPERM;
                }

                if (optlen != sizeof(node))
                        return -EINVAL;

                if (copy_from_user(&node, optval, optlen))
                        return -EFAULT;

                to->cpu_index = node;

                pr_devel("[PFQ|%d] creating TX thread on node %d -> if_index:%d hw_queue:%d\n", so->id, to->cpu_index, to->if_index, to->hw_queue);

                to->thread = kthread_create_on_node(pfq_tx_thread,
                                                    so,
                                                    cpu_to_node(to->cpu_index),
                                                    "pfq_tx_%d", to->cpu_index);

                if (IS_ERR(to->thread)) {
                        printk(KERN_INFO "[PFQ] kernel_thread() create failed on node %d!\n", to->cpu_index);
                        return PTR_ERR(to->thread);
                }

                kthread_bind(to->thread, to->cpu_index);

                pr_devel("[PFQ|%d] starting TX thread: done.\n", so->id);

        } break;

        case Q_SO_TX_THREAD_STOP:
        {
                pr_devel("[PFQ|%d] stopping TX thread...\n", so->id);

                if (!to->thread)
                {
                        pr_devel("[PFQ|%d] TX thread not running!\n", so->id);
                        return -EPERM;
                }

                if (to->thread)
                {
                        kthread_stop(to->thread);
                        to->thread = NULL;
                }

                pr_devel("[PFQ|%d] stop TX thread: done.\n", so->id);

        } break;

        case Q_SO_TX_THREAD_WAKEUP:
        {
                if (to->if_index == -1)
                {
                        pr_devel("[PFQ|%d] socket TX not bound to any device!\n", so->id);
                        return -EPERM;
                }
                if (!to->thread)
                {
                        pr_devel("[PFQ|%d] TX thread not running!\n", so->id);
                        return -EPERM;
                }

                wake_up_process(to->thread);
        } break;

        case Q_SO_TX_QUEUE_FLUSH:
        {
                struct net_device *dev;

                if (to->if_index == -1)
                {
                        pr_devel("[PFQ|%d] socket TX not bound to any device!\n", so->id);
                        return -EPERM;
                }

                if (to->thread && to->thread->state == TASK_RUNNING)
                {
                        pr_devel("[PFQ|%d] TX thread is running!\n", so->id);
                        return -EPERM;
                }

                if (to->queue_info == NULL)
                {
                        pr_devel("[PFQ|%d] socket not enabled!\n", so->id);
                        return -EPERM;
                }

                dev = dev_get_by_index(sock_net(&so->sk), to->if_index);
                if (!dev)
                {
                        pr_devel("[PFQ|%d] No such device (if_index = %d)\n", so->id, to->if_index);
                        return -EPERM;
                }

                pfq_tx_queue_flush(to, dev);
                dev_put(dev);
        } break;

        default:
        {
                found = false;
        } break;

        }

        return found ? 0 : sock_setsockopt(sock, level, optname, optval, optlen);
}

