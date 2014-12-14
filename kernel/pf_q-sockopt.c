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

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/version.h>

#include <linux/kthread.h>
#include <linux/pf_q.h>

#include <pf_q-transmit.h>
#include <pf_q-thread.h>
#include <pf_q-global.h>
#include <pf_q-sock.h>
#include <pf_q-group.h>
#include <pf_q-memory.h>
#include <pf_q-devmap.h>
#include <pf_q-symtable.h>
#include <pf_q-engine.h>
#include <pf_q-sockopt.h>
#include <pf_q-endpoint.h>
#include <pf_q-mpdb-queue.h>


int pfq_getsockopt(struct socket *sock,
                int level, int optname,
                char __user * optval, int __user * optlen)
{
        struct pfq_sock *so = pfq_sk(sock->sk);
        int len;

        if (so == NULL)
                return -EFAULT;

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

                if (copy_from_user(&group, optval, sizeof(group)))
                        return -EFAULT;

                if (group.class_mask == 0) {
                        pr_devel("[PFQ|%d] join error: bad class_mask (%lx)!\n", so->id, group.class_mask);
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
                	int err = pfq_check_group(so->id, group.gid, "group join");
			if (err != 0)
				return err;

                        if (pfq_join_group(group.gid, so->id, group.class_mask, group.policy) < 0) {
                                pr_devel("[PFQ|%d] join error: permission denied (gid=%d)!\n", so->id, group.gid);
                                return -EACCES;
                        }
                }

                pr_devel("[PFQ|%d] join: gid=%d class_mask=%lx\n", so->id, group.gid, group.class_mask);
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

                enabled = so->shmem_addr == NULL ? 0 : 1;

                if (copy_to_user(optval, &enabled, sizeof(enabled)))
                        return -EFAULT;

        } break;

        case Q_SO_GET_STATS:
        {
                struct pfq_stats stat;
                if (len != sizeof(struct pfq_stats))
                        return -EINVAL;

                stat.recv = sparse_read(&so->rx_opt.stats.recv);
                stat.lost = sparse_read(&so->rx_opt.stats.lost);
                stat.drop = sparse_read(&so->rx_opt.stats.drop);

		stat.frwd = 0;
		stat.kern = 0;

                stat.sent = sparse_read(&so->tx_opt.stats.sent);
                stat.disc = sparse_read(&so->tx_opt.stats.disc);

                if (copy_to_user(optval, &stat, sizeof(stat)))
                        return -EFAULT;
        } break;

        case Q_SO_GET_RX_TSTAMP:
        {
                if (len != sizeof(so->rx_opt.tstamp))
                        return -EINVAL;
                if (copy_to_user(optval, &so->rx_opt.tstamp, sizeof(so->rx_opt.tstamp)))
                        return -EFAULT;
        } break;

        case Q_SO_GET_SHARED_MEM:
        {
                if (len != sizeof(so->shmem_size))
                        return -EINVAL;

                if (copy_to_user(optval, &so->shmem_size, sizeof(so->shmem_size)))
                        return -EFAULT;
        } break;

        case Q_SO_GET_RX_CAPLEN:
        {
                if (len != sizeof(so->rx_opt.caplen))
                        return -EINVAL;
                if (copy_to_user(optval, &so->rx_opt.caplen, sizeof(so->rx_opt.caplen)))
                        return -EFAULT;
        } break;

        case Q_SO_GET_TX_MAXLEN:
        {
                if (len != sizeof(so->tx_opt.maxlen))
                        return -EINVAL;
                if (copy_to_user(optval, &so->tx_opt.maxlen, sizeof(so->tx_opt.maxlen)))
                        return -EFAULT;
        } break;

        case Q_SO_GET_RX_SLOTS:
        {
                if (len != sizeof(so->rx_opt.queue_size))
                        return -EINVAL;
                if (copy_to_user(optval, &so->rx_opt.queue_size, sizeof(so->rx_opt.queue_size)))
                        return -EFAULT;
        } break;

        case Q_SO_GET_TX_SLOTS:
        {
                if (len != sizeof(so->tx_opt.queue_size))
                        return -EINVAL;
                if (copy_to_user(optval, &so->tx_opt.queue_size, sizeof(so->tx_opt.queue_size)))
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
                struct pfq_group *g;
                struct pfq_stats stat;
                int err, gid;

                if (len != sizeof(stat))
                        return -EINVAL;

                if (copy_from_user(&stat, optval, sizeof(stat)))
                        return -EFAULT;

                gid = (int)stat.recv;

                err = pfq_check_group(so->id, gid, "group stat");
                if (err != 0)
                	return err;

                g = pfq_get_group(gid);
                if (!g) {
                        pr_devel("[PFQ|%d] group error: invalid group id %d!\n", so->id, gid);
                        return -EFAULT;
                }

                /* check whether the group is joinable.. */

                if (!__pfq_group_access(gid, so->id, Q_POLICY_GROUP_UNDEFINED, false)) {
                        pr_devel("[PFQ|%d] group stats error: permission denied (gid=%d)!\n", so->id, gid);
                        return -EACCES;
                }

                stat.recv = sparse_read(&g->stats.recv);
                stat.drop = sparse_read(&g->stats.drop);
                stat.frwd = sparse_read(&g->stats.frwd);
                stat.kern = sparse_read(&g->stats.kern);

                stat.lost = 0;
                stat.sent = 0;
                stat.disc = 0;

                if (copy_to_user(optval, &stat, sizeof(stat)))
                        return -EFAULT;
        } break;

        case Q_SO_GET_GROUP_COUNTERS:
        {
                struct pfq_group *g;
                struct pfq_counters cs;
                int i, err, gid;

                if (len != sizeof(cs))
                        return -EINVAL;

                if (copy_from_user(&cs, optval, sizeof(cs)))
                        return -EFAULT;

                gid = (int)cs.counter[0];

                err = pfq_check_group(so->id, gid, "group stat");
                if (err != 0)
                	return err;

                g = pfq_get_group(gid);
                if (!g) {
                        pr_devel("[PFQ|%d] group error: invalid group id %d!\n", so->id, gid);
                        return -EFAULT;
                }

                /* check whether the group is joinable.. */

                if (!__pfq_group_access(gid, so->id, Q_POLICY_GROUP_UNDEFINED, false)) {
                        pr_devel("[PFQ|%d] group error: permission denied (gid=%d)!\n", so->id, gid);
                        return -EACCES;
                }

                for(i = 0; i < Q_MAX_COUNTERS; i++)
                {
                        cs.counter[i] = sparse_read(&g->context.counter[i]);
                }

                if (copy_to_user(optval, &cs, sizeof(cs)))
                        return -EFAULT;
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

        bool found = true;

        if (so == NULL)
                return -EINVAL;

        switch(optname)
        {
        case Q_SO_ENABLE:
	{
		int err = 0;
                size_t n;

                err = pfq_shared_queue_enable(so);
                if (err < 0) {
                        pr_devel("[PFQ|%d] enable error!\n", so->id);
                        return err;
                }

		/* start tx kernel threads */

		for(n = 0; n < Q_MAX_TX_QUEUES; n++)
		{
			struct pfq_thread_data *data;
                       	int node;

			if (so->tx_opt.queue[n].if_index == -1)
				break;

			if (so->tx_opt.queue[n].cpu == Q_NO_KTHREAD)
				continue;

			data = kmalloc(sizeof(struct pfq_thread_data), GFP_KERNEL);
			if (!data) {
				printk(KERN_INFO "[PFQ|%d] kernel_thread: could not allocate thread_data! Failed starting thread on cpu %d!\n",
						so->id, so->tx_opt.queue[n].cpu);
				err = -EPERM;
				break;
			}

			data->so = so;
			data->id = n;
			node     = cpu_online(so->tx_opt.queue[n].cpu) ? cpu_to_node(so->tx_opt.queue[n].cpu) : NUMA_NO_NODE;

			pr_devel("[PFQ|%d] creating TX[%zu] thread on cpu %d: if_index=%d hw_queue=%d\n",
					so->id, n, so->tx_opt.queue[n].cpu, so->tx_opt.queue[n].if_index, so->tx_opt.queue[n].hw_queue);

			so->tx_opt.queue[n].task = kthread_create_on_node(pfq_tx_thread, data, node, "pfq_tx_%d#%zu", so->id, n);

			if (IS_ERR(so->tx_opt.queue[n].task)) {
				printk(KERN_INFO "[PFQ|%d] kernel_thread: create failed on cpu %d!\n", so->id, so->tx_opt.queue[n].cpu);
				err = PTR_ERR(so->tx_opt.queue[n].task);
				so->tx_opt.queue[n].task = NULL;
				kfree (data);
				break;
			}

			if (so->tx_opt.queue[n].cpu != -1)
				kthread_bind(so->tx_opt.queue[n].task, so->tx_opt.queue[n].cpu);

			wake_up_process(so->tx_opt.queue[n].task);
		}

		if (err)
			return err;

	} break;

	case Q_SO_DISABLE:
	{
		int err = 0;
                size_t n;

		for(n = 0; n < so->tx_opt.num_queues; n++)
		{
			if (so->tx_opt.queue[n].task) {
				pr_devel("[PFQ|%d] stopping TX[%zu] thread@%p\n", so->id, n, so->tx_opt.queue[n].task);
				kthread_stop(so->tx_opt.queue[n].task);
				so->tx_opt.queue[n].task = NULL;
			}
		}

                err = pfq_shared_queue_disable(so);
                if (err < 0) {
                        pr_devel("[PFQ|%d] disable error!\n", so->id);
                        return err;
                }

	} break;

        case Q_SO_GROUP_BIND:
        {
                struct pfq_binding bind;
                int err;

                if (optlen != sizeof(struct pfq_binding))
                        return -EINVAL;

                if (copy_from_user(&bind, optval, optlen))
                        return -EFAULT;

                err = pfq_check_group_access(so->id, bind.gid, "add binding");
                if (err != 0)
                	return err;

                rcu_read_lock();
                if (!dev_get_by_index_rcu(sock_net(&so->sk), bind.if_index))
                {
                        rcu_read_unlock();
                        pr_devel("[PFQ|%d] bind: invalid if_index=%d\n", so->id, bind.if_index);
                        return -EPERM;
                }
                rcu_read_unlock();

                pfq_devmap_update(map_set, bind.if_index, bind.hw_queue, bind.gid);

        } break;

        case Q_SO_GROUP_UNBIND:
        {
                struct pfq_binding bind;
                int err;

                if (optlen != sizeof(struct pfq_binding))
                        return -EINVAL;

                if (copy_from_user(&bind, optval, optlen))
                        return -EFAULT;

                err = pfq_check_group_access(so->id, bind.gid, "remove binding");
                if (err != 0)
                	return err;

                rcu_read_lock();
                if (!dev_get_by_index_rcu(sock_net(&so->sk), bind.if_index))
                {
                        rcu_read_unlock();
                        pr_devel("[PFQ|%d] unbind: invalid if_index=%d\n", so->id, bind.if_index);
                        return -EPERM;
                }
                rcu_read_unlock();

                pfq_devmap_update(map_reset, bind.if_index, bind.hw_queue, bind.gid);

        } break;

        case Q_SO_EGRESS_BIND:
        {
                struct pfq_binding info;

                if (optlen != sizeof(info))
                        return -EINVAL;
                if (copy_from_user(&info, optval, optlen))
                        return -EFAULT;

                rcu_read_lock();
                if (!dev_get_by_index_rcu(sock_net(&so->sk), info.if_index))
                {
                        rcu_read_unlock();
                        pr_devel("[PFQ|%d] egress bind: invalid if_index=%d\n", so->id, info.if_index);
                        return -EPERM;
                }
                rcu_read_unlock();

                if (info.hw_queue < -1)
                {
                        pr_devel("[PFQ|%d] egress bind: invalid queue=%d\n", so->id, info.hw_queue);
                        return -EPERM;
                }

		so->egress_type  = pfq_endpoint_device;
                so->egress_index = info.if_index;
                so->egress_queue = info.hw_queue;

                pr_devel("[PFQ|%d] egress bind: device if_index=%d hw_queue=%d\n", so->id, so->egress_index, so->egress_queue);

        } break;

        case Q_SO_EGRESS_UNBIND:
        {
		so->egress_type  = pfq_endpoint_socket;
                so->egress_index = 0;
                so->egress_queue = 0;
                pr_devel("[PFQ|%d] egress unbind.\n", so->id);

        } break;

        case Q_SO_SET_RX_TSTAMP:
        {
                int tstamp;
                if (optlen != sizeof(so->rx_opt.tstamp))
                        return -EINVAL;

                if (copy_from_user(&tstamp, optval, optlen))
                        return -EFAULT;

                tstamp = tstamp ? 1 : 0;

                so->rx_opt.tstamp = tstamp;

                pr_devel("[PFQ|%d] timestamp enabled.\n", so->id);
        } break;

        case Q_SO_SET_RX_CAPLEN:
        {
                typeof(so->rx_opt.caplen) caplen;

                if (optlen != sizeof(caplen))
                        return -EINVAL;
                if (copy_from_user(&caplen, optval, optlen))
                        return -EFAULT;

                if (caplen > (size_t)cap_len) {
                        pr_devel("[PFQ|%d] invalid caplen=%zu (max %d)\n", so->id, caplen, cap_len);
                        return -EPERM;
                }

                so->rx_opt.caplen = caplen;

                so->rx_opt.slot_size = MPDB_QUEUE_SLOT_SIZE(so->rx_opt.caplen);

                pr_devel("[PFQ|%d] caplen=%zu, slot_size=%zu\n",
                                so->id, so->rx_opt.caplen, so->rx_opt.slot_size);
        } break;

        case Q_SO_SET_RX_SLOTS:
        {
                typeof(so->rx_opt.queue_size) slots;

                if (optlen != sizeof(slots))
                        return -EINVAL;

                if (copy_from_user(&slots, optval, optlen))
                        return -EFAULT;

                if (slots > (size_t)max_queue_slots) {
                        pr_devel("[PFQ|%d] invalid rx slots=%zu (max %d)\n", so->id, slots, max_queue_slots);
                        return -EPERM;
                }

                so->rx_opt.queue_size = slots;

                pr_devel("[PFQ|%d] rx_queue slots=%zu\n", so->id, so->rx_opt.queue_size);
        } break;

        case Q_SO_SET_TX_MAXLEN:
        {
                typeof (so->tx_opt.maxlen) maxlen;

                if (optlen != sizeof(maxlen))
                        return -EINVAL;
                if (copy_from_user(&maxlen, optval, optlen))
                        return -EFAULT;

                if (maxlen > (size_t)max_len) {
                        pr_devel("[PFQ|%d] invalid maxlen=%zu (max %d)\n", so->id, maxlen, max_len);
                        return -EPERM;
                }

                so->tx_opt.maxlen = maxlen < 64 ? 64 : maxlen;

                so->tx_opt.slot_size = SPSC_QUEUE_SLOT_SIZE(so->tx_opt.maxlen); /* max_len: max length */

                pr_devel("[PFQ|%d] tx_maxlen=%zu, tx_slot_size=%zu\n", so->id, so->tx_opt.maxlen, so->tx_opt.slot_size);

        } break;

        case Q_SO_SET_TX_SLOTS:
        {
                typeof (so->tx_opt.queue_size) slots;

                if (optlen != sizeof(slots))
                        return -EINVAL;
                if (copy_from_user(&slots, optval, optlen))
                        return -EFAULT;

                if (slots & (slots-1)) {
                        pr_devel("[PFQ|%d] TX slots must be a power of two.\n", so->id);
                        return -EINVAL;
                }

                if (slots > (size_t)max_queue_slots) {
                        pr_devel("[PFQ|%d] invalid TX slots=%zu (max %d)\n", so->id, slots, max_queue_slots);
                        return -EPERM;
                }

                so->tx_opt.queue_size = slots;

                pr_devel("[PFQ|%d] tx_queue slots=%zu\n", so->id, so->tx_opt.queue_size);
        } break;

        case Q_SO_GROUP_LEAVE:
        {
                int gid;

                if (optlen != sizeof(gid))
                        return -EINVAL;

                if (copy_from_user(&gid, optval, optlen))
                        return -EFAULT;

                if (pfq_leave_group(gid, so->id) < 0)
                        return -EFAULT;

                pr_devel("[PFQ|%d] leave: gid=%d\n", so->id, gid);

        } break;

        case Q_SO_GROUP_FPROG:
        {
                struct pfq_fprog fprog;
                int err;

                if (optlen != sizeof(fprog))
                        return -EINVAL;

                if (copy_from_user(&fprog, optval, optlen))
                        return -EFAULT;

                err = pfq_check_group_access(so->id, fprog.gid, "group fprog");
                if (err == -EPERM)
                	return 0;

                if (fprog.fcode.len > 0)  /* set the filter */
                {
                        struct sk_filter *filter;

			if (fprog.fcode.len == 1) /* check for dummey BPF_CLASS == BPF_RET */
			{
                       	 	if (BPF_CLASS(fprog.fcode.filter[0].code) == BPF_RET) {
                                	pr_devel("[PFQ|%d] fprog: BPF_RET optimized out!\n", so->id);
                                	return 0;
				}
			}

                        filter = pfq_alloc_sk_filter(&fprog.fcode);
                        if (filter == NULL)
                        {
                                pr_devel("[PFQ|%d] fprog error: alloc_sk_filter for gid=%d\n", so->id, fprog.gid);
                                return -EINVAL;
                        }

			if (fprog.fcode.len == 1) {

			}

                        __pfq_set_group_filter(fprog.gid, filter);

                        pr_devel("[PFQ|%d] fprog: gid=%d (fprog len %d bytes)\n", so->id, fprog.gid, fprog.fcode.len);
                }
                else 	/* reset the filter */
                {
                        __pfq_set_group_filter(fprog.gid, NULL);

                        pr_devel("[PFQ|%d] fprog: gid=%d (resetting filter)\n", so->id, fprog.gid);
                }

        } break;

        case Q_SO_GROUP_VLAN_FILT_TOGGLE:
        {
                struct pfq_vlan_toggle vlan;
                int err;

                if (optlen != sizeof(vlan))
                        return -EINVAL;

                if (copy_from_user(&vlan, optval, optlen))
                        return -EFAULT;

                err = pfq_check_group_access(so->id, vlan.gid, "group vlan filt toggle");
                if (err != 0)
                	return err;

                __pfq_toggle_group_vlan_filters(vlan.gid, vlan.toggle);

                pr_devel("[PFQ|%d] vlan filters %s for gid=%d\n", so->id, (vlan.toggle ? "enabled" : "disabled"), vlan.gid);
        } break;

        case Q_SO_GROUP_VLAN_FILT:
        {
                struct pfq_vlan_toggle filt;
                int err;

                if (optlen != sizeof(filt))
                        return -EINVAL;

                if (copy_from_user(&filt, optval, optlen))
                        return -EFAULT;

                err = pfq_check_group_access(so->id, filt.gid, "group vlan filt");
                if (err != 0)
                	return err;

                if (filt.vid < -1 || filt.vid > 4094) {
                        pr_devel("[PFQ|%d] vlan_set error: gid=%d invalid vid=%d!\n", so->id, filt.gid, filt.vid);
                        return -EINVAL;
                }

                if (!__pfq_vlan_filters_enabled(filt.gid)) {
                        pr_devel("[PFQ|%d] vlan_set error: vlan filters disabled for gid=%d!\n", so->id, filt.gid);
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

                pr_devel("[PFQ|%d] vlan_set filter vid %d for gid=%d\n", so->id, filt.vid, filt.gid);
        } break;

        case Q_SO_TX_BIND:
        {
                struct pfq_binding info;
                size_t i;

                if (optlen != sizeof(info))
                        return -EINVAL;

                if (copy_from_user(&info, optval, optlen))
                        return -EFAULT;

		if (so->tx_opt.num_queues >= Q_MAX_TX_QUEUES) {
                        pr_devel("[PFQ|%d] TX bind: max number of queues exceeded!\n", so->id);
			return -EPERM;
		}

                rcu_read_lock();
                if (!dev_get_by_index_rcu(sock_net(&so->sk), info.if_index))
                {
                        rcu_read_unlock();
                        pr_devel("[PFQ|%d] TX bind: invalid if_index=%d\n", so->id, info.if_index);
                        return -EPERM;
                }
                rcu_read_unlock();

                if (info.hw_queue < -1)
                {
                        pr_devel("[PFQ|%d] TX bind: invalid queue=%d\n", so->id, info.hw_queue);
                        return -EPERM;
                }

                i = so->tx_opt.num_queues;

		if (info.cpu < -1) {
			pr_devel("[PFQ|%d] TX[%zu] thread: invalid cpu (%d)!\n", so->id, i, info.cpu);
			return -EPERM;
		}

                so->tx_opt.queue[i].if_index = info.if_index;
                so->tx_opt.queue[i].hw_queue = info.hw_queue;
                so->tx_opt.queue[i].cpu      = info.cpu;

		so->tx_opt.num_queues++;

                pr_devel("[PFQ|%d] TX[%zu] bind: if_index=%d hw_queue=%d\n", so->id, i,
                		so->tx_opt.queue[i].if_index, so->tx_opt.queue[i].hw_queue);

        } break;

	case Q_SO_TX_UNBIND:
        {
        	size_t n;

         	for(n = 0; n < Q_MAX_TX_QUEUES; ++n)
		{
			so->tx_opt.queue[n].if_index = -1;
			so->tx_opt.queue[n].hw_queue = -1;
			so->tx_opt.queue[n].cpu      = -1;
		}

        } break;

        case Q_SO_TX_FLUSH:
        {
		int queue, err = 0;
                size_t n;

        	if (optlen != sizeof(queue))
        		return -EINVAL;
        	if (copy_from_user(&queue, optval, optlen))
        		return -EFAULT;

		if (pfq_get_tx_queue_hdr(&so->tx_opt, 0) == NULL) {
			pr_devel("[PFQ|%d] TX queue flush: socket not enabled!\n", so->id);
			return -EPERM;
		}

		if (queue < -1 || (queue > 0 && queue >= so->tx_opt.num_queues)) {
			pr_devel("[PFQ|%d] TX queue flush: bad queue %d (num_queue=%zu)!\n", so->id, queue, so->tx_opt.num_queues);
			return -EPERM;
		}

		if (queue != -1) {
			pr_devel("[PFQ|%d] flushing TX queue %d...\n", so->id, queue);
			return pfq_queue_flush_or_wakeup(so, queue);
		}

		for(n = 0; n < so->tx_opt.num_queues; n++)
		{
			if (pfq_queue_flush_or_wakeup(so, n) != 0) {
				pr_devel("[PFQ|%d] TX[%zu] queue flush: flush error (if_index=%d)!\n", so->id, n, so->tx_opt.queue[n].if_index);
				err = -EPERM;
			}
		}

		if (err)
			return err;
        } break;

        case Q_SO_GROUP_FUNCTION:
        {
                struct pfq_computation_descr *descr = NULL;
                struct pfq_group_computation tmp;
                size_t psize, ucsize;
                int err = 0;

                struct pfq_computation_tree *comp = NULL;
                void *context = NULL;

                if (optlen != sizeof(tmp))
                        return -EINVAL;
                if (copy_from_user(&tmp, optval, optlen))
                        return -EFAULT;

                err = pfq_check_group_access(so->id, tmp.gid, "group computation");
                if (err != 0)
                	return err;

                if (copy_from_user(&psize, tmp.prog, sizeof(size_t)))
                        return -EFAULT;

                pr_devel("[PFQ|%d] computation size: %zu\n", so->id, psize);

                ucsize = sizeof(size_t) * 2 + psize * sizeof(struct pfq_functional_descr);

                descr = kmalloc(ucsize, GFP_KERNEL);
                if (descr == NULL) {
                        pr_devel("[PFQ|%d] computation: out of memory!\n", so->id);
                        return -ENOMEM;
                }

                if (copy_from_user(descr, tmp.prog, ucsize)) {
                        pr_devel("[PFQ|%d] computation: copy_from_user error!\n", so->id);
                        err = -EFAULT;
                        goto error;
                }

                /* print user computation */

                pr_devel_computation_descr(descr);

		/* ensure the correctness of the specified functional computation */

		if (pfq_validate_computation_descr(descr) < 0) {
                        pr_devel("[PFQ|%d] invalid expression!\n", so->id);
                        err = -EFAULT;
                        goto error;
		}

                /* allocate context */

                context = pfq_context_alloc(descr);
                if (context == NULL) {
                        pr_devel("[PFQ|%d] context: alloc error!\n", so->id);
                        err = -EFAULT;
                        goto error;
                }

                /* allocate struct pfq_computation_tree */

                comp = pfq_computation_alloc(descr);
                if (comp == NULL) {
                        pr_devel("[PFQ|%d] computation: alloc error!\n", so->id);
                        err = -EFAULT;
                        goto error;
                }

                /* link the functional computation */

                if (pfq_computation_rtlink(descr, comp, context) < 0) {
                        pr_devel("[PFQ|%d] computation aborted!", so->id);
                        err = -EPERM;
                        goto error;
                }

		/* print executable tree data structure */

		pr_devel_computation_tree(comp);

		/* exec init functions */

		if (pfq_computation_init(comp) < 0) {
                        pr_devel("[PFQ|%d] initialization of computation aborted!", so->id);
                        pfq_computation_fini(comp);
                        err = -EPERM;
                        goto error;
		}

                /* set the new program */

                if (pfq_set_group_prog(tmp.gid, comp, context) < 0) {
                        pr_devel("[PFQ|%d] set group program error!\n", so->id);
                        err = -EPERM;
                        goto error;
                }

		kfree(descr);
                return 0;

	error:  kfree(comp);
		kfree(context);
		kfree(descr);
		return err;

        } break;

        default:
        {
                found = false;
        } break;

        }

        return found ? 0 : sock_setsockopt(sock, level, optname, optval, optlen);
}

