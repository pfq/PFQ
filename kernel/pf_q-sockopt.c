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

#include <pragma/diagnostic_push>

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/version.h>
#include <linux/kthread.h>
#include <linux/pf_q.h>

#include <pragma/diagnostic_pop>

#include <pf_q-transmit.h>
#include <pf_q-thread.h>
#include <pf_q-global.h>
#include <pf_q-sock.h>
#include <pf_q-group.h>
#include <pf_q-memory.h>
#include <pf_q-devmap.h>
#include <pf_q-symtable.h>
#include <pf_q-engine.h>
#include <pf_q-printk.h>
#include <pf_q-thread.h>
#include <pf_q-sockopt.h>
#include <pf_q-endpoint.h>
#include <pf_q-shared-queue.h>

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
                        printk(KERN_INFO "[PFQ|%d] join error: bad class_mask (%lx)!\n",
                               so->id.value, group.class_mask);
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
			pfq_gid_t gid = { group.gid };

			if (!pfq_get_group(gid)) {
				printk(KERN_INFO "[PFQ|%d] group error: invalid group id %d!\n",
				       so->id.value, gid.value);
				return -EFAULT;
			}

                        if (pfq_join_group(gid, so->id, group.class_mask, group.policy) < 0) {
                                printk(KERN_INFO "[PFQ|%d] join error: permission denied (gid=%d)!\n",
                                       so->id.value, group.gid);
                                return -EACCES;
                        }
                }

                pr_devel("[PFQ|%d] join: gid=%d class_mask=%lx\n", so->id.value, group.gid, group.class_mask);
        } break;

        case Q_SO_GET_ID:
        {
                if (len != sizeof(so->id.value))
                        return -EINVAL;
                if (copy_to_user(optval, &so->id.value, sizeof(so->id.value)))
                        return -EFAULT;
        } break;

        case Q_SO_GET_STATUS:
        {
                int enabled;
                if (len != sizeof(int))
                        return -EINVAL;

                enabled = so->shmem.addr == NULL ? 0 : 1;

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

        case Q_SO_GET_SHMEM_SIZE:
	{
		size_t size = pfq_shared_memory_size(so);

                if (len != sizeof(size))
                        return -EINVAL;

                if (copy_to_user(optval, &size, sizeof(size)))
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
                if (len != sizeof(max_len))
                        return -EINVAL;
                if (copy_to_user(optval, &max_len, sizeof(max_len)))
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
                pfq_gid_t gid;

                if (len != sizeof(stat))
                        return -EINVAL;

                if (copy_from_user(&stat, optval, sizeof(stat)))
                        return -EFAULT;

                gid.value = (int)stat.recv;

                g = pfq_get_group(gid);
                if (g == NULL) {
                        printk(KERN_INFO "[PFQ|%d] group error: invalid group id %d!\n", so->id.value, gid.value);
                        return -EFAULT;
                }

		if (pfq_group_is_free(gid)) {
                        printk(KERN_INFO "[PFQ|%d] group stats error: gid=%d is a free group!\n",
                               so->id.value, gid.value);
                        return -EACCES;
		}

                if (!pfq_group_access(gid, so->id)) {
                        printk(KERN_INFO "[PFQ|%d] group stats error: gid=%d permission denied!\n",
                               so->id.value, gid.value);
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
                pfq_gid_t gid;
                int i;

                if (len != sizeof(cs))
                        return -EINVAL;

                if (copy_from_user(&cs, optval, sizeof(cs)))
                        return -EFAULT;

                gid.value = (int)cs.counter[0];

                g = pfq_get_group(gid);
                if (g == NULL) {
                        printk(KERN_INFO "[PFQ|%d] group error: invalid group id %d!\n", so->id.value, gid.value);
                        return -EFAULT;
                }

                /* check whether the group is joinable.. */

                if (!pfq_group_policy_access(gid, so->id, Q_POLICY_GROUP_UNDEFINED)) {
                        printk(KERN_INFO "[PFQ|%d] group error: permission denied (gid=%d)!\n",
                               so->id.value, gid.value);
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
		unsigned long addr;
		int err = 0;

                if (optlen != sizeof(addr))
                        return -EINVAL;

                if (copy_from_user(&addr, optval, optlen))
                        return -EFAULT;

                err = pfq_shared_queue_enable(so, addr);
                if (err < 0) {
                        printk(KERN_INFO "[PFQ|%d] enable error!\n", so->id.value);
                        return err;
                }

		return 0;

	} break;

	case Q_SO_DISABLE:
	{
		int err = 0;

		pfq_stop_all_tx_threads(so);

                err = pfq_shared_queue_disable(so);
                if (err < 0) {
                        printk(KERN_INFO "[PFQ|%d] disable error!\n", so->id.value);
                        return err;
                }

	} break;

        case Q_SO_GROUP_BIND:
        {
                struct pfq_binding bind;
		pfq_gid_t gid;

                if (optlen != sizeof(struct pfq_binding))
                        return -EINVAL;

                if (copy_from_user(&bind, optval, optlen))
                        return -EFAULT;

		gid.value = bind.gid;

                if (!pfq_has_joined_group(gid, so->id)) {
                        printk(KERN_INFO "[PFQ|%d] add bind: gid=%d not joined!\n", so->id.value, bind.gid);
			return -EACCES;
		}

                rcu_read_lock();
                if (!dev_get_by_index_rcu(sock_net(&so->sk), bind.if_index)) {
                        rcu_read_unlock();
                        printk(KERN_INFO "[PFQ|%d] bind: invalid if_index=%d!\n", so->id.value, bind.if_index);
                        return -EACCES;
                }
                rcu_read_unlock();

                pfq_devmap_update(map_set, bind.if_index, bind.hw_queue, gid);

        } break;

        case Q_SO_GROUP_UNBIND:
        {
                struct pfq_binding bind;
		pfq_gid_t gid;

                if (optlen != sizeof(struct pfq_binding))
                        return -EINVAL;

                if (copy_from_user(&bind, optval, optlen))
                        return -EFAULT;

		gid.value = bind.gid;

		if (!pfq_has_joined_group(gid, so->id)) {
                        printk(KERN_INFO "[PFQ|%d] remove bind: gid=%d not joined!\n", so->id.value, bind.gid);
			return -EACCES;
		}

                rcu_read_lock();
                if (!dev_get_by_index_rcu(sock_net(&so->sk), bind.if_index)) {
                        rcu_read_unlock();
                        printk(KERN_INFO "[PFQ|%d] unbind: invalid if_index=%d\n", so->id.value, bind.if_index);
                        return -EPERM;
                }
                rcu_read_unlock();

                pfq_devmap_update(map_reset, bind.if_index, bind.hw_queue, gid);

        } break;

        case Q_SO_EGRESS_BIND:
        {
                struct pfq_binding info;

                if (optlen != sizeof(info))
                        return -EINVAL;
                if (copy_from_user(&info, optval, optlen))
                        return -EFAULT;

                rcu_read_lock();
                if (!dev_get_by_index_rcu(sock_net(&so->sk), info.if_index)) {
                        rcu_read_unlock();
                        printk(KERN_INFO "[PFQ|%d] egress bind: invalid if_index=%d\n", so->id.value, info.if_index);
                        return -EPERM;
                }
                rcu_read_unlock();

                if (info.hw_queue < -1) {
                        printk(KERN_INFO "[PFQ|%d] egress bind: invalid queue=%d\n", so->id.value, info.hw_queue);
                        return -EPERM;
                }

		so->egress_type  = pfq_endpoint_device;
                so->egress_index = info.if_index;
                so->egress_queue = info.hw_queue;

                pr_devel("[PFQ|%d] egress bind: device if_index=%d hw_queue=%d\n",
			 so->id.value, so->egress_index, so->egress_queue);

        } break;

        case Q_SO_EGRESS_UNBIND:
        {
		so->egress_type  = pfq_endpoint_socket;
                so->egress_index = 0;
                so->egress_queue = 0;
                pr_devel("[PFQ|%d] egress unbind.\n", so->id.value);

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

                pr_devel("[PFQ|%d] timestamp enabled.\n", so->id.value);
        } break;

        case Q_SO_SET_RX_CAPLEN:
        {
                typeof(so->rx_opt.caplen) caplen;

                if (optlen != sizeof(caplen))
                        return -EINVAL;
                if (copy_from_user(&caplen, optval, optlen))
                        return -EFAULT;

                if (caplen > (size_t)cap_len) {
                        printk(KERN_INFO "[PFQ|%d] invalid caplen=%zu (max %d)\n", so->id.value, caplen, cap_len);
                        return -EPERM;
                }

                so->rx_opt.caplen = caplen;
                so->rx_opt.slot_size = Q_MPDB_QUEUE_SLOT_SIZE(so->rx_opt.caplen);

                pr_devel("[PFQ|%d] caplen=%zu, slot_size=%zu\n",
                                so->id.value, so->rx_opt.caplen, so->rx_opt.slot_size);
        } break;

        case Q_SO_SET_RX_SLOTS:
        {
                typeof(so->rx_opt.queue_size) slots;

                if (optlen != sizeof(slots))
                        return -EINVAL;

                if (copy_from_user(&slots, optval, optlen))
                        return -EFAULT;

                if (slots > (size_t)max_queue_slots) {
                        printk(KERN_INFO "[PFQ|%d] invalid Rx slots=%zu (max %d)\n",
                               so->id.value, slots, max_queue_slots);
                        return -EPERM;
                }

                so->rx_opt.queue_size = slots;

                pr_devel("[PFQ|%d] rx_queue slots=%zu\n", so->id.value, so->rx_opt.queue_size);
        } break;

        case Q_SO_SET_TX_SLOTS:
        {
                typeof (so->tx_opt.queue_size) slots;

                if (optlen != sizeof(slots))
                        return -EINVAL;
                if (copy_from_user(&slots, optval, optlen))
                        return -EFAULT;

                if (slots > (size_t)max_queue_slots) {
                        printk(KERN_INFO "[PFQ|%d] invalid Tx slots=%zu (max %d)\n",
                               so->id.value, slots, max_queue_slots);
                        return -EPERM;
                }

                so->tx_opt.queue_size = slots;

                pr_devel("[PFQ|%d] tx_queue slots=%zu\n", so->id.value, so->tx_opt.queue_size);
        } break;

        case Q_SO_GROUP_LEAVE:
        {
                pfq_gid_t gid;

                if (optlen != sizeof(gid.value))
                        return -EINVAL;

                if (copy_from_user(&gid.value, optval, optlen))
                        return -EFAULT;

                if (pfq_leave_group(gid, so->id) < 0)
                        return -EFAULT;

                pr_devel("[PFQ|%d] leave: gid=%d\n", so->id.value, gid.value);

        } break;

        case Q_SO_GROUP_FPROG:
        {
                struct pfq_fprog fprog;
		pfq_gid_t gid;

                if (optlen != sizeof(fprog))
                        return -EINVAL;

                if (copy_from_user(&fprog, optval, optlen))
                        return -EFAULT;

		gid.value = fprog.gid;

		if (!pfq_has_joined_group(gid, so->id)) {
			/* don't set the first and return */
			return 0;
		}

                if (fprog.fcode.len > 0) {  /* set the filter */

                        struct sk_filter *filter;

			if (fprog.fcode.len == 1) { /* check for dummey BPF_CLASS == BPF_RET */

				if (BPF_CLASS(fprog.fcode.filter[0].code) == BPF_RET) {
					pr_devel("[PFQ|%d] fprog: BPF_RET optimized out!\n", so->id.value);
					return 0;
				}
			}

                        filter = pfq_alloc_sk_filter(&fprog.fcode);
                        if (filter == NULL) {
                                printk(KERN_INFO "[PFQ|%d] fprog error: alloc_sk_filter for gid=%d\n",
                                       so->id.value, fprog.gid);
                                return -EINVAL;
                        }

                        pfq_set_group_filter(gid, filter);

                        pr_devel("[PFQ|%d] fprog: gid=%d (fprog len %d bytes)\n",
				 so->id.value, fprog.gid, fprog.fcode.len);
                }
                else {	/* reset the filter */

                        pfq_set_group_filter(gid, NULL);
                        pr_devel("[PFQ|%d] fprog: gid=%d (resetting filter)\n", so->id.value, fprog.gid);
                }

        } break;

        case Q_SO_GROUP_VLAN_FILT_TOGGLE:
        {
                struct pfq_vlan_toggle vlan;
                pfq_gid_t gid;

                if (optlen != sizeof(vlan))
                        return -EINVAL;

                if (copy_from_user(&vlan, optval, optlen))
                        return -EFAULT;

		gid.value = vlan.gid;

		if (!pfq_has_joined_group(gid, so->id)) {
                        printk(KERN_INFO "[PFQ|%d] vlan filter toggle: gid=%d not joined!\n", so->id.value, vlan.gid);
			return -EACCES;
		}

                pfq_toggle_group_vlan_filters(gid, vlan.toggle);
                pr_devel("[PFQ|%d] vlan filters %s for gid=%d\n",
			 so->id.value, (vlan.toggle ? "enabled" : "disabled"), vlan.gid);

        } break;

        case Q_SO_GROUP_VLAN_FILT:
        {
                struct pfq_vlan_toggle filt;
                pfq_gid_t gid;

                if (optlen != sizeof(filt))
                        return -EINVAL;

                if (copy_from_user(&filt, optval, optlen))
                        return -EFAULT;

		gid.value = filt.gid;

		if (!pfq_has_joined_group(gid, so->id)) {
                        printk(KERN_INFO "[PFQ|%d] vlan filter: gid=%d not joined!\n", so->id.value, filt.gid);
			return -EACCES;
		}

                if (filt.vid < -1 || filt.vid > 4094) {
                        printk(KERN_INFO "[PFQ|%d] vlan error: invalid vid=%d for gid=%d!\n",
                               so->id.value, filt.vid, filt.gid);
                        return -EINVAL;
                }

                if (!pfq_vlan_filters_enabled(gid)) {
                        printk(KERN_INFO "[PFQ|%d] vlan error: vlan filters disabled for gid=%d!\n",
                               so->id.value, filt.gid);
                        return -EPERM;
                }

                if (filt.vid  == -1) { /* any */
                        int i;
                        for(i = 1; i < 4095; i++)
			{
                                pfq_set_group_vlan_filter(gid, filt.toggle, i);
			}
                }
                else  {
                        pfq_set_group_vlan_filter(gid, filt.toggle, filt.vid);
		}

                pr_devel("[PFQ|%d] vlan filter vid %d set for gid=%d\n", so->id.value, filt.vid, filt.gid);
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
                        printk(KERN_INFO "[PFQ|%d] Tx bind: max number of queues exceeded!\n", so->id.value);
			return -EPERM;
		}

                rcu_read_lock();
                if (!dev_get_by_index_rcu(sock_net(&so->sk), info.if_index)) {
                        rcu_read_unlock();
                        printk(KERN_INFO "[PFQ|%d] Tx bind: invalid if_index=%d\n", so->id.value, info.if_index);
                        return -EPERM;
                }
                rcu_read_unlock();

                if (info.hw_queue < -1) {
                        printk(KERN_INFO "[PFQ|%d] Tx bind: invalid queue=%d\n", so->id.value, info.hw_queue);
                        return -EPERM;
                }

                i = so->tx_opt.num_queues;

		if (info.cpu < -1) {
			printk(KERN_INFO "[PFQ|%d] Tx[%zu] thread: invalid cpu (%d)!\n", so->id.value, i, info.cpu);
			return -EPERM;
		}

                so->tx_opt.queue[i].if_index = info.if_index;
                so->tx_opt.queue[i].hw_queue = info.hw_queue;
                so->tx_opt.queue[i].cpu      = info.cpu;

		so->tx_opt.num_queues++;

                pr_devel("[PFQ|%d] Tx[%zu] bind: if_index=%d hw_queue=%d cpu=%d\n", so->id.value, i,
			 so->tx_opt.queue[i].if_index, so->tx_opt.queue[i].hw_queue, info.cpu);

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

		if (pfq_get_tx_queue(&so->tx_opt, 0) == NULL) {
			printk(KERN_INFO "[PFQ|%d] Tx queue flush: socket not enabled!\n", so->id.value);
			return -EPERM;
		}

		if (queue < -1 || (queue > 0 && queue >= so->tx_opt.num_queues)) {
			printk(KERN_INFO "[PFQ|%d] Tx queue flush: bad queue %d (num_queue=%zu)!\n",
			       so->id.value, queue, so->tx_opt.num_queues);
			return -EPERM;
		}

		if (queue != -1) {
			pr_devel("[PFQ|%d] flushing Tx queue %d...\n", so->id.value, queue);
			return pfq_queue_flush(so, queue);
		}

		for(n = 0; n < so->tx_opt.num_queues; n++)
		{
			if (pfq_queue_flush(so, n) != 0) {
				printk(KERN_INFO "[PFQ|%d] Tx[%zu] queue flush: flush error (if_index=%d)!\n",
				       so->id.value, n, so->tx_opt.queue[n].if_index);
				err = -EPERM;
			}
		}

		if (err)
			return err;
        } break;

        case Q_SO_TX_ASYNC:
        {
                int toggle, err = 0;
                size_t n;

		if (optlen != sizeof(toggle))
			return -EINVAL;

		if (copy_from_user(&toggle, optval, optlen))
			return -EFAULT;

		printk(KERN_INFO "[PFQ|%d] Tx async %s!\n", so->id.value, toggle ? "enabled" : "disabled");

		if (toggle) {

			size_t started = 0;

			if (pfq_get_tx_queue(&so->tx_opt, 0) == NULL) {
				printk(KERN_INFO "[PFQ|%d] Tx queue flush: socket not enabled!\n", so->id.value);
				return -EPERM;
			}

			/* start Tx kernel threads */

			mutex_lock(&kthread_tx_pool_lock);

			for(n = 0; n < Q_MAX_TX_QUEUES; n++)
			{
				struct pfq_thread_data *data;
				int node;

				if (so->tx_opt.queue[n].if_index == -1)
					break;

				if (so->tx_opt.queue[n].cpu == Q_NO_KTHREAD) {
					printk(KERN_INFO "[PFQ|%d] kernel_thread: skipping queue %zu (no kthread).\n",
					       so->id.value, n);
					continue;
				}

				if (so->tx_opt.queue[n].task != NULL ||
				    kthread_tx_pool[so->tx_opt.queue[n].cpu % 256] != NULL) {
					printk(KERN_INFO "[PFQ|%d] kernel_thread: Tx[%zu] kthread already running (cpu = %d)!\n",
					       so->id.value, n,
					       so->tx_opt.queue[n].cpu);
					continue;
				}

				data = kmalloc(sizeof(struct pfq_thread_data), GFP_KERNEL);
				if (!data) {
					printk(KERN_INFO "[PFQ|%d] kernel_thread: could not allocate thread_data! Failed starting thread on cpu %d!\n",
							so->id.value, so->tx_opt.queue[n].cpu);
					err = -EPERM;
					continue;
				}

				data->so = so;
				data->id = n;
				node = cpu_online(so->tx_opt.queue[n].cpu) ?
				       cpu_to_node(so->tx_opt.queue[n].cpu) : NUMA_NO_NODE;

				pr_devel("[PFQ|%d] creating Tx[%zu] thread on cpu %d: if_index=%d hw_queue=%d\n",
						so->id.value, n, so->tx_opt.queue[n].cpu, so->tx_opt.queue[n].if_index,
						so->tx_opt.queue[n].hw_queue);

				so->tx_opt.queue[n].task = kthread_create_on_node(pfq_tx_thread, data, node, "pfq_tx_%d#%zu", so->id.value, n);

				if (IS_ERR(so->tx_opt.queue[n].task)) {
					printk(KERN_INFO "[PFQ|%d] kernel_thread: create failed on cpu %d!\n",
					       so->id.value, so->tx_opt.queue[n].cpu);
					err = PTR_ERR(so->tx_opt.queue[n].task);
					so->tx_opt.queue[n].task = NULL;
					kfree (data);
					continue;
				}

				/* update global tx pool */

				kthread_tx_pool[so->tx_opt.queue[n].cpu % 256] = so->tx_opt.queue[n].task;

				/* bind the thread */

				kthread_bind(so->tx_opt.queue[n].task, so->tx_opt.queue[n].cpu);

				/* start it */

				wake_up_process(so->tx_opt.queue[n].task);

				started++;
			}

			mutex_unlock(&kthread_tx_pool_lock);

			if (started == 0) {
				printk(KERN_INFO "[PFQ|%d] no kernel thread started!\n", so->id.value);
				err = -EPERM;
			}
		}
		else {
			/* stop running threads */

			pfq_stop_all_tx_threads(so);
		}

		return err;

        } break;

        case Q_SO_GROUP_FUNCTION:
        {
                struct pfq_computation_descr *descr = NULL;
                struct pfq_computation_tree *comp = NULL;
                struct pfq_group_computation tmp;
                size_t psize, ucsize;
                void *context = NULL;
                pfq_gid_t gid;
                int err = 0;

                if (optlen != sizeof(tmp))
                        return -EINVAL;

                if (copy_from_user(&tmp, optval, optlen))
                        return -EFAULT;

		gid.value = tmp.gid;

		if (!pfq_has_joined_group(gid, so->id)) {
                        printk(KERN_INFO "[PFQ|%d] group computation: gid=%d not joined!\n", so->id.value, tmp.gid);
			return -EACCES;
		}

                if (copy_from_user(&psize, tmp.prog, sizeof(size_t)))
                        return -EFAULT;

                pr_devel("[PFQ|%d] computation size: %zu\n", so->id.value, psize);

                ucsize = sizeof(size_t) * 2 + psize * sizeof(struct pfq_functional_descr);

                descr = kmalloc(ucsize, GFP_KERNEL);
                if (descr == NULL) {
                        printk(KERN_INFO "[PFQ|%d] computation: out of memory!\n", so->id.value);
                        return -ENOMEM;
                }

                if (copy_from_user(descr, tmp.prog, ucsize)) {
                        printk(KERN_INFO "[PFQ|%d] computation: copy_from_user error!\n", so->id.value);
                        err = -EFAULT;
                        goto error;
                }

                /* print user computation */

                pr_devel_computation_descr(descr);

		/* check the correctness of computation */

		if (pfq_check_computation_descr(descr) < 0) {
                        printk(KERN_INFO "[PFQ|%d] invalid expression!\n", so->id.value);
                        err = -EFAULT;
                        goto error;
		}

                /* allocate context */

                context = pfq_context_alloc(descr);
                if (context == NULL) {
                        printk(KERN_INFO "[PFQ|%d] context: alloc error!\n", so->id.value);
                        err = -EFAULT;
                        goto error;
                }

                /* allocate a pfq_computation_tree */

                comp = pfq_computation_alloc(descr);
                if (comp == NULL) {
                        printk(KERN_INFO "[PFQ|%d] computation: alloc error!\n", so->id.value);
                        err = -EFAULT;
                        goto error;
                }

                /* link functions of computation */

                if (pfq_computation_rtlink(descr, comp, context) < 0) {
                        printk(KERN_INFO "[PFQ|%d] computation aborted!", so->id.value);
                        err = -EPERM;
                        goto error;
                }

		/* print executable tree data structure */

		pr_devel_computation_tree(comp);

		/* run init functions */

		if (pfq_computation_init(comp) < 0) {
                        printk(KERN_INFO "[PFQ|%d] initialization of computation aborted!", so->id.value);
                        pfq_computation_fini(comp);
                        err = -EPERM;
                        goto error;
		}

                /* enable functional program */

                if (pfq_set_group_prog(gid, comp, context) < 0) {
                        printk(KERN_INFO "[PFQ|%d] set group program error!\n", so->id.value);
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

