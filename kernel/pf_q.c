/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
 *             Andrea Di Pietro <andrea.dipietro@for.unipi.it>
 * 	       Loris Gazzarrini <loris.gazzarrini@iet.unipi.it>
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
#include <linux/version.h>
#include <linux/module.h>
#include <linux/moduleparam.h>
#include <linux/semaphore.h>
#include <linux/rwsem.h>
#include <linux/socket.h>
#include <linux/types.h>
#include <linux/skbuff.h>
#include <linux/highmem.h>
#include <linux/ioctl.h>
#include <linux/ip.h>
#include <linux/poll.h>
#include <linux/etherdevice.h>
#include <linux/kthread.h>
#include <linux/vmalloc.h>
#include <linux/percpu.h>
#include <linux/bug.h>

#include <net/sock.h>
#ifdef CONFIG_INET
#include <net/inet_common.h>
#endif

#include <linux/pf_q.h>

#include <pragma/diagnostic_pop>

#include <pf_q-shmem.h>
#include <pf_q-proc.h>
#include <pf_q-macro.h>
#include <pf_q-sockopt.h>
#include <pf_q-devmap.h>
#include <pf_q-group.h>
#include <pf_q-engine.h>
#include <pf_q-symtable.h>
#include <pf_q-bitops.h>
#include <pf_q-bpf.h>
#include <pf_q-memory.h>
#include <pf_q-sock.h>
#include <pf_q-thread.h>
#include <pf_q-global.h>
#include <pf_q-vlan.h>
#include <pf_q-stats.h>
#include <pf_q-endpoint.h>
#include <pf_q-shared-queue.h>
#include <pf_q-skbuff-pool.h>
#include <pf_q-transmit.h>
#include <pf_q-percpu.h>
#include <pf_q-GC.h>


static struct packet_type       pfq_prot_hook;


MODULE_LICENSE("GPL");
MODULE_AUTHOR("Nicola Bonelli <nicola@pfq.io>");
MODULE_DESCRIPTION("Functional Networking Framework for Multi-core Architectures");

#ifdef PFQ_DEBUG
#pragma message "[PFQ] *** PFQ_DEBUG mode ***"
#endif
#ifdef DEBUG
#pragma message "[PFQ] *** DEBUG mode ***"
#endif

static DEFINE_SEMAPHORE(sock_sem);

void pfq_timer(unsigned long cpu);

/* send this packet to selected sockets */

static inline
void mask_to_sock_queue(unsigned long n, unsigned long mask, unsigned long long *sock_queue)
{
	unsigned long bit;
	pfq_bitwise_foreach(mask, bit,
	{
	        int index = pfq_ctz(bit);
                sock_queue[index] |= 1UL << n;
        })
}

/*
 * Find the next power of two.
 * from "Hacker's Delight, Henry S. Warren."
 */

static inline
unsigned clp2(unsigned int x)
{
        x = x - 1;
        x = x | (x >> 1);
        x = x | (x >> 2);
        x = x | (x >> 4);
        x = x | (x >> 8);
        x = x | (x >> 16);
        return x + 1;
}


/*
 * Optimized folding operation...
 */

static inline
unsigned int pfq_fold(unsigned int a, unsigned int b)
{
	unsigned int c;
	if (b == 1)
		return 0;
        c = b - 1;
        if (likely((b & c) == 0))
		return a & c;
        switch(b)
        {
        case 3:  return a % 3;
        case 5:  return a % 5;
        case 6:  return a % 6;
        case 7:  return a % 7;
        default: {
                const unsigned int p = clp2(b);
                const unsigned int r = a & (p-1);
                return r < b ? r : a % b;
            }
        }
}


static inline
void send_to_kernel(struct sk_buff *skb)
{
	skb_pull(skb, skb->mac_len);
	skb->peeked = capture_incoming;
	netif_receive_skb(skb);
}


static int
pfq_process_batch(struct local_data * local, struct gc_data *gcollector, int cpu)
{
	unsigned long long sock_queue[Q_SKBUFF_SHORT_BATCH];
        unsigned long group_mask, socket_mask;
	struct lazy_fwd_targets targets;
        struct sk_buff *skb;

        long unsigned n, bit, lb;
        struct pfq_monad monad;
	struct gc_buff buff;
	size_t this_batch_len;

#ifdef PFQ_RX_PROFILE
	cycles_t start, stop;
#endif

#if (LINUX_VERSION_CODE >= KERNEL_VERSION(3,9,0))
	BUILD_BUG_ON_MSG(Q_SKBUFF_SHORT_BATCH > (sizeof(sock_queue[0]) << 3), "skbuff batch overflow");
#endif

	this_batch_len = gc_size(gcollector);

	__sparse_add(&global_stats.recv, this_batch_len, cpu);

	/* cleanup sock_queue... */

        memset(sock_queue, 0, sizeof(sock_queue));
	group_mask = 0;

#ifdef PFQ_RX_PROFILE
	start = get_cycles();
#endif
        /* setup all the skbs collected */

	for_each_skbuff(SKBUFF_BATCH_ADDR(gcollector->pool), skb, n)
        {
		uint16_t queue = skb_rx_queue_recorded(skb) ? skb_get_rx_queue(skb) : 0;
		unsigned long local_group_mask = pfq_devmap_get_groups(skb->dev->ifindex, queue);
		group_mask |= local_group_mask;
		PFQ_CB(skb)->group_mask = local_group_mask;
		PFQ_CB(skb)->monad = &monad;
	}

        /* process all groups enabled for this batch */

	pfq_bitwise_foreach(group_mask, bit,
	{
		pfq_gid_t gid = { pfq_ctz(bit) };

		struct pfq_group * this_group = pfq_get_group(gid);
		bool bf_filter_enabled = atomic_long_read(&this_group->bp_filter);
		bool vlan_filter_enabled = pfq_vlan_filters_enabled(gid);
		struct gc_queue_buff refs = { len:0 };

		socket_mask = 0;

		for_each_gcbuff(&gcollector->pool, buff, n)
		{
			struct pfq_computation_tree *prg;
			unsigned long sock_mask = 0;

			/* stop processing packets in GC ? */

			if (n == this_batch_len)
				break;

			/* skip this packet for this group ? */

			if ((PFQ_CB(buff.skb)->group_mask & bit) == 0)
				continue;

			/* increment counter for this group */

			__sparse_inc(&this_group->stats.recv, cpu);

			/* check if bp filter is enabled */

			if (bf_filter_enabled) {
				struct sk_filter *bpf = (struct sk_filter *)atomic_long_read(&this_group->bp_filter);

#if (LINUX_VERSION_CODE < KERNEL_VERSION(3,15,0))
				if (bpf && !sk_run_filter(buff.skb, bpf->insns))
#else
				if (bpf && !SK_RUN_FILTER(bpf, buff.skb))
#endif
				{
					__sparse_inc(&this_group->stats.drop, cpu);
					continue;
				}
			}

			/* check vlan filter */

			if (vlan_filter_enabled) {
				if (!pfq_check_group_vlan_filter(gid, buff.skb->vlan_tci & ~VLAN_TAG_PRESENT)) {
					__sparse_inc(&this_group->stats.drop, cpu);
					continue;
				}
			}

			/* check where a functional program is available for this group */

			prg = (struct pfq_computation_tree *)atomic_long_read(&this_group->comp);
			if (prg) {
				unsigned long cbit, eligible_mask = 0;
				size_t to_kernel = PFQ_CB(buff.skb)->log->to_kernel;
				size_t num_fwd = PFQ_CB(buff.skb)->log->num_devs;

				/* setup monad for this computation */

				monad.fanout.class_mask = Q_CLASS_DEFAULT;
				monad.fanout.type = fanout_copy;
				monad.state = 0;
				monad.group = this_group;

				/* run the functional program */

				buff = pfq_run(prg, buff).value;
				if (buff.skb == NULL) {
					__sparse_inc(&this_group->stats.drop, cpu);
					continue;
				}

				/* save a reference to the current packet */
				refs.queue[refs.len++] = buff;

				/* update stats */

                                __sparse_add(&this_group->stats.frwd, PFQ_CB(buff.skb)->log->num_devs -num_fwd, cpu);
                                __sparse_add(&this_group->stats.kern, PFQ_CB(buff.skb)->log->to_kernel -to_kernel, cpu);

				/* skip the packet? */

				if (is_drop(monad.fanout)) {
					__sparse_inc(&this_group->stats.drop, cpu);
					continue;
				}

				/* compute the eligible mask of sockets enabled for this packet... */

				pfq_bitwise_foreach(monad.fanout.class_mask, cbit,
				{
					int class = pfq_ctz(cbit);
					eligible_mask |= atomic_long_read(&this_group->sock_mask[class]);
				})

				if (is_steering(monad.fanout)) {

					/* cache the number of sockets in the mask */

					if (eligible_mask != local->eligible_mask) {
						unsigned long ebit;
						local->eligible_mask = eligible_mask;
						local->sock_cnt = 0;
						pfq_bitwise_foreach(eligible_mask, ebit,
						{
							local->sock_mask[local->sock_cnt++] = ebit;
						})
					}

					if (likely(local->sock_cnt)) {
						unsigned int h = monad.fanout.hash ^ (monad.fanout.hash >> 8) ^
							(monad.fanout.hash >> 16);
						sock_mask |= local->sock_mask[pfq_fold(h, local->sock_cnt)];
					}
				}
				else {  /* clone or continue ... */

					sock_mask |= eligible_mask;
				}
			}
			else {
				/* save a reference to the current packet */
				refs.queue[refs.len++] = buff;
				sock_mask |= atomic_long_read(&this_group->sock_mask[0]);
			}

			mask_to_sock_queue(n, sock_mask, sock_queue);
			socket_mask |= sock_mask;
		}

		/* copy payloads to endpoints... */

		pfq_bitwise_foreach(socket_mask, lb,
		{
			int i = pfq_ctz(lb);
			struct pfq_sock * so;
			pfq_id_t id = { i };
			so= pfq_get_sock_by_id(id);
			copy_to_endpoint_buffs(so, &refs, sock_queue[i], cpu, gid);
		})
	})

	/* forward skbs to network devices */

	gc_get_fwd_targets(gcollector, &targets);

	if (targets.cnt_total)
	{
		size_t total = pfq_lazy_xmit_exec(gcollector, &targets);
		__sparse_add(&global_stats.frwd, total, cpu);
		__sparse_add(&global_stats.disc, targets.cnt_total - total, cpu);
	}

	/* forward skbs to kernel or to the pool */

	for_each_skbuff(SKBUFF_BATCH_ADDR(gcollector->pool), skb, n)
	{
		struct pfq_cb *cb = PFQ_CB(skb);

		/* send a copy of this skb to the kernel */

		if (cb->direct && fwd_to_kernel(skb)) {
		        __sparse_inc(&global_stats.kern, cpu);
			send_to_kernel(skb);
		}
		else {
			pfq_kfree_skb_pool(skb, &local->rx_pool);
		}
	}

	/* reset the GC */

	gc_reset(gcollector);
	local_bh_enable();

#ifdef PFQ_RX_PROFILE
	stop = get_cycles();
	if (printk_ratelimit())
		printk(KERN_INFO "[PFQ] Rx profile: %llu_tsc.\n", (stop-start)/batch_len);
#endif
        return 0;
}


static int
pfq_receive(struct napi_struct *napi, struct sk_buff * skb, int direct)
{
        struct gc_data *gcollector;
	struct local_data * local;
	struct gc_buff buff;
	int cpu;

	/* if no socket is open drop the packet */

	if (unlikely(pfq_get_sock_count() == 0)) {
		kfree_skb(skb);
		return 0;
	}

	/* disable soft-irq */

        local_bh_disable();

        cpu = smp_processor_id();
	local = per_cpu_ptr(cpu_data, cpu);
	gcollector = &local->gc;

	if (likely(skb))
	{
		/* if required, timestamp the packet now */

		if (skb->tstamp.tv64 == 0)
			__net_timestamp(skb);

		/* if vlan header is present, remove it */

		if (vl_untag && skb->protocol == cpu_to_be16(ETH_P_8021Q)) {
			skb = pfq_vlan_untag(skb);
			if (unlikely(!skb)) {
				sparse_inc(&global_stats.lost);
				local_bh_enable();
				return -1;
			}
		}

		skb_reset_mac_len(skb);

		/* push the mac header: reset skb->data to the beginning of the packet */

		if (likely(skb->pkt_type != PACKET_OUTGOING))
		    skb_push(skb, skb->mac_len);

		/* pass the ownership of this skb to the garbage collector */

		buff = gc_make_buff(gcollector, skb);
		if (buff.skb == NULL) {
			if (printk_ratelimit())
				printk(KERN_INFO "[PFQ] GC: memory exhausted!\n");
			__sparse_inc(&global_stats.lost, cpu);
			kfree_skb(skb);
			local_bh_enable();
			return 0;
		}

		PFQ_CB(buff.skb)->direct = direct;

		if ((gc_size(gcollector) < capt_batch_len) &&
		     (ktime_to_ns(ktime_sub(skb_get_ktime(buff.skb), local->last_ts)) < 1000000))
		{
			local_bh_enable();
			return 0;
		}

		local->last_ts = skb_get_ktime(buff.skb);
	}
	else {
                if (gc_size(gcollector) == 0)
		{
			local_bh_enable();
			return 0;
		}
	}

	return pfq_process_batch(local, gcollector, cpu);
}


/* simple packet HANDLER */

static int
pfq_packet_rcv
(
    struct sk_buff *skb, struct net_device *dev,
    struct packet_type *pt
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,16))
    ,struct net_device *orig_dev
#endif
    )
{
	if (skb->pkt_type == PACKET_LOOPBACK)
		goto out;

	if (skb->peeked) {
		skb->peeked = 0;
		goto out;
	}

	skb = skb_share_check(skb, GFP_ATOMIC);
	if (unlikely(!skb))
		return 0;

        switch(skb->pkt_type)
        {
            case PACKET_OUTGOING: {
                if (!capture_outgoing)
                        goto out;

                skb->mac_len = ETH_HLEN;
            } break;

            default:
		if (!capture_incoming)
			goto out;
        }

        return pfq_receive(NULL, skb, 0);
out:
	kfree_skb(skb);
	return 0;
}


void pfq_timer(unsigned long cpu)
{
	struct local_data *local;

	pfq_receive(NULL, NULL, 0);

	local = per_cpu_ptr(cpu_data, cpu);
	mod_timer_pinned(&local->timer, jiffies + msecs_to_jiffies(100));
}



static void pfq_sock_destruct(struct sock *sk)
{
        skb_queue_purge(&sk->sk_error_queue);

        WARN_ON(atomic_read(&sk->sk_rmem_alloc));
        WARN_ON(atomic_read(&sk->sk_wmem_alloc));

        sk_refcnt_debug_dec(sk);
}


static int
pfq_release(struct socket *sock)
{
        struct sock * sk = sock->sk;
        struct pfq_sock *so;
        pfq_id_t id;
        int total = 0;

	if (!sk)
		return 0;

        so = pfq_sk(sk);
        id = so->id;

        /* stop TX thread (if running) */

	pfq_stop_all_tx_threads(so);

        pr_devel("[PFQ|%d] releasing socket...\n", id);

        pfq_leave_all_groups(so->id);
        pfq_release_sock_id(so->id);

        if (so->shmem.addr)
                pfq_shared_queue_disable(so);

        down(&sock_sem);

        /* purge both batch and recycle queues if no socket is open */

        if (pfq_get_sock_count() == 0) {
                total += pfq_percpu_flush();
        }

        up (&sock_sem);

        if (total)
                printk(KERN_INFO "[PFQ|%d] cleanup: %d skb purged.\n", id, total);

        sock_orphan(sk);
	sock->sk = NULL;
	sock_put(sk);

        up_read(&symtable_sem);

	pr_devel("[PFQ|%d] socket closed.\n", id);
        return 0;
}


static unsigned int
pfq_poll(struct file *file, struct socket *sock, poll_table * wait)
{
        struct sock *sk = sock->sk;
        struct pfq_sock *so = pfq_sk(sk);
        unsigned int mask = 0;

#ifdef PFQ_USE_EXTENDED_PROC
	sparse_inc(&global_stats.poll);
#endif

	poll_wait(file, &so->rx_opt.waitqueue, wait);

        if(!pfq_get_rx_queue(&so->rx_opt))
                return mask;

        if (pfq_mpsc_queue_len(so) > 0)
                mask |= POLLIN | POLLRDNORM;

        return mask;
}

static
int pfq_ioctl(struct socket *sock, unsigned int cmd, unsigned long arg)
{
        switch (cmd) {
#ifdef CONFIG_INET
        case SIOCGIFFLAGS:
        case SIOCSIFFLAGS:
        case SIOCGIFCONF:
        case SIOCGIFMETRIC:
        case SIOCSIFMETRIC:
        case SIOCGIFMEM:
        case SIOCSIFMEM:
        case SIOCGIFMTU:
        case SIOCSIFMTU:
        case SIOCSIFLINK:
        case SIOCGIFHWADDR:
        case SIOCSIFHWADDR:
        case SIOCSIFMAP:
        case SIOCGIFMAP:
        case SIOCSIFSLAVE:
        case SIOCGIFSLAVE:
        case SIOCGIFINDEX:
        case SIOCGIFNAME:
        case SIOCGIFCOUNT:
        case SIOCSIFHWBROADCAST:
            return(inet_dgram_ops.ioctl(sock, cmd, arg));
#endif
        default:
            return -ENOIOCTLCMD;
        }

        return 0;
}

static int pfq_netdev_notifier(struct notifier_block *this, unsigned long info,
			       void *data)
{
	struct net_device *dev = netdev_notifier_info_to_dev(data);

	if (dev) {
                const char *kind = "NETDEV_UNKNOWN";
		BUG_ON(dev->ifindex >= Q_MAX_DEVICE);

		switch(info) {
			case NETDEV_UP			: kind = "NETDEV_UP"; break;
			case NETDEV_DOWN		: kind = "NETDEV_DOWN"; break;
			case NETDEV_REBOOT		: kind = "NETDEV_REBOOT"; break;
			case NETDEV_CHANGE		: kind = "NETDEV_CHANGE"; break;
			case NETDEV_REGISTER		: kind = "NETDEV_REGISTER"; break;
			case NETDEV_UNREGISTER		: kind = "NETDEV_UNREGISTER"; break;
			case NETDEV_CHANGEMTU		: kind = "NETDEV_CHANGEMTU"; break;
			case NETDEV_CHANGEADDR		: kind = "NETDEV_CHANGEADDR"; break;
			case NETDEV_GOING_DOWN		: kind = "NETDEV_GOING_DOWN"; break;
			case NETDEV_CHANGENAME		: kind = "NETDEV_CHANGENAME"; break;
			case NETDEV_FEAT_CHANGE		: kind = "NETDEV_FEAT_CHANGE"; break;
			case NETDEV_BONDING_FAILOVER	: kind = "NETDEV_BONDING_FAILOVER"; break;
			case NETDEV_PRE_UP		: kind = "NETDEV_PRE_UP"; break;
			case NETDEV_PRE_TYPE_CHANGE	: kind = "NETDEV_PRE_TYPE_CHANGE"; break;
			case NETDEV_POST_TYPE_CHANGE	: kind = "NETDEV_POST_TYPE_CHANGE"; break;
			case NETDEV_POST_INIT		: kind = "NETDEV_POST_INIT"; break;
			case NETDEV_UNREGISTER_FINAL	: kind = "NETDEV_UNREGISTER_FINAL"; break;
			case NETDEV_RELEASE		: kind = "NETDEV_RELEASE"; break;
			case NETDEV_NOTIFY_PEERS	: kind = "NETDEV_NOTIFY_PEERS"; break;
			case NETDEV_JOIN		: kind = "NETDEV_JOIN"; break;
			case NETDEV_CHANGEUPPER		: kind = "NETDEV_CHANGEUPPER"; break;
			case NETDEV_RESEND_IGMP		: kind = "NETDEV_RESEND_IGMP"; break;
			case NETDEV_PRECHANGEMTU	: kind = "NETDEV_PRECHANGEMTU"; break;
			// case NETDEV_CHANGEINFODATA	: kind = "NETDEV_CHANGEINFODATA"; break;
			// case NETDEV_BONDING_INFO	: kind = "NETDEV_BONDING_INFO"; break;
		}
		printk(KERN_INFO "[PFQ] %s: device %s, ifindex %d\n", kind, dev->name, dev->ifindex);
		return NOTIFY_OK;
	}

	return NOTIFY_DONE;
}


static
void pfq_register_device_handler(void)
{
        if (capture_incoming || capture_outgoing) {
                pfq_prot_hook.func = pfq_packet_rcv;
                pfq_prot_hook.type = __constant_htons(ETH_P_ALL);
                dev_add_pack(&pfq_prot_hook);
        }
}


static
void unregister_device_handler(void)
{
        if (capture_incoming || capture_outgoing) {
                dev_remove_pack(&pfq_prot_hook); /* Remove protocol hook */
        }
}


static struct proto_ops pfq_ops =
{
	.family = PF_Q,
        .owner = THIS_MODULE,

        /* Operations that make no sense on queue sockets. */
        .connect    = sock_no_connect,
        .socketpair = sock_no_socketpair,
        .accept     = sock_no_accept,
        .getname    = sock_no_getname,
        .listen     = sock_no_listen,
        .shutdown   = sock_no_shutdown,
        .sendpage   = sock_no_sendpage,

        /* Now the operations that really occur. */
        .release    = pfq_release,
        .bind       = sock_no_bind,
        .mmap       = pfq_mmap,
        .poll       = pfq_poll,
        .setsockopt = pfq_setsockopt,
        .getsockopt = pfq_getsockopt,
        .ioctl      = pfq_ioctl,
        .recvmsg    = sock_no_recvmsg,
        .sendmsg    = sock_no_sendmsg
};


static struct proto pfq_proto =
{
	.name  = "PFQ",
        .owner = THIS_MODULE,
        .obj_size = sizeof(struct pfq_sock)
};


static int
pfq_create(
#if(LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,24))
    struct net *net,
#endif
    struct socket *sock, int protocol
#if(LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,33))
    , int kern
#endif
    )
{
        struct pfq_sock *so;
        struct sock *sk;

        /* security and sanity check */

        if (!capable(CAP_NET_ADMIN))
                return -EPERM;
        if (sock->type != SOCK_RAW)
                return -ESOCKTNOSUPPORT;
        if (protocol != __constant_htons(ETH_P_ALL))
                return -EPROTONOSUPPORT;

        sock->state = SS_UNCONNECTED;

        sk = sk_alloc(net, PF_INET, GFP_KERNEL, &pfq_proto);
        if (sk == NULL) {
                printk(KERN_WARNING "[PFQ] error: could not allocate a socket\n");
                return -ENOMEM;
        }

        sock->ops = &pfq_ops;

        /* initialize the socket */

        sock_init_data(sock,sk);

        so = pfq_sk(sk);

        /* get a unique id for this sock */

        so->id = pfq_get_free_id(so);
        if (so->id == -1) {
                printk(KERN_WARNING "[PFQ] error: resource exhausted\n");
                sk_free(sk);
                return -EBUSY;
        }

        /* memory mapped queues are allocated later, when the socket is enabled */

	so->egress_type  = pfq_endpoint_socket;
	so->egress_index = 0;
	so->egress_queue = 0;

        so->shmem.addr = NULL;
        so->shmem.size = 0;
        so->shmem.kind = 0;

        so->shmem.hugepages = NULL;
        so->shmem.npages = 0;

        down(&sock_sem);

        /* initialize both rx_opt and tx_opt */

        pfq_rx_opt_init(&so->rx_opt, capt_slot_size);
        pfq_tx_opt_init(&so->tx_opt, xmit_slot_size);

        /* initialize socket */

        sk->sk_family   = PF_Q;
        sk->sk_destruct = pfq_sock_destruct;

        sk_refcnt_debug_inc(sk);

        up (&sock_sem);

        down_read(&symtable_sem);
        return 0;
}


static struct net_proto_family pfq_family_ops =
{
	.family = PF_Q,
        .create = pfq_create,
        .owner = THIS_MODULE,
};



static struct notifier_block pfq_netdev_notifier_block =
{
	.notifier_call = pfq_netdev_notifier
};


static int __init pfq_init_module(void)
{
        int err = 0;

        printk(KERN_INFO "[PFQ] loading...\n");

	/* check options */

        if (capt_batch_len <= 0 || capt_batch_len > Q_SKBUFF_SHORT_BATCH) {
                printk(KERN_INFO "[PFQ] capt_batch_len=%d not allowed: valid range (0,%zu]!\n",
                       capt_batch_len, Q_SKBUFF_SHORT_BATCH);
                return -EFAULT;
        }

        if (xmit_batch_len <= 0 || xmit_batch_len > Q_SKBUFF_SHORT_BATCH) {
                printk(KERN_INFO "[PFQ] xmit_batch_len=%d not allowed: valid range (0,%zu]!\n",
                       xmit_batch_len, Q_SKBUFF_SHORT_BATCH);
                return -EFAULT;
        }

	if (skb_pool_size > Q_MAX_POOL_SIZE) {
                printk(KERN_INFO "[PFQ] skb_pool_size=%d not allowed: valid range [0,%d]!\n",
                       skb_pool_size, Q_MAX_POOL_SIZE);
		return -EFAULT;
	}

	/* initialize data structures ... */

	pfq_groups_init();

	/* initialization */

	err = pfq_percpu_init();
	if (err < 0)
		goto err;

	err = pfq_proc_init();
	if (err < 0)
		goto err2;

        /* register PFQ sniffer protocol */

        err = proto_register(&pfq_proto, 0);
        if (err < 0)
		goto err3;

	/* register the pfq socket */

        err = sock_register(&pfq_family_ops);
        if (err < 0)
		goto err4;

#ifdef PFQ_USE_SKB_POOL
	err = pfq_skb_pool_init_all();
        if (err < 0) {
		pfq_skb_pool_free_all();
		goto err5;
	}
        printk(KERN_INFO "[PFQ] skb pool initialized.\n");
#endif

	/* register pfq-lang default functions */
	pfq_symtable_init();

        /* finally register the basic device handler */
        pfq_register_device_handler();

	/* register netdev notifier */
        register_netdevice_notifier(&pfq_netdev_notifier_block);

	/* ensure each device has ifindex < Q_MAX_DEVICE */
	{
		struct net_device *dev;
		for_each_netdev(&init_net, dev)
			BUG_ON(dev->ifindex >= Q_MAX_DEVICE);
	}

        printk(KERN_INFO "[PFQ] version %d.%d.%d ready!\n",
               PFQ_MAJOR(PFQ_VERSION_CODE),
               PFQ_MINOR(PFQ_VERSION_CODE),
               PFQ_PATCHLEVEL(PFQ_VERSION_CODE));

        return 0;

#ifdef PFQ_USE_SKB_POOL
err5:
        sock_unregister(PF_Q);
#endif

err4:
        proto_unregister(&pfq_proto);
err3:
	pfq_proc_fini();
err2:
	pfq_percpu_flush();
	free_percpu(cpu_data);
err:
	return err;
}


static void __exit pfq_exit_module(void)
{
        int total = 0;

#ifdef PFQ_USE_SKB_POOL
        pfq_skb_pool_enable(false);
#endif
	/* unregister netdevice notifier */
        unregister_netdevice_notifier(&pfq_netdev_notifier_block);

        /* unregister the basic device handler */
        unregister_device_handler();

        /* unregister the pfq socket */
        sock_unregister(PF_Q);

        /* unregister the pfq protocol */
        proto_unregister(&pfq_proto);

        /* disable direct capture */
        pfq_devmap_monitor_reset();

        /* wait grace period */
        msleep(Q_GRACE_PERIOD);

        /* free per CPU data */
        total += pfq_percpu_flush();

#ifdef PFQ_USE_SKB_POOL
        total += pfq_skb_pool_free_all();
	SPARSE_ADD(&memory_stats.pool_pop, total);
#endif
        if (total)
                printk(KERN_INFO "[PFQ] %d skbuff freed.\n", total);

        /* free per-cpu data */
	free_percpu(cpu_data);

	/* free symbol table of pfq-lang functions */
	pfq_symtable_free();

	pfq_proc_fini();

        printk(KERN_INFO "[PFQ] unloaded.\n");
}


/* pfq direct capture drivers support */

static inline
int pfq_direct_capture(const struct sk_buff *skb)
{
        return pfq_devmap_monitor_get(skb->dev->ifindex);
}


static inline
int pfq_normalize_skb(struct sk_buff *skb)
{
        skb_reset_network_header(skb);
	skb_reset_transport_header(skb);

#ifdef PFQ_USE_SKB_LINEARIZE
	if(skb_linearize(skb) < 0) {
		__kfree_skb(skb);
		return -1;
	}
#endif
	return 0;
}


static int
pfq_netif_receive_skb(struct sk_buff *skb)
{
        if (likely(pfq_direct_capture(skb))) {

		if (pfq_normalize_skb(skb) < 0)
			return NET_RX_DROP;

		pfq_receive(NULL, skb, 2);
		return NET_RX_SUCCESS;
	}

	return netif_receive_skb(skb);
}


static int
pfq_netif_rx(struct sk_buff *skb)
{
        if (likely(pfq_direct_capture(skb))) {

		if (pfq_normalize_skb(skb) < 0)
			return NET_RX_DROP;

		pfq_receive(NULL, skb, 1);
		return NET_RX_SUCCESS;
	}

	return netif_rx(skb);
}


static gro_result_t
pfq_gro_receive(struct napi_struct *napi, struct sk_buff *skb)
{
        if (likely(pfq_direct_capture(skb))) {

		if (pfq_normalize_skb(skb) < 0)
			return GRO_DROP;

                pfq_receive(napi, skb, 3);
                return GRO_NORMAL;
        }

        return napi_gro_receive(napi,skb);
}


EXPORT_SYMBOL_GPL(pfq_netif_rx);
EXPORT_SYMBOL_GPL(pfq_netif_receive_skb);
EXPORT_SYMBOL_GPL(pfq_gro_receive);

EXPORT_SYMBOL_GPL(pfq_symtable_register_functions);
EXPORT_SYMBOL_GPL(pfq_symtable_unregister_functions);

module_init(pfq_init_module);
module_exit(pfq_exit_module);
