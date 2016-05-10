/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
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
#include <linux/mutex.h>
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

#include <engine/global.h>
#include <engine/io.h>
#include <engine/devmap.h>
#include <engine/lang/engine.h>
#include <engine/lang/symtable.h>
#include <engine/lang/GC.h>
#include <engine/percpu.h>
#include <engine/group.h>
#include <engine/sock.h>
#include <engine/stats.h>
#include <engine/queue.h>
#include <engine/endpoint.h>
#include <engine/define.h>

#include <pf_q-shmem.h>
#include <pf_q-proc.h>
#include <pf_q-sockopt.h>
#include <pf_q-bpf.h>
#include <pf_q-memory.h>
#include <pf_q-thread.h>
#include <pf_q-vlan.h>
#include <pf_q-pool.h>
#include <pf_q-kcompact.h>


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

static DEFINE_MUTEX(sock_lock);

void pfq_timer(unsigned long cpu);


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
	sparse_inc(&memory_stats, os_free);
	kfree_skb(skb);
	return 0;
}


void pfq_timer(unsigned long cpu)
{
	struct pfq_percpu_data *data;

	pfq_receive(NULL, NULL, 0);

	data = per_cpu_ptr(percpu_data, cpu);
	mod_timer_pinned(&data->timer, jiffies + msecs_to_jiffies(100));
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

        /* unbind AX threads */

        pr_devel("[PFQ|%d] unbinding devs and Tx threads...\n", id);

	pfq_sock_tx_unbind(so);

        pr_devel("[PFQ|%d] releasing socket...\n", id);

        pfq_leave_all_groups(so->id);
        pfq_release_sock_id(so->id);

	msleep(Q_GRACE_PERIOD);

        if (so->shmem.addr) {
		pr_devel("[PFQ|%d] freeing shared memory...\n", id);
                pfq_shared_queue_disable(so);
	}

        mutex_lock(&sock_lock);

        /* purge both batch and recycle queues if no socket is open */

        if (pfq_get_sock_count() == 0)
                total += pfq_percpu_destruct();

        mutex_unlock(&sock_lock);

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

	poll_wait(file, &so->opt.waitqueue, wait);

        if(!pfq_get_rx_queue(&so->opt))
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

		switch(info)
		{
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
			case NETDEV_RELEASE		: kind = "NETDEV_RELEASE"; break;
			case NETDEV_NOTIFY_PEERS	: kind = "NETDEV_NOTIFY_PEERS"; break;
			case NETDEV_JOIN		: kind = "NETDEV_JOIN"; break;
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(3,8,0))
			case NETDEV_UNREGISTER_FINAL	: kind = "NETDEV_UNREGISTER_FINAL"; break;
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(3,11,0))
			case NETDEV_CHANGEUPPER		: kind = "NETDEV_CHANGEUPPER"; break;
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(3,13,0))
			case NETDEV_RESEND_IGMP		: kind = "NETDEV_RESEND_IGMP"; break;
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(3,14,0))
			case NETDEV_PRECHANGEMTU	: kind = "NETDEV_PRECHANGEMTU"; break;
#endif
#endif
#endif
#endif
		}

		pr_devel("[PFQ] %s: device %s, ifindex %d\n", kind, dev->name, dev->ifindex);
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
	int id;

        /* security and sanity check */

        if (!capable(CAP_NET_ADMIN))
                return -EPERM;
        if (sock->type != SOCK_RAW)
                return -ESOCKTNOSUPPORT;
        if (protocol != __constant_htons(ETH_P_ALL))
                return -EPROTONOSUPPORT;

        sock->state = SS_UNCONNECTED;

	sk = sk_alloc(  net
		     ,  PF_INET
		     ,  GFP_KERNEL
		     ,  &pfq_proto
#if(LINUX_VERSION_CODE >= KERNEL_VERSION(4,2,0))
		     ,  kern
#endif
		     );

        if (sk == NULL) {
                printk(KERN_WARNING "[PFQ] error: pfq_sock_init: could not allocate a socket!\n");
                return -ENOMEM;
        }

        sock->ops = &pfq_ops;

        /* initialize the socket */

        sock_init_data(sock,sk);

        so = pfq_sk(sk);

        /* get a unique id for this sock */

        id = pfq_get_free_id(so);
        if (id == -1) {
                printk(KERN_WARNING "[PFQ] error: pfq_sock_init: resource exhausted!\n");
                sk_free(sk);
                return -EBUSY;
        }

        mutex_lock(&sock_lock);

        /* initialize sock */

	if (pfq_sock_init(so, id) < 0) {
                printk(KERN_WARNING "[PFQ] error: pfq_sock_init: no memory!\n");
		sk_free(sk);
		mutex_unlock(&sock_lock);
		return -EINVAL;
	}

        /* initialize sock opt */

        pfq_sock_opt_init(&so->opt, capt_slot_size, xmit_slot_size);

        /* initialize socket */

        sk->sk_family   = PF_Q;
        sk->sk_destruct = pfq_sock_destruct;

        sk_refcnt_debug_inc(sk);

        mutex_unlock(&sock_lock);

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



static int
check_tx_threads_affinity(void)
{
	int i, j;

	for(i=0; i < tx_thread_nr; ++i)
	{
		if (tx_affinity[i] < 0 || tx_affinity[i] >= num_online_cpus())
		{
			printk(KERN_INFO "[PFQ] error: Tx thread bad affinity on cpu:%d!\n", tx_affinity[i]);
			return -EFAULT;
		}
	}

	for(i=0; i < tx_thread_nr-1; ++i)
	for(j=i+1; j < tx_thread_nr; ++j)
	{
		if (tx_affinity[i] == tx_affinity[j])
		{
			printk(KERN_INFO "[PFQ] error: Tx thread affinity for cpu:%d already in use!\n", tx_affinity[i]);
			return -EFAULT;
		}
	}

	return 0;
}


static int __init pfq_init_module(void)
{
        int err = -EFAULT;

        printk(KERN_INFO "[PFQ] loading...\n");

	/* check options */

        if (capt_batch_len <= 0 || capt_batch_len > Q_SKBUFF_BATCH) {
                printk(KERN_INFO "[PFQ] capt_batch_len=%d not allowed: valid range (0,%d]!\n",
                       capt_batch_len, Q_SKBUFF_BATCH);
                return -EFAULT;
        }

        if (xmit_batch_len <= 0 || xmit_batch_len > (Q_SKBUFF_BATCH*4)) {
                printk(KERN_INFO "[PFQ] xmit_batch_len=%d not allowed: valid range (0,%d]!\n",
                       xmit_batch_len, Q_SKBUFF_BATCH * 4);
                return -EFAULT;
        }

	if (skb_pool_size > Q_MAX_POOL_SIZE) {
                printk(KERN_INFO "[PFQ] skb_pool_size=%d not allowed: valid range [0,%d]!\n",
                       skb_pool_size, Q_MAX_POOL_SIZE);
		return -EFAULT;
	}

	/* initialize data structures ... */

	err = pfq_groups_init();
	if (err < 0)
		goto err;

	/* initialization */

	err = pfq_percpu_alloc();
	if (err < 0)
		goto err;

	err = pfq_percpu_init();
	if (err < 0)
		goto err1;

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
	pfq_lang_symtable_init();

        /* finally register the basic device handler */
        pfq_register_device_handler();

	/* register netdev notifier */
        register_netdevice_notifier(&pfq_netdev_notifier_block);

	/* start Tx threads for asynchronous transmission */
	if (tx_thread_nr)
	{
		if ((err = check_tx_threads_affinity()) < 0)
			goto err6;

		if ((err = pfq_start_all_tx_threads()) < 0)
			goto err7;
	}

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

err7:
	pfq_stop_all_tx_threads();
err6:
	unregister_netdevice_notifier(&pfq_netdev_notifier_block);
	unregister_device_handler();
err5:
        sock_unregister(PF_Q);
err4:
        proto_unregister(&pfq_proto);
err3:
	pfq_proc_destruct();
err2:
	pfq_percpu_destruct();
err1:
	pfq_percpu_free();
err:
	return err < 0 ? err : -EFAULT;
}


static void __exit pfq_exit_module(void)
{
        int total = 0;

	/* stop Tx threads */
	pfq_stop_all_tx_threads();

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
        total += pfq_percpu_destruct();

#ifdef PFQ_USE_SKB_POOL
        total += pfq_skb_pool_free_all();
	sparse_add(&memory_stats, pool_pop, total);
#endif
        if (total)
                printk(KERN_INFO "[PFQ] %d skbuff freed.\n", total);

        /* free per-cpu data */
        pfq_percpu_free();

	/* free symbol table of pfq-lang functions */
	pfq_lang_symtable_free();

	pfq_proc_destruct();

	pfq_groups_destruct();

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

EXPORT_SYMBOL(pfq_lang_symtable_register_functions);
EXPORT_SYMBOL(pfq_lang_symtable_unregister_functions);

module_init(pfq_init_module);
module_exit(pfq_exit_module);
