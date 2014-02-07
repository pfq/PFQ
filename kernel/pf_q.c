/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#include <linux/kernel.h>
#include <linux/version.h>
#include <linux/module.h>
#include <linux/moduleparam.h>
#include <linux/semaphore.h>
#include <linux/socket.h>
#include <linux/types.h>
#include <linux/skbuff.h>
#include <linux/highmem.h>
#include <linux/ioctl.h>
#include <linux/ip.h>
#include <linux/poll.h>
#include <linux/etherdevice.h>

#include <linux/percpu.h>

#include <net/sock.h>
#ifdef CONFIG_INET
#include <net/inet_common.h>
#endif

#include <linux/pf_q.h>

#include <pf_q-common.h>
#include <pf_q-devmap.h>
#include <pf_q-group.h>
#include <pf_q-skb-queue.h>
#include <pf_q-functional.h>
#include <pf_q-bits.h>
#include <pf_q-bpf.h>
#include <pf_q-memory.h>
#include <pf_q-queue.h>
#include <pf_q-sock.h>

#include <pf_q-mpdb-queue.h>

struct net_proto_family  pfq_family_ops;
struct packet_type       pfq_prot_hook;
struct proto             pfq_proto;
struct proto_ops         pfq_ops;

static int direct_capture = 0;

static int capture_incoming = 1;
static int capture_outgoing = 0;
static int capture_loopback = 0;

static int queue_slots  = 131072; // slots per queue
static int cap_len      = 1514;
static int prefetch_len = 1;
static int flow_control = 0;
static int vl_untag     = 0;

int recycle_len  = 1024;

MODULE_LICENSE("GPL");

MODULE_AUTHOR("Nicola Bonelli <nicola.bonelli@cnit.it>");

MODULE_DESCRIPTION("Packet capture system for 64bit multi-core architectures");

module_param(direct_capture,  int, 0644);
module_param(capture_incoming,  int, 0644);
module_param(capture_outgoing,  int, 0644);
module_param(capture_loopback,  int, 0644);


module_param(cap_len,         int, 0644);
module_param(queue_slots,     int, 0644);
module_param(prefetch_len,    int, 0644);
module_param(recycle_len,     int, 0644);
module_param(flow_control,    int, 0644);
module_param(vl_untag,        int, 0644);

MODULE_PARM_DESC(direct_capture," Direct capture packets: (0 default)");
MODULE_PARM_DESC(capture_incoming," Sniff incoming packets: (1 default)");
MODULE_PARM_DESC(capture_outgoing," Sniff outgoing packets: (0 default)");
MODULE_PARM_DESC(capture_loopback," Sniff lookback packets: (0 default)");

MODULE_PARM_DESC(cap_len,       " Default capture length (bytes)");
MODULE_PARM_DESC(queue_slots,   " Queue slots (default=131072)");
MODULE_PARM_DESC(prefetch_len,  " Prefetch queue length");
MODULE_PARM_DESC(recycle_len,   " Recycle skb list (default=1024)");
MODULE_PARM_DESC(flow_control,  " Flow control value (default=0)");
MODULE_PARM_DESC(vl_untag,      " Enable vlan untagging (default=0)");


/* timestamp toggle */

atomic_t timestamp_toggle;


struct local_data __percpu    * cpu_data;


inline
bool pfq_copy_to_user_skbs(struct pfq_rx_opt *ro, int cpu, unsigned long sock_queue, struct pfq_queue_skb *skbs, int gid)
{
        /* enqueue the sk_buff: it's wait-free. */

        int len = 0; size_t sent = 0;

        if (likely(ro->queue_addr))
        {
        	smp_rmb();

                len  = (int)hweight64(sock_queue);
                sent = mpdb_enqueue_batch(ro, sock_queue, len, skbs, gid);

        	__sparse_add(&ro->stat.recv, cpu, sent);

		if (len > sent)
		{
			__sparse_add(&ro->stat.lost, cpu, len - sent);
			return false;
		}
        }
        return true;
}


/* send this packet to selected sockets */

inline
void pfq_sock_mask_to_queue(unsigned long j, unsigned long mask, unsigned long *sock_queue)
{
	unsigned long bit;
       	bitwise_foreach(mask, bit)
	{
	        int index = pfq_ctz(bit);
                sock_queue[index] |= 1UL << j;
        }
}


void
pfq_dump_skb(struct sk_buff const *skb)
{
        unsigned char * p;

        printk(KERN_INFO "[PFQ] skb type:%d mac_len:%d proto:%x %pM -> %pM tci:%d %pI4 -> %pI4\n",
                skb->pkt_type,
                skb->mac_len,
                htons(eth_hdr(skb)->h_proto),
                eth_hdr(skb)->h_source,
                eth_hdr(skb)->h_dest,
                skb->vlan_tci & VLAN_VID_MASK,
                &ip_hdr(skb)->saddr,
                &ip_hdr(skb)->daddr
               );

        p = (unsigned char *)eth_hdr(skb);

        printk(KERN_INFO "[PFQ] %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x\n",
                       p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9], p[10], p[11], p[12], p[13], p[14], p[15],
                       p[16], p[17], p[18], p[19], p[20], p[21], p[22], p[23], p[24], p[25], p[26], p[27], p[28], p[29]);
}

/*
 * Find the next power of two.
 * from "Hacker's Delight, Henry S. Warren."
 */

inline
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

inline
unsigned int pfq_fold(unsigned int a, unsigned int b)
{
        const unsigned int c = b - 1;
        if (b & c)
        {
                switch(b)
                {
                case 3:  return a % 3;
                case 5:  return a % 5;
                case 6:  return a % 6;
                case 9:  return a % 9;
                case 10: return a % 10;
                case 11: return a % 11;
                case 12: return a % 12;
                case 13: return a % 13;
                case 17: return a % 17;
                case 18: return a % 18;
                case 19: return a % 19;
                case 20: return a % 20;
                default: {
                        const unsigned int p = clp2(b);
                        const unsigned int r = a & (p-1);
                        return likely(r < b) ? r : a % b;
                    }
                }
        }
        else
        {
                return a & c;
        }
}



int
pfq_receive(struct napi_struct *napi, struct sk_buff *skb, int direct)
{
        struct local_data * local_cache = __this_cpu_ptr(cpu_data);
        struct pfq_queue_skb * prefetch_queue = &local_cache->prefetch_queue;
        unsigned long group_mask, socket_mask;
        unsigned long sock_queue[sizeof(unsigned long) << 3];
        struct pfq_annotation *cb;
        long unsigned n, bit, lb;
        int cpu;

#ifdef PFQ_USE_FLOW_CONTROL
	/* flow control */

	if (local_cache->flowctrl &&
	    local_cache->flowctrl--)
	{
                if (direct)
                        pfq_kfree_skb_recycle(skb, &local_cache->recycle_list);
                else
                        kfree_skb(skb);

		return 0;
	}
#endif

        /* if vlan header is present, remove it */
        if (vl_untag && skb->protocol == cpu_to_be16(ETH_P_8021Q)) {
                skb = vlan_untag(skb);
                if (unlikely(!skb))
                        return -1;
        }

        /* reset mac len */

        skb_reset_mac_len(skb);

        /* push the mac header: reset skb->data to the beginning of the packet */

        if (likely(skb->pkt_type != PACKET_OUTGOING))
        {
            skb_push(skb, skb->mac_len);
        }

	/* if required, timestamp this packet now */

        if (atomic_read(&timestamp_toggle) && skb->tstamp.tv64 == 0) {
                __net_timestamp(skb);
        }

	/* enqueue the packet to the prefetch queue */

        cb = pfq_skb_annotation(skb);

        cb->direct_skb      = direct;
        cb->stolen_skb      = false;
        cb->send_to_kernel  = false;

        /* enqueue this skb ... */

        pfq_queue_skb_push(prefetch_queue, skb);

        if (pfq_queue_skb_size(prefetch_queue) < prefetch_len) {
                return 0;
	}

	/* initialize data */

        memset(sock_queue, 0, sizeof(sock_queue));

	cpu = get_cpu();

#ifdef PFQ_STEERING_PROFILE
	cycles_t a = get_cycles();
#endif

        group_mask = 0;

        queue_for_each(skb, n, prefetch_queue)
        {
                struct pfq_annotation *cb = pfq_skb_annotation(skb);
		unsigned long local_group_mask = __pfq_devmap_get_groups(skb->dev->ifindex, skb_get_rx_queue(skb));

		group_mask |= local_group_mask;

		cb->group_mask = local_group_mask;
	}

        bitwise_foreach(group_mask, bit)
        {
		int gid = pfq_ctz(bit);

                struct sk_filter *bpf = (struct sk_filter *)atomic_long_read(&pfq_groups[gid].filter);

                bool vlan_filter_enabled = __pfq_vlan_filters_enabled(gid);

                socket_mask = 0;

        	queue_for_each(skb, n, prefetch_queue)
		{
                	struct pfq_annotation *cb = pfq_skb_annotation(skb);

			unsigned long sock_mask = 0;

			ret_t ret;

			if (unlikely((cb->group_mask & bit) == 0))
                         	continue;

                        /* increment recv counter for this group */

                        __sparse_inc(&pfq_groups[gid].recv, cpu);

                        /* check bpf filter */

                        if (bpf && !sk_run_filter(skb, bpf->insns))
                        {
                                continue;
                        }

                        /* check vlan filter */

                        if (vlan_filter_enabled)
                        {
                                if (!__pfq_check_group_vlan_filter(gid, skb->vlan_tci & ~VLAN_TAG_PRESENT))
                                        continue;
                        }

                        /* retrieve the function for this group */

			if (atomic_long_read(&pfq_groups[gid].fun_ctx[0].function))
                        {
                                /* continuation-passing style evaluation */

                                sk_function_t fun = (sk_function_t) atomic_long_read(&pfq_groups[gid].fun_ctx[0].function);

                                /* reset state, index call and fun_ctx ptr */

                                cb->index  =  -1;
                                cb->state  =  0;
                                cb->fun_ctx =  pfq_groups[gid].fun_ctx;

                                ret = pfq_call(fun, skb, pass());

                                if (ret.type & action_steal)
                                {
                                        cb->stolen_skb = true;
                                        continue;
                                }

                                if (ret.type & action_to_kernel)
                                {
                                        cb->send_to_kernel = true;
                                }

                                if (likely((ret.type && ret.type & action_drop) == 0))
                                {
                                        unsigned long eligible_mask = 0;
                                        unsigned long cbit;

                                        bitwise_foreach(ret.class, cbit)
                                        {
                                                int cindex = pfq_ctz(cbit);
                                                eligible_mask |= atomic_long_read(&pfq_groups[gid].sock_mask[cindex]);
                                        }

                                        if (unlikely(ret.type & action_clone)) {

                                                sock_mask |= eligible_mask;
                                        }
                                        else {
                                                if (unlikely(eligible_mask != local_cache->eligible_mask)) {

                                                        unsigned long ebit;

                                                        local_cache->eligible_mask = eligible_mask;
                                                        local_cache->sock_cnt = 0;

                                                        bitwise_foreach(eligible_mask, ebit)
                                                        {
                                                                local_cache->sock_mask[local_cache->sock_cnt++] = ebit;
                                                        }
                                                }

                                                if (likely(local_cache->sock_cnt))
                                                {
                                                        unsigned int h = ret.hash ^ (ret.hash >> 8) ^ (ret.hash >> 16);
                                                        sock_mask |= local_cache->sock_mask[pfq_fold(h, local_cache->sock_cnt)];
                                                }
                                        }
                                }
                        }
                        else
                        {
                                sock_mask |= atomic_long_read(&pfq_groups[gid].sock_mask[0]);
                        }

			pfq_sock_mask_to_queue(n, sock_mask, sock_queue);
			socket_mask |= sock_mask;
		}

                /* copy packets of this group to pfq sockets... */

                bitwise_foreach(socket_mask, lb)
                {
                        int i = pfq_ctz(lb);
                        struct pfq_rx_opt * ro = &pfq_get_sock_by_id(i)->rx_opt;
                        if (likely(ro))
                        {
#ifdef PFQ_USE_FLOW_CONTROL
                                if (!pfq_copy_to_user_skbs(ro, cpu, sock_queue[i], prefetch_queue, gid))
                                        local_cache->flowctrl = flow_control;
#else
                                pfq_copy_to_user_skbs(ro, cpu, sock_queue[i], prefetch_queue, gid);
#endif
                        }
                }
	}

#ifdef PFQ_STEERING_PROFILE
	cycles_t b = get_cycles();

	if (printk_ratelimit())
		printk(KERN_INFO "-> %llu\n", (b-a)/prefetch_len);
#endif

        /* free skb, or route them to kernel... */

        queue_for_each(skb, n, prefetch_queue)
        {
                cb = pfq_skb_annotation(skb);

                if (unlikely(cb->stolen_skb))
                        continue;

                if (likely(cb->direct_skb))
		{
		        if (unlikely(!capture_incoming && cb->send_to_kernel))
                        {
                                if (cb->direct_skb == 1)
                                        netif_rx(skb);
                                else
                                if (cb->direct_skb == 2)
                                        netif_receive_skb(skb);
                                else
                                        napi_gro_receive(napi, skb);
                        }
                        else {
                                pfq_kfree_skb_recycle(skb, &local_cache->recycle_list);
        		}
		}
                else
                {
                        /* to avoid loops, sniffed packets are not passed back to kernel */
                        kfree_skb(skb);
                }
        }

	pfq_queue_skb_flush(prefetch_queue);
        return 0;
}


/* simple packet HANDLER */

int
pfq_packet_rcv
(
    struct sk_buff *skb, struct net_device *dev,
    struct packet_type *pt
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(2,6,16))
    ,struct net_device *orig_dev
#endif
    )
{
	skb = skb_share_check(skb, GFP_ATOMIC);
	if (unlikely(!skb))
		return 0;

        switch(skb->pkt_type)
        {
            case PACKET_OUTGOING: {
                if (!capture_outgoing)
                        return 0;
                skb->mac_len = ETH_HLEN;
            } break;

            case PACKET_LOOPBACK: {
                if (!capture_loopback)
                        return 0;
            } break;

            default: /* PACKET_INCOMING */
                if (!capture_incoming)
                        return 0;
        }

        return pfq_receive(NULL, skb, 0);
}


void
pfq_rx_opt_init(struct pfq_rx_opt *ro)
{
        /* the queue is allocate later, when the socket is enabled */

        ro->queue_info = NULL;
        ro->queue_addr = NULL;
        ro->queue_size = 0;

        /* disable tiemstamping by default */
        ro->tstamp = false;

        /* set q_slots and q_caplen default values */

        ro->caplen    = cap_len;
        ro->offset    = 0;

        /* initialize waitqueue */

        init_waitqueue_head(&ro->waitqueue);

        /* reset stats */
        sparse_set(&ro->stat.recv, 0);
        sparse_set(&ro->stat.lost, 0);
        sparse_set(&ro->stat.drop, 0);
}


void
pfq_tx_opt_init(struct pfq_tx_opt *to)
{
        /* the queue is allocate later, when the socket is enabled */

        to->queue_info          = NULL;
        to->queue_addr          = NULL;
        to->queue_size          = 0;

        to->counter             = 0;
        to->dev                 = NULL;
        to->txq                 = NULL;
        to->hardware_queue      = 0;
        to->cpu_index           = -1;
        to->queue_addr          = NULL;
        to->queue_size          = 0;
        to->thread              = NULL;
        to->thread_stop         = false;
}


static void pfq_sock_destruct(struct sock *sk)
{
        skb_queue_purge(&sk->sk_error_queue);

        WARN_ON(atomic_read(&sk->sk_rmem_alloc));
        WARN_ON(atomic_read(&sk->sk_wmem_alloc));

        sk_refcnt_debug_dec(sk);
}


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
        if (sk == NULL)
        {
                printk(KERN_WARNING "[PFQ] error: could not allocate a socket\n");
                return -ENOMEM;
        }

        sock->ops = &pfq_ops;

        /* initialize the socket */

        sock_init_data(sock,sk);

        so = pfq_sk(sk);

        /* get a unique id for this sock */

        so->id = pfq_get_free_sock_id(so);
        if (so->id == -1)
        {
                printk(KERN_WARNING "[PFQ] error: resource exhausted\n");
                sk_free(sk);
                return -EBUSY;
        }

        /* memory mapped queues are allocated later, when the socket is enabled */

        so->mem_addr = NULL;
        so->mem_size = 0;

        /* initialize both rx_opt and tx_opt */

        pfq_rx_opt_init(&so->rx_opt);

        pfq_tx_opt_init(&so->tx_opt);

        /* initialize socket */

        sk->sk_family   = PF_Q;
        sk->sk_destruct = pfq_sock_destruct;

        sk_refcnt_debug_inc(sk);

        return 0;
}


static void
pfq_rx_release(struct pfq_rx_opt *ro)
{
        /* decrease the timestamp_toggle counter */

        if (ro->tstamp)
                atomic_dec(&timestamp_toggle);

        ro->queue_addr = NULL;
        ro->queue_size = 0;

        /* Convenient way to avoid a race condition,
         * without using expensive rw-mutexes
         */

        msleep(Q_GRACE_PERIOD);
}


static void
pfq_tx_release(struct pfq_tx_opt *tq)
{
        /* TODO */
}


static int
pfq_release(struct socket *sock)
{
        struct sock * sk = sock->sk;
        struct pfq_sock * so;
        int id;

	if (!sk)
		return 0;

	so = pfq_sk(sk);

        pfq_rx_release(&so->rx_opt);
        pfq_tx_release(&so->tx_opt);

        pfq_leave_all_groups(so->id);
        pfq_release_sock_id(so->id);

        /* Convenient way to avoid a race condition,
         * without using expensive rw-mutexes
         */

        msleep(Q_GRACE_PERIOD);

        pfq_queue_free(so->mem_addr);

        sock_orphan(sk);
	sock->sk = NULL;
	sock_put(sk);

	pr_devel("[PFQ|%d] socket closed.\n", id);

        return 0;
}


static
int pfq_getsockopt(struct socket *sock,
                   int level, int optname,
                   char __user * optval, int __user * optlen)
{
        struct pfq_sock *so = pfq_sk(sock->sk);
        struct pfq_rx_opt * ro;
        int len;

        if (so == NULL)
                return -EFAULT;

        ro = &so->rx_opt;

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

		    if (group.gid < Q_ANY_GROUP || group.gid >= Q_MAX_GROUP) {
			    pr_devel("[PFQ|%d] join error: bad gid:%d!\n", so->id, group.gid);
			    return -EINVAL;
		    }

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
			    if (pfq_join_group(group.gid, so->id, group.class_mask, group.policy) < 0) {
				    pr_devel("[PFQ|%d] join error: gid:%d no permission!\n", so->id, group.gid);
				    return -EPERM;
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

        case Q_SO_GET_TSTAMP:
            {
                    if (len != sizeof(ro->tstamp))
                            return -EINVAL;
                    if (copy_to_user(optval, &ro->tstamp, sizeof(ro->tstamp)))
                            return -EFAULT;
            } break;

        case Q_SO_GET_QUEUE_MEM:
            {
                    if (len != sizeof(ro->queue_size))
                            return -EINVAL;
                    if (copy_to_user(optval, &ro->queue_size, sizeof(ro->queue_size)))
                            return -EFAULT;
            } break;

        case Q_SO_GET_CAPLEN:
            {
                    if (len != sizeof(ro->caplen))
                            return -EINVAL;
                    if (copy_to_user(optval, &ro->caplen, sizeof(ro->caplen)))
                            return -EFAULT;
            } break;

        case Q_SO_GET_SLOTS:
            {
                    if (len != sizeof(ro->queue_info->size))
                            return -EINVAL;
                    if (copy_to_user(optval, &ro->queue_info->size, sizeof(ro->queue_info->size)))
                            return -EFAULT;
            } break;

        case Q_SO_GET_OFFSET:
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

                    if (gid < 0  || gid >= Q_MAX_GROUP) {
                    	    pr_devel("[PFQ|%d] group stats error: gid:%d invalid argument!\n", so->id, gid);
			    return -EINVAL;
		    }

		    /* check whether the group is joinable.. */

		    if (!__pfq_group_access(gid, so->id, Q_GROUP_UNDEFINED, false)) {
                    	    pr_devel("[PFQ|%d] group stats error: gid:%d access denied!\n", so->id, gid);
			    return -EPERM;
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

                    if (s.gid < 0  || s.gid >= Q_MAX_GROUP) {
                    	    pr_devel("[PFQ|%d] group context error: gid:%d invalid gid!\n", so->id, s.gid);
			    return -EINVAL;
		    }

                    if (!s.size || !s.context) {
                    	    pr_devel("[PFQ|%d] group context error: gid:%d invalid argument!\n", so->id, s.gid);
                            return -EFAULT;
                    }

		    /* check whether the group is joinable.. */

		    if (!__pfq_group_access(s.gid, so->id, Q_GROUP_UNDEFINED, false)) {
                    	    pr_devel("[PFQ|%d] group context error: gid:%d access denied!\n", so->id, s.gid);
			    return -EPERM;
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


#define CHECK_GROUP_PERM(gid, msg) \
        if (gid < 0 || gid >= Q_MAX_GROUP) { \
        	    pr_devel("[PFQ|%d] " msg " error: gid:%d invalid group!\n", so->id, gid); \
                return -EINVAL; \
        } \
        \
        if (!__pfq_has_joined_group(gid, so->id)) { \
        	    pr_devel("[PFQ|%d] " msg " error: gid:%d no permission!\n", so->id, gid); \
                return -EPERM; \
        }


static
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
                                    /* alloc queue memory */

                                    if (pfq_queue_alloc(so, queue_tot_mem(so)) < 0)
                                    {
                                        return -ENOMEM;
                                    }

                                    // TODO: setup queue headers (rx and tx)

                                    /* sq = (struct pfq_rx_queue_hdr *)so->mem_addr; */
                                    /* sq->data      = (1L << 24); */
                                    /* sq->poll_wait = 0; */

				    smp_wmb();

				    // TODO: setup queues in rx and tx opt...
                            }
                    }
                    else {

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

                    CHECK_GROUP_PERM(bind.gid, "add binding");

                    pfq_devmap_update(map_set, bind.if_index, bind.hw_queue, bind.gid);
            } break;

        case Q_SO_REMOVE_BINDING:
            {
                    struct pfq_binding bind;
                    if (optlen != sizeof(struct pfq_binding))
                            return -EINVAL;

		    if (copy_from_user(&bind, optval, optlen))
                            return -EFAULT;

                    CHECK_GROUP_PERM(bind.gid, "remove binding");

                    pfq_devmap_update(map_reset, bind.if_index, bind.hw_queue, bind.gid);
            } break;

	case Q_SO_SET_TSTAMP:
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

        case Q_SO_SET_CAPLEN:
            {
                    if (optlen != sizeof(so->rx_opt.caplen))
                            return -EINVAL;
                    if (copy_from_user(&so->rx_opt.caplen, optval, optlen))
                            return -EFAULT;

                    so->rx_opt.queue_info->slot_size = MPDB_QUEUE_SLOT_SIZE(so->rx_opt.caplen);
                    pr_devel("[PFQ|%d] caplen:%lu -> slot_size:%u\n",
                                    so->id, so->rx_opt.caplen, so->rx_opt.queue_info->slot_size);
            } break;

        case Q_SO_SET_SLOTS:
            {
                    if (optlen != sizeof(so->rx_opt.queue_info->size))
                            return -EINVAL;
                    if (copy_from_user(&so->rx_opt.queue_info->size, optval, optlen))
                            return -EFAULT;

                    pr_devel("[PFQ|%d] queue_slots:%u -> slot_size:%u\n",
                                    so->id, so->rx_opt.queue_info->size, so->rx_opt.queue_info->slot_size);
            } break;

        case Q_SO_SET_OFFSET:
            {
                    if (optlen != sizeof(so->rx_opt.offset))
                            return -EINVAL;
                    if (copy_from_user(&so->rx_opt.offset, optval, optlen))
                            return -EFAULT;

                    pr_devel("[PFQ|%d] offset:%lu\n", so->id, so->rx_opt.offset);
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

                    CHECK_GROUP_PERM(gid, "reset group");

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

                    CHECK_GROUP_PERM(s.gid, "group context");

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

                    CHECK_GROUP_PERM(s.gid, "group function");

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

                    CHECK_GROUP_PERM(fprog.gid, "group fprog");

                    if (fprog.fcode.len > 0)  /* set the filter */
		    {
			struct sk_filter *filter = pfq_alloc_sk_filter(&fprog.fcode);
		 	if (filter == NULL)
			{
                    	    pr_devel("[PFQ|%d] fprog error: prepare_sk_filter for gid:%d\n", so->id, fprog.gid);
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

                    CHECK_GROUP_PERM(vlan.gid, "group vlan");

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

                    CHECK_GROUP_PERM(filt.gid, "group vlan filt");

                    if (filt.vid < -1 || filt.vid > 4094) {
                    	    pr_devel("[PFQ|%d] vlan_set error: gid:%d invalid vid:%d!\n", so->id, filt.gid, filt.vid);
			    return -EINVAL;
                    }

                    if (!__pfq_vlan_filters_enabled(filt.gid)) {
                    	    pr_devel("[PFQ|%d] vlan_set error: vlan filters disabled for gid:%d!\n", so->id, filt.gid);
			    return -EINVAL;
                    }

                    if (filt.vid  == -1) // any
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

        default:
            {
                    found = false;
            } break;
        }

        return found ? 0 : sock_setsockopt(sock, level, optname, optval, optlen);
}


static inline
int
pfq_memory_mmap(struct vm_area_struct *vma,
                unsigned long size, char *ptr, unsigned int flags)
{
        vma->vm_flags |= flags;

        if (remap_vmalloc_range(vma, ptr, 0) != 0)
        {
                printk(KERN_WARNING "[PFQ] remap_vmalloc_range!\n");
                return -EAGAIN;
        }

        return 0;
}


static int
pfq_mmap(struct file *file, struct socket *sock, struct vm_area_struct *vma)
{
        struct pfq_sock *so = pfq_sk(sock->sk);

        unsigned long size = (unsigned long)(vma->vm_end - vma->vm_start);
        int ret;

        if(size & (PAGE_SIZE-1)) {
                printk(KERN_WARNING "[PFQ] pfq_mmap: size not multiple of PAGE_SIZE!\n");
                return -EINVAL;
        }

        if(size > so->mem_size) {
                printk(KERN_WARNING "[PFQ] pfq_mmap: area too large!\n");
                return -EINVAL;
        }

        if((ret = pfq_memory_mmap(vma, size, so->mem_addr, VM_LOCKED)) < 0)
                return ret;

        return 0;
}


unsigned int
pfq_poll(struct file *file, struct socket *sock, poll_table * wait)
{
        struct sock *sk = sock->sk;
        struct pfq_sock *so = pfq_sk(sk);
        struct pfq_rx_queue_hdr * q;
        unsigned int mask = 0;

        q = (struct pfq_rx_queue_hdr *)so->rx_opt.queue_addr;
        if (q == NULL)
                return mask;

        if (mpdb_queue_len(so) >= (so->rx_opt.queue_info->size >> 1)) {
                q->poll_wait = 0;
                mask |= POLLIN | POLLRDNORM;
        }
        else if (!q->poll_wait) {
                q->poll_wait = 1;
                poll_wait(file, &so->rx_opt.waitqueue, wait);
        }

        return mask;
}


static
int
pfq_ioctl(struct socket *sock, unsigned int cmd, unsigned long arg)
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


static
void pfq_proto_ops_init(void)
{
        pfq_ops = (struct proto_ops)
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
                .bind       = sock_no_bind,         // pfq_bind,
                .mmap       = pfq_mmap,             // pfq_mmap,
                .poll       = pfq_poll,             // pfq_poll,
                .setsockopt = pfq_setsockopt,       // pfq_setsockopt,
                .getsockopt = pfq_getsockopt,       // pfq_getsockopt,
                .ioctl      = pfq_ioctl,            // pfq_ioctl,
                .recvmsg    = sock_no_recvmsg,      // pfq_recvmsg,
                .sendmsg    = sock_no_sendmsg       // pfq_sendmsg,
        };
}


static
void pfq_proto_init(void)
{
        pfq_proto = (struct proto)
        {
                .name  = "PFQ",
                .owner = THIS_MODULE,
                .obj_size = sizeof(struct pfq_sock)
        };
}


static
void pfq_net_proto_family_init(void)
{
        pfq_family_ops = (struct net_proto_family)
        {
                .family = PF_Q,
                .create = pfq_create,
                .owner = THIS_MODULE,
        };
}


static
void register_device_handler(void)
{
        if (capture_incoming || capture_outgoing || capture_loopback)
        {
                pfq_prot_hook.func = pfq_packet_rcv;
                pfq_prot_hook.type = __constant_htons(ETH_P_ALL);
                dev_add_pack(&pfq_prot_hook);
        }
}


static
void unregister_device_handler(void)
{
        if (capture_incoming || capture_outgoing || capture_loopback)
        {
                dev_remove_pack(&pfq_prot_hook); /* Remove protocol hook */
        }
}


static int __init pfq_init_module(void)
{
        int n;
        printk(KERN_INFO "[PFQ] loading (%s)...\n", Q_VERSION);

        pfq_net_proto_family_init();
        pfq_proto_ops_init();
        pfq_proto_init();

        if (prefetch_len > PFQ_QUEUE_MAX_LEN) {
                printk(KERN_INFO "[PFQ] prefetch_len=%d not allowed (max=%lu)!\n", prefetch_len, (sizeof(unsigned long) << 3)-1);
                return -EFAULT;
        }

	/* create a per-cpu context */
	cpu_data = alloc_percpu(struct local_data);
	if (!cpu_data) {
                printk(KERN_WARNING "[PFQ] out of memory!\n");
		return -ENOMEM;
        }

#ifdef PFQ_USE_SKB_RECYCLE
        {
                int cpu;

                /* setup skb-recycle list */

                for_each_possible_cpu(cpu) {
                        struct local_data *this_cpu = per_cpu_ptr(cpu_data, cpu);
                        skb_queue_head_init(&this_cpu->recycle_list);
                }
        }
#endif

        /* register pfq sniffer protocol */
        n = proto_register(&pfq_proto, 0);
        if (n != 0)
                return n;

	/* register the pfq socket */
        sock_register(&pfq_family_ops);

        /* finally register the basic device handler */
        register_device_handler();

	/* register functions */

	pfq_function_factory_init();

	printk(KERN_INFO "[PFQ] ready!\n");
        return 0;
}


static void __exit pfq_exit_module(void)
{
        int cpu;

        /* unregister the basic device handler */
        unregister_device_handler();

        /* unregister the pfq socket */
        sock_unregister(PF_Q);

        /* unregister the pfq protocol */
        proto_unregister(&pfq_proto);

        /* disable direct capture */
        __pfq_devmap_monitor_reset();

        /* wait grace period */
        msleep(Q_GRACE_PERIOD);

        /* destroy pipeline queues (of each cpu) */

        for_each_possible_cpu(cpu) {

                struct local_data *local_cache = per_cpu_ptr(cpu_data, cpu);
                struct pfq_queue_skb *this_queue = &local_cache->prefetch_queue;
                struct sk_buff *skb;
		int n = 0;
		queue_for_each(skb, n, this_queue)
		{
                        struct pfq_annotation *cb = pfq_skb_annotation(skb);
                        if (unlikely(cb->stolen_skb))
                                continue;
                 	kfree_skb(skb);
		}

       		pfq_queue_skb_flush(this_queue);

#ifdef PFQ_USE_SKB_RECYCLE
                skb_queue_purge(&local_cache->recycle_list);
#endif
        }

        /* free per-cpu data */
	free_percpu(cpu_data);

	/* free functions */

	pfq_function_factory_free();

        printk(KERN_INFO "[PFQ] unloaded.\n");
}


/* pfq direct capture drivers support */

inline
int pfq_direct_capture(const struct sk_buff *skb)
{
        return direct_capture && __pfq_devmap_monitor_get(skb->dev->ifindex);
}


inline
int pfq_normalize_skb(struct sk_buff *skb)
{
        skb_reset_network_header(skb);
	skb_reset_transport_header(skb);

#ifdef PFQ_USE_SKB_LINEARIZE
#pragma message "[PFQ] using skb_linearize!"
	if(skb_linearize(skb) < 0)
	{
		__kfree_skb(skb);
		return -1;
	}
#endif
	return 0;
}


int
pfq_netif_receive_skb(struct sk_buff *skb)
{
        if (likely(pfq_direct_capture(skb)))
        {
		if (pfq_normalize_skb(skb) < 0)
                	return NET_RX_DROP;

		pfq_receive(NULL, skb, 2);
		return NET_RX_SUCCESS;
	}

	return netif_receive_skb(skb);
}


int
pfq_netif_rx(struct sk_buff *skb)
{
        if (likely(pfq_direct_capture(skb)))
        {
		if (pfq_normalize_skb(skb) < 0)
                	return NET_RX_DROP;

		pfq_receive(NULL, skb, 1);
		return NET_RX_SUCCESS;
	}

	return netif_rx(skb);
}


gro_result_t
pfq_gro_receive(struct napi_struct *napi, struct sk_buff *skb)
{
        if (likely(pfq_direct_capture(skb)))
        {
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

EXPORT_SYMBOL_GPL(pfq_register_functions);
EXPORT_SYMBOL_GPL(pfq_unregister_functions);

module_init(pfq_init_module);
module_exit(pfq_exit_module);
