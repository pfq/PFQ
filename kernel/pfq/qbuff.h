 /****************************************************************
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PFQ_QBUFF_H
#define PFQ_QBUFF_H

#include <pfq/qbuff.h>
#include <pfq/global.h>
#include <pfq/vlan.h>
#include <pfq/types.h>
#include <pfq/skbuff.h>

#include <linux/kernel.h>
#include <linux/version.h>
#include <linux/skbuff.h>
#include <linux/ip.h>

struct pfq_lang_monad;


struct qbuff
{
	void		       *addr;				/* struct sk_buff * */
	struct pfq_lang_monad  *monad;
	struct net_device      *fwd_dev[Q_BUFF_QUEUE_LEN];	/* fwd to devs */
	size_t			fwd_dev_num;
        unsigned long		fwd_mask;			/* fwd to sockets */
        uint32_t		counter;			/* unique id */
        bool			to_kernel;			/* fwd to kernel */
};



static inline void
qbuff_init( struct qbuff *buff
	      , void *addr
	      , struct pfq_lang_monad *monad
	      , size_t id)
{
	buff->addr = addr;
	buff->monad = monad;
	buff->fwd_dev_num = 0;
	buff->counter = id;
	buff->fwd_mask = 0;
	buff->to_kernel = false;
}


#define PFQ_DEFINE_QUEUE(name, size) \
	name {  \
		size_t len; \
		struct qbuff queue[size]; \
	}


struct pfq_qbuff_queue
{
	size_t len;
	struct qbuff queue[];
};



PFQ_DEFINE_QUEUE(struct pfq_qbuff_batch_queue, Q_BUFF_BATCH_LEN);
PFQ_DEFINE_QUEUE(struct pfq_qbuff_long_queue,  Q_BUFF_QUEUE_LEN);


#define PFQ_QBUFF_QUEUE(q) \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct pfq_qbuff_batch_queue *),(struct pfq_qbuff_queue *)(q), \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct pfq_qbuff_long_queue *), (struct pfq_qbuff_queue *)(q), (void)0))


#define PFQ_QBUFF_QUEUE_AT(q, n) \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct pfq_qbuff_batch_queue *), (struct qbuff  *)(&((q)->queue[n])), \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct pfq_qbuff_long_queue *),  (struct qbuff  *)(&((q)->queue[n])), \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(q),struct pfq_qbuff_queue *),       (struct qbuff  *)(&((q)->queue[n])),  (void)0)))


#define STATIC_TYPE(typ, val) __builtin_choose_expr(__builtin_types_compatible_p(typ, typeof((val))), 0, (void)0)


#define for_each_qbuff(q, buff, n) \
        for((n) = 0; ((n) < (q)->len) && ((buff) = PFQ_QBUFF_QUEUE_AT((q),n)); (n)++)


#define for_each_qbuff_with_mask(mask, q, buff, n) \
        for((n) = pfq_ctz(mask); ((n) < (q)->len) && (mask) && ((buff) = PFQ_QBUFF_QUEUE_AT((q),n)); \
                (mask) ^=(((unsigned __int128)1) << (n)), n = pfq_ctz(mask))


#define for_each_qbuff_from(x, q, buff, n) \
        for((n) = (x); ((n) < (q)->len) && ((buff) = PFQ_QBUFF_QUEUE_AT((q), n)); (n)++)


#define for_each_qbuff_upto(max, q, buff, n) \
        for((n) = 0; ((n) < (max)) && ((n) < (q)->len) && ((buff) = PFQ_QBUFF_QUEUE_AT((q),n)); (n)++)


#define QBUFF_SKB(buff) \
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(buff),struct qbuff *), (struct sk_buff *)((buff)->addr),\
	__builtin_choose_expr(__builtin_types_compatible_p(typeof(buff),struct qbuff const *), (struct sk_buff const *)((buff)->addr), (void)0))


#define QBUFF_CB(buff)  (PFQ_CB(buff->addr))


bool qbuff_ingress(struct qbuff const *buff, struct iphdr const *ip);


#define qbuff_free(buff, ...)	pfq_free_skb_pool(QBUFF_SKB(buff), __VA_ARGS__)


static inline
int qbuff_get_ifindex(struct qbuff const *buff)
{
	return QBUFF_SKB(buff)->dev->ifindex;
}

static inline
unsigned int
qbuff_headroom(struct qbuff const *buff)
{
	return skb_headroom(QBUFF_SKB(buff));
}


static inline
unsigned int
qbuff_tailroom(struct qbuff const *buff)
{
	return skb_tailroom(QBUFF_SKB(buff));
}


static inline
struct net_device *
qbuff_device(struct qbuff *buff)
{
	return QBUFF_SKB(buff)->dev;
}


static inline
uint16_t qbuff_get_queue_mapping(struct qbuff const *buff)
{
	return skb_get_queue_mapping(QBUFF_SKB(buff));
}


static inline
void qbuff_set_queue_mapping(struct qbuff *buff, uint16_t map)
{
	skb_set_queue_mapping(QBUFF_SKB(buff), map);
}


static inline
struct qbuff *
qbuff_clone(struct qbuff *buff)
{
	/* FIXME: GFP_ATMOIC */
	return NULL;
}

static inline uint32_t
qbuff_get_rss_hash(struct qbuff *buff)
{
#if (LINUX_VERSION_CODE < KERNEL_VERSION(3,14,0))
	return 0;
#else
	return skb_get_hash(QBUFF_SKB(buff));
#endif
}

static inline uint16_t
qbuff_vlan_tci(struct qbuff const *buff)
{
	return QBUFF_SKB(buff)->vlan_tci;
}


static inline void *
qbuff_header_pointer(struct qbuff const *buff, int offset, int len, void *buffer)
{
	struct sk_buff const *skb = QBUFF_SKB(buff);
	return skb_header_pointer(skb, offset, len, buffer);
}


static inline
struct ethhdr *
qbuff_eth_hdr(struct qbuff *buff)
{
	return eth_hdr(QBUFF_SKB(buff));
}


static inline
unsigned int
qbuff_len(struct qbuff const *buff)
{
	return QBUFF_SKB(buff)->len;
}


static inline size_t
qbuff_maclen(struct qbuff const *buff)
{
	return QBUFF_SKB(buff)->mac_len;
}


static inline void
qbuff_move_or_copy_to_kernel(struct qbuff *buff, gfp_t pri)
{
	struct sk_buff *nskb, *skb = QBUFF_SKB(buff);

	if (likely(skb->pkt_type != PACKET_OUTGOING))
		skb_pull(skb, QBUFF_SKB(buff)->mac_len);

	skb->network_header = 0;
	skb->transport_header = -1;
	skb_reset_mac_len(skb);

	/* copy the skb only if peeked */

	nskb = skb->peeked ? skb_copy(skb, GFP_KERNEL) : skb;
	if (nskb) {
		nskb->peeked = 0;
		netif_receive_skb(nskb);
	}
	else {
		if (printk_ratelimit())
			printk(KERN_INFO "[PFQ] error: copy_to_kernel!\n");
	}
}


static inline ktime_t
qbuff_get_ktime(struct qbuff const *buff)
{
	return skb_get_ktime(QBUFF_SKB(buff));
}


static inline uint16_t
qbuff_get_mark(struct qbuff const *buff)
{
	return QBUFF_SKB(buff)->mark;
}


static inline void
qbuff_set_mark(struct qbuff *buff, uint32_t value)
{
	QBUFF_SKB(buff)->mark = value;
}


static inline uint16_t
qbuff_get_rx_queue(struct qbuff const *buff)
{
	return skb_rx_queue_recorded(QBUFF_SKB(buff)) ? skb_get_rx_queue(QBUFF_SKB(buff)) : 0;
}


static inline bool
qbuff_run_bp_filter(struct qbuff *buff, struct pfq_group *this_group)
{
	struct sk_filter *bpf = (struct sk_filter *)atomic_long_read(&this_group->bp_filter);

	if (!bpf) return true;

#if (LINUX_VERSION_CODE < KERNEL_VERSION(3,15,0))
	return sk_run_filter(QBUFF_SKB(buff), bpf->insns);
#elif (LINUX_VERSION_CODE < KERNEL_VERSION(4,4,0))
	return SK_RUN_FILTER(bpf, QBUFF_SKB(buff));
#else
	return bpf_prog_run_save_cb(bpf->prog, QBUFF_SKB(buff));
#endif

}

static inline bool
qbuff_run_vlan_filter(struct qbuff const *buff, pfq_gid_t gid)
{
	return pfq_group_check_vlan_filter(gid, QBUFF_SKB(buff)->vlan_tci & ~VLAN_TAG_PRESENT);
}


#endif /* PFQ_QBUFF_H */
