/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#ifndef _PFQ_KCOMPAT_H_
#define _PFQ_KCOMPAT_H_

#include <linux/netdevice.h>
#include <linux/skbuff.h>

extern  int pfq_netif_rx(struct sk_buff *);
extern  int pfq_netif_receive_skb(struct sk_buff *);
extern  gro_result_t pfq_gro_receive(struct napi_struct *, struct sk_buff *);

extern struct sk_buff * __pfq_alloc_skb(unsigned int size, gfp_t priority, int fclone, int node);
extern struct sk_buff * pfq_dev_alloc_skb(unsigned int length);
extern struct sk_buff * __pfq_netdev_alloc_skb(struct net_device *dev, unsigned int length, gfp_t gfp);

static inline
struct sk_buff *
pfq_netdev_alloc_skb(struct net_device *dev, unsigned int length)
{
        return __pfq_netdev_alloc_skb(dev, length, GFP_ATOMIC);
}


static inline
struct sk_buff *
__pfq_netdev_alloc_skb_ip_align(struct net_device *dev, unsigned int length, gfp_t gfp)
{
        struct sk_buff *skb = __pfq_netdev_alloc_skb(dev, length + NET_IP_ALIGN, gfp);
        if (NET_IP_ALIGN && likely(skb))
                skb_reserve(skb, NET_IP_ALIGN);
        return skb;
}


static inline
struct sk_buff *
pfq_netdev_alloc_skb_ip_align(struct net_device *dev, unsigned int length)
{
        return __pfq_netdev_alloc_skb_ip_align(dev, length, GFP_ATOMIC);
}


static inline
struct sk_buff *
pfq_alloc_skb(unsigned int size, gfp_t priority)
{
        return __pfq_alloc_skb(size, priority, 0, NUMA_NO_NODE);
}


static inline
struct sk_buff *
pfq_alloc_skb_fclone(unsigned int size, gfp_t priority)
{
        return __pfq_alloc_skb(size, priority, 1, NUMA_NO_NODE);
}

#define netif_receive_skb(_skb)                         pfq_netif_receive_skb(_skb)
#define netif_rx(_skb)                                  pfq_netif_rx(_skb)
#define napi_gro_receive(_napi, _skb)                   pfq_gro_receive(_napi, _skb)

#define netdev_alloc_skb(dev,len)	  		pfq_netdev_alloc_skb(dev,len)
#define __netdev_alloc_skb_ip_align(dev,len, gfp)	__pfq_netdev_alloc_skb_ip_align(dev,len, gfp)
#define   netdev_alloc_skb_ip_align(dev,len) 	        pfq_netdev_alloc_skb_ip_align(dev,len)
#define alloc_skb(size, prio)				pfq_alloc_skb(size, prio)
#define alloc_skb_fclone(size,prio)			pfq_alloc_skb_fclone(size, prio)

#define __alloc_skb(size,prio,fclone,node)              __pfq_alloc_skb(size,prio,fclone,node)
#define __netdev_alloc_skb(dev,len,gfp)        		__pfq_netdev_alloc_skb(dev,len,gfp)
#define dev_alloc_skb(len)             			pfq_dev_alloc_skb(len)


#endif /* _PFQ_KCOMPAT_H_ */
