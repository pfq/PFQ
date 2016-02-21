/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
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
#include <linux/swab.h>
#include <linux/inetdevice.h>

#include <pragma/diagnostic_pop>

#include <lang/module.h>


static ActionSkBuff
steering_rrobin(arguments_t args, SkBuff skb)
{
	return Steering(skb, PFQ_CB(skb)->counter);
}



static ActionSkBuff
steering_rss(arguments_t args, SkBuff skb)
{
#if (LINUX_VERSION_CODE < KERNEL_VERSION(3,14,0))
	uint32_t hash = 0;
#else
	uint32_t hash = skb_get_hash(PFQ_SKB(skb));
#endif
	return Steering(skb, hash);
}


static ActionSkBuff
steering_to(arguments_t args, SkBuff skb)
{
	return Steering(skb, GET_ARG_0(uint32_t, args));
}


static ActionSkBuff
steering_field(arguments_t args, SkBuff skb)
{
	uint32_t offset = GET_ARG_0(uint32_t, args);
	uint32_t size   = GET_ARG_1(uint32_t, args);
	uint32_t data;

	if (size > 4) {
		if (printk_ratelimit())
			printk(KERN_INFO "[pfq-lang] steering_field: size too big (max. 4 bytes)!\n");
		return Drop(skb);
	}

	if (!skb_header_pointer(PFQ_SKB(skb), offset, size, &data))
		return Drop(skb);

	return Steering(skb, data);
}


static ActionSkBuff
steering_field2(arguments_t args, SkBuff skb)
{
	uint32_t offset1 = GET_ARG_0(uint32_t, args);
	uint32_t offset2 = GET_ARG_1(uint32_t, args);
	uint32_t size    = GET_ARG_2(uint32_t, args);
	uint32_t data1, data2;

	if (size > 4) {
		if (printk_ratelimit())
			printk(KERN_INFO "[pfq-lang] steering_field: size too big (max. 4 bytes)!\n");
		return Drop(skb);
	}

	if (!skb_header_pointer(PFQ_SKB(skb), offset1, size, &data1))
		return Drop(skb);
	if (!skb_header_pointer(PFQ_SKB(skb), offset2, size, &data2))
		return Drop(skb);

	return Steering(skb, data1 ^ data2);
}


static ActionSkBuff
steering_link(arguments_t args, SkBuff skb)
{
	uint16_t * w;
	w = (uint16_t *)eth_hdr(PFQ_SKB(skb));

	if ((w[0] & w[1] & w[2]) == 0xffff ||
	    (w[3] & w[4] & w[5]) == 0xffff)
		return Broadcast(skb);

	return Steering(skb, w[0] ^ w[1] ^ w[2] ^ w[3] ^ w[4] ^ w[5]);
}
}


static ActionSkBuff
steering_vlan_id(arguments_t args, SkBuff skb)
{
	if (skb->vlan_tci & VLAN_VID_MASK)
		return Steering(skb, skb->vlan_tci & VLAN_VID_MASK);
	else
		return Drop(skb);
}


static ActionSkBuff
steering_ip(arguments_t args, SkBuff skb)
{
	if (eth_hdr(PFQ_SKB(skb))->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;
		__be32 hash;

		ip = skb_header_pointer(PFQ_SKB(skb), skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return Drop(skb);

		hash = ip->saddr ^ ip->daddr;

		return Steering(skb, (__force uint32_t)hash);
	}

	return Drop(skb);
}


static int steering_net_init(arguments_t args)
{
	__be32 addr = GET_ARG_0(__be32, args);
	int prefix  = GET_ARG_1(int, args);
	int subpref = GET_ARG_2(int, args);

	__be32 mask, submask;

	mask    = inet_make_mask(prefix);
	submask = inet_make_mask(subpref);

	SET_ARG_0(args, addr & mask);
	SET_ARG_1(args, mask);
	SET_ARG_2(args, submask);

	pr_devel("[PFQ|init] steer_net: addr=%pI4 mask=%pI4 submask=%pI4\n", &addr, &mask, &submask);

	return 0;
}


static ActionSkBuff
steering_net(arguments_t args, SkBuff skb)
{
	__be32 addr    = GET_ARG_0(__be32, args);
	__be32 mask    = GET_ARG_1(__be32, args);
	__be32 submask = GET_ARG_2(__be32, args);

	if (eth_hdr(PFQ_SKB(skb))->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		ip = skb_header_pointer(PFQ_SKB(skb), skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return Drop(skb);

		if ((ip->saddr & mask) == addr)
			return Steering(skb, (__force uint32_t)(ip->saddr & submask));

		if ((ip->daddr & mask) == addr)
			return Steering(skb, (__force uint32_t)(ip->daddr & submask));
	}

	return Drop(skb);
}


static ActionSkBuff
steering_flow(arguments_t args, SkBuff skb)
{
	if (eth_hdr(PFQ_SKB(skb))->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;
		__be32 hash;

		ip = skb_header_pointer(PFQ_SKB(skb), skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return Drop(skb);

		if (ip->protocol != IPPROTO_UDP &&
		    ip->protocol != IPPROTO_TCP)
			return Drop(skb);

		udp = skb_header_pointer(PFQ_SKB(skb), skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp == NULL)
			return Drop(skb);  /* broken */

		hash = ip->saddr ^ ip->daddr ^ (__force __be32)udp->source ^ (__force __be32)udp->dest;

		return Steering(skb, (__force uint32_t)hash);
	}

	return Drop(skb);
}


static ActionSkBuff
steering_ip6(arguments_t args, SkBuff skb)
{
	if (eth_hdr(PFQ_SKB(skb))->h_proto == __constant_htons(ETH_P_IPV6))
	{
		struct ipv6hdr _ip6h;
		const struct ipv6hdr *ip6;
		__be32 hash;

		ip6 = skb_header_pointer(PFQ_SKB(skb), skb->mac_len, sizeof(_ip6h), &_ip6h);
		if (ip6 == NULL)
			return Drop(skb);

		hash = ip6->saddr.in6_u.u6_addr32[0] ^
			ip6->saddr.in6_u.u6_addr32[1] ^
			ip6->saddr.in6_u.u6_addr32[2] ^
			ip6->saddr.in6_u.u6_addr32[3] ^
			ip6->daddr.in6_u.u6_addr32[0] ^
			ip6->daddr.in6_u.u6_addr32[1] ^
			ip6->daddr.in6_u.u6_addr32[2] ^
			ip6->daddr.in6_u.u6_addr32[3];

		return Steering(skb, (__force uint32_t)hash);
	}

	return Drop(skb);
}


struct pfq_lang_function_descr steering_functions[] = {

	{ "steer_rrobin","SkBuff -> Action SkBuff", steering_rrobin  },
	{ "steer_rss",   "SkBuff -> Action SkBuff", steering_rss     },
	{ "steer_link",  "SkBuff -> Action SkBuff", steering_link    },
	{ "steer_vlan",  "SkBuff -> Action SkBuff", steering_vlan_id },
	{ "steer_ip",    "SkBuff -> Action SkBuff", steering_ip      },
	{ "steer_ip6",	 "SkBuff -> Action SkBuff", steering_ip6     },
	{ "steer_flow",  "SkBuff -> Action SkBuff", steering_flow    },
	{ "steer_to",    "CInt   -> SkBuff -> Action SkBuff", steering_to },
	{ "steer_field", "Word32 -> Word32 -> SkBuff -> Action SkBuff", steering_field },
	{ "steer_field2","Word32 -> Word32 -> Word32 -> SkBuff -> Action SkBuff", steering_field2 },
	{ "steer_net",   "Word32 -> Word32 -> Word32 -> SkBuff -> Action SkBuff", steering_net, steering_net_init },
	{ NULL }};

