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

#include <lang/skbuff.h>
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
	uint32_t *data, data_;

	if (size > 4) {
		if (printk_ratelimit())
			printk(KERN_INFO "[pfq-lang] steering_field: size too big (max. 4 bytes)!\n");
		return Drop(skb);
	}

	if (!(data = skb_header_pointer(PFQ_SKB(skb), offset, size, &data_)))
		return Drop(skb);

	return Steering(skb, *data);
}


static ActionSkBuff
steering_field_symmetric(arguments_t args, SkBuff skb)
{
	uint32_t offset1 = GET_ARG_0(uint32_t, args);
	uint32_t offset2 = GET_ARG_1(uint32_t, args);
	uint32_t size    = GET_ARG_2(uint32_t, args);
	uint32_t *data1, *data2, data1_, data2_;

	if (size > 4) {
		if (printk_ratelimit())
			printk(KERN_INFO "[pfq-lang] steering_field_symmetric: size too big (max. 4 bytes)!\n");
		return Drop(skb);
	}

	if (!(data1 = skb_header_pointer(PFQ_SKB(skb), offset1, size, &data1_)))
		return Drop(skb);
	if (!(data2 = skb_header_pointer(PFQ_SKB(skb), offset2, size, &data2_)))
		return Drop(skb);

	return Steering(skb, *data1 ^ *data2);
}


static ActionSkBuff
steering_field_double(arguments_t args, SkBuff skb)
{
	uint32_t offset1 = GET_ARG_0(uint32_t, args);
	uint32_t offset2 = GET_ARG_1(uint32_t, args);
	uint32_t size    = GET_ARG_2(uint32_t, args);
	uint32_t *data1, *data2, data1_, data2_;

	if (size > 4) {
		if (printk_ratelimit())
			printk(KERN_INFO "[pfq-lang] steering_field_double: size too big (max. 4 bytes)!\n");
		return Drop(skb);
	}

	if (!(data1 = skb_header_pointer(PFQ_SKB(skb), offset1, size, &data1_)))
		return Drop(skb);
	if (!(data2 = skb_header_pointer(PFQ_SKB(skb), offset2, size, &data2_)))
		return Drop(skb);

	return DoubleSteering(skb, *data1, *data2);
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


static int steering_link_local_init(arguments_t args)
{
	char *mac = GET_ARG(char *, args);
	char mac_addr[6];

	if (!mac_pton(mac, mac_addr)) {
		printk(KERN_INFO "[pfq-lang] steering_link_local: bad mac address format!\n");
		return -EINVAL;
	}

	memcpy(mac, mac_addr, 6);
	printk(KERN_INFO "[pfq-lang] steering_link_local: gateway MAC -> %*phC\n", 6, mac);
	return 0;
}


static ActionSkBuff
steering_link_local(arguments_t args, SkBuff skb)
{
	uint16_t * gw_mac = GET_ARG(uint16_t *, args);
	uint16_t * w;
	w = (uint16_t *)eth_hdr(PFQ_SKB(skb));

	if ((w[0] & w[1] & w[2]) == 0xffff ||
	    (w[3] & w[4] & w[5]) == 0xffff)
		return Broadcast(skb);

	if (w[0] == gw_mac[0] &&
	    w[1] == gw_mac[1] &&
	    w[2] == gw_mac[2])
		return Steering(skb, w[3] ^ w[4] ^ w[5]);

	if (w[3] == gw_mac[0] &&
	    w[4] == gw_mac[1] &&
	    w[5] == gw_mac[2])
		return Steering(skb, w[0] ^ w[1] ^ w[2]);

	return DoubleSteering(skb, w[0] ^ w[1] ^ w[2], w[3] ^ w[4] ^ w[5]);
}

static ActionSkBuff
steering_mac(arguments_t args, SkBuff skb)
{
	uint16_t * w;
	w = (uint16_t *)eth_hdr(PFQ_SKB(skb));

	if ((w[0] & w[1] & w[2]) == 0xffff ||
	    (w[3] & w[4] & w[5]) == 0xffff)
		return Broadcast(skb);

	return DoubleSteering(skb, w[0] ^ w[1] ^ w[2],
				   w[3] ^ w[4] ^ w[5]);
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
steering_p2p(arguments_t args, SkBuff skb)
{
		struct iphdr _iph;
		const struct iphdr *ip;

		ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
		if (ip == NULL)
			return Drop(skb);

		if (ip->saddr == (__force __be32)0xffffffff ||
		    ip->daddr == (__force __be32)0xffffffff)
			return Broadcast(skb);

		return Steering(skb, (__force uint32_t)(ip->saddr ^ ip->daddr));
	}


static ActionSkBuff
steering_ip(arguments_t args, SkBuff skb)
{
		struct iphdr _iph;
		const struct iphdr *ip;

		ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
		if (ip == NULL)
			return Drop(skb);

		if (ip->saddr == (__force __be32)0xffffffff ||
		    ip->daddr == (__force __be32)0xffffffff)
			return Broadcast(skb);

		return DoubleSteering(skb, (__force uint32_t)ip->saddr,
					   (__force uint32_t)ip->daddr);
	}

static int steering_ip_local_init(arguments_t args)
{
	CIDR_INIT(args, 0);
	return 0;
}

static ActionSkBuff
steering_ip_local(arguments_t args, SkBuff skb)
{
	struct CIDR_ *data = GET_PTR_0(struct CIDR_, args);
		struct iphdr _iph;
		const struct iphdr *ip;

		ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
		if (ip == NULL)
			return Drop(skb);

		if (ip->saddr == (__force __be32)0xffffffff ||
		    ip->daddr == (__force __be32)0xffffffff)
			return Broadcast(skb);

                if ((ip->daddr & data->mask) == data->addr &&
                    (ip->saddr & data->mask) == data->addr)
			return DoubleSteering(skb, (__force uint32_t)ip->saddr,
						   (__force uint32_t)ip->daddr);

                if ((ip->saddr & data->mask) == data->addr)
			return Steering(skb, (__force uint32_t)ip->saddr);

                if ((ip->daddr & data->mask) == data->addr)
			return Steering(skb, (__force uint32_t)ip->daddr);

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

		struct iphdr _iph;
		const struct iphdr *ip;
		bool src_net, dst_net;

		ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
		if (ip == NULL)
			return Drop(skb);

		if (ip->saddr == (__force __be32)0xffffffff ||
		    ip->daddr == (__force __be32)0xffffffff)
			return Broadcast(skb);

		src_net = (ip->saddr & mask) == addr;
		dst_net = (ip->daddr & mask) == addr;

		if (src_net && dst_net)
			return DoubleSteering(skb, (__force uint32_t)(ip->saddr & submask),
						   (__force uint32_t)(ip->daddr & submask));
		if (src_net)
			return Steering(skb, (__force uint32_t)(ip->saddr & submask));

		if (dst_net)
			return Steering(skb, (__force uint32_t)(ip->daddr & submask));

	return Drop(skb);
}


static ActionSkBuff
steering_flow(arguments_t args, SkBuff skb)
{
		struct iphdr _iph;
		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;
		__be32 hash;

		ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
		if (ip == NULL)
			return Drop(skb);

		if (ip->protocol != IPPROTO_UDP &&
		    ip->protocol != IPPROTO_TCP)
			return Drop(skb);

		udp = skb_ip_header_pointer(skb, (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp == NULL)
			return Drop(skb);  /* broken */

		hash = ip->saddr ^ ip->daddr ^ (__force __be32)udp->source ^ (__force __be32)udp->dest;

		return Steering(skb, (__force uint32_t)hash);
	}


struct pfq_lang_function_descr steering_functions[] = {

	{ "steer_rrobin","SkBuff -> Action SkBuff", steering_rrobin  },
	{ "steer_rss",   "SkBuff -> Action SkBuff", steering_rss     },
	{ "steer_link",  "SkBuff -> Action SkBuff", steering_link    },
	{ "steer_link_local",  "String -> SkBuff -> Action SkBuff", steering_link_local, steering_link_local_init },
	{ "steer_mac",   "SkBuff -> Action SkBuff", steering_mac     },
	{ "steer_vlan",  "SkBuff -> Action SkBuff", steering_vlan_id },
	{ "steer_ip",    "SkBuff -> Action SkBuff", steering_ip      },

	{ "steer_ip_local","CIDR -> SkBuff -> Action SkBuff", steering_ip_local, steering_ip_local_init },

	{ "steer_p2p",   "SkBuff -> Action SkBuff", steering_p2p     },
	{ "steer_flow",  "SkBuff -> Action SkBuff", steering_flow    },
	{ "steer_to",    "CInt   -> SkBuff -> Action SkBuff", steering_to },

	{ "steer_field", "Word32 -> Word32 -> SkBuff -> Action SkBuff", steering_field },
	{ "steer_field_double",   "Word32 -> Word32 -> Word32 -> SkBuff -> Action SkBuff", steering_field_double},
	{ "steer_field_symmetric","Word32 -> Word32 -> Word32 -> SkBuff -> Action SkBuff", steering_field_symmetric },

	{ "steer_net",   "Word32 -> Word32 -> Word32 -> SkBuff -> Action SkBuff", steering_net, steering_net_init },
	{ NULL }};

