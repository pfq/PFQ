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
#include <linux/swab.h>

#include <pf_q-module.h>


static Action_SkBuff
steering_field(arguments_t args, SkBuff b)
{
	uint32_t offset = get_arg_0(uint32_t, args);
	uint32_t size   = get_arg_1(uint32_t, args);

	uint32_t data, *ptr;
	uint32_t mask;

	if (size > (sizeof(data)*8)) { 	/* size is number of bits */

		if (printk_ratelimit())
                	printk(KERN_INFO "[PFQ/lang] steering_field: bit-size too big (max. 32)!\n");

                return Drop(b);
	}

	ptr = skb_header_pointer(b.skb, offset, sizeof(uint32_t), &data);
        if (ptr == NULL)
        	return Drop(b);

	mask = (1ULL << size) - 1;

	return Steering(b, *ptr & mask);
}


static Action_SkBuff
steering_link(arguments_t args, SkBuff b)
{
        uint32_t * w;

        w = (uint32_t *)eth_hdr(b.skb);

	return Steering(b, w[0] ^ w[1] ^ w[2]); // 3 * sizeof(uint32_t) = 12 bytes.
}


static Action_SkBuff
steering_vlan_id(arguments_t args, SkBuff b)
{
        if (b.skb->vlan_tci & VLAN_VID_MASK)
 	        return Steering(b, b.skb->vlan_tci & VLAN_VID_MASK);
        else
                return Drop(b);
}


static Action_SkBuff
steering_ip(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;
		__be32 hash;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return Drop(b);

		hash = ip->saddr ^ ip->daddr;

        	return Steering(b, *(uint32_t *)&hash);
	}

        return Drop(b);
}


static int steering_net_init(arguments_t args)
{
	__be32 addr = get_arg_0(__be32, args);
	int prefix  = get_arg_1(int, args);
        int subpref = get_arg_2(int, args);

	__be32 mask, submask;

	mask    = make_mask(prefix);
	submask = make_mask(subpref);

	set_arg_0(args, addr & mask);
	set_arg_1(args, mask);
	set_arg_2(args, submask);

	pr_devel("[PFQ|init] steer_net: addr=%pI4 mask=%pI4 submask=%pI4\n", &addr, &mask, &submask);

	return 0;
}


static Action_SkBuff
steering_net(arguments_t args, SkBuff b)
{
	__be32 addr    = get_arg_0(__be32, args);
	__be32 mask    = get_arg_1(__be32, args);
	__be32 submask = get_arg_2(__be32, args);

	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return Drop(b);

		if ((ip->saddr & mask) == addr)
		{
        		return Steering(b, __swab32(ntohl(ip->saddr & submask)));
		}
		if ((ip->daddr & mask) == addr)
		{
        		return Steering(b, __swab32(ntohl(ip->daddr & submask)));
		}
	}

        return Drop(b);
}


static Action_SkBuff
steering_flow(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;
               	__be32 hash;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return Drop(b);

		if (ip->protocol != IPPROTO_UDP &&
		    ip->protocol != IPPROTO_TCP)
                        return Drop(b);

		udp = skb_header_pointer(b.skb, b.skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp == NULL)
			return Drop(b);  /* broken */

		hash = ip->saddr ^ ip->daddr ^ (__force __be32)udp->source ^ (__force __be32)udp->dest;

        	return Steering(b, *(uint32_t *)&hash);
	}

        return Drop(b);
}


static Action_SkBuff
steering_ip6(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IPV6))
	{
		struct ipv6hdr _ip6h;
    		const struct ipv6hdr *ip6;
		__be32 hash;

		ip6 = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_ip6h), &_ip6h);
 		if (ip6 == NULL)
                        return Drop(b);

		hash = ip6->saddr.in6_u.u6_addr32[0] ^
		       ip6->saddr.in6_u.u6_addr32[1] ^
		       ip6->saddr.in6_u.u6_addr32[2] ^
		       ip6->saddr.in6_u.u6_addr32[3] ^
		       ip6->daddr.in6_u.u6_addr32[0] ^
		       ip6->daddr.in6_u.u6_addr32[1] ^
		       ip6->daddr.in6_u.u6_addr32[2] ^
		       ip6->daddr.in6_u.u6_addr32[3];

		return Steering(b, *(uint32_t *)&hash);
	}

        return Drop(b);
}


struct pfq_function_descr steering_functions[] = {

	{ "steer_link",  "SkBuff -> Action SkBuff", steering_link    },
        { "steer_vlan",  "SkBuff -> Action SkBuff", steering_vlan_id },
        { "steer_ip",    "SkBuff -> Action SkBuff", steering_ip      },
        { "steer_ip6",	 "SkBuff -> Action SkBuff", steering_ip6     },
        { "steer_flow",  "SkBuff -> Action SkBuff", steering_flow    },
	{ "steer_field", "Word32 -> Word32 -> SkBuff -> Action SkBuff", steering_field },

	{ "steer_net",   "Word32 -> Word32 -> Word32 -> SkBuff -> Action SkBuff", steering_net, steering_net_init },

        { NULL }};

