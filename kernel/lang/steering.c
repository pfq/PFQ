/***************************************************************
 *
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

#include <lang/module.h>
#include <lang/types.h>
#include <lang/qbuff.h>

#include <pfq/bitops.h>
#include <pfq/kcompat.h>
#include <pfq/printk.h>
#include <pfq/qbuff.h>
#include <pfq/vlan.h>



#define IP_TOS_MASK      0x3
#define IP_DSCP_MASK     0xfc

static ActionQbuff
steering_key(arguments_t args, struct qbuff * buff)
{
	uint64_t key = GET_ARG_0(uint64_t, args);
        uint32_t hash, src_hash, dst_hash;
	uint64_t field;
        bool symmetric = false;

	struct iphdr   _ip;    struct iphdr const *ip;
	struct udphdr  _udp;   struct udphdr const *udp;
	struct icmphdr _icmp;  struct icmphdr const *icmp;

	switch(key)
	{
	case Q_KEY_IP_SRC|Q_KEY_IP_DST|Q_KEY_IP_PROTO|Q_KEY_SYMMETRIC: {

		ip = qbuff_ip_header_pointer(buff, 0, sizeof(_ip), &_ip);
		if (ip == NULL)
			return Drop(buff);

		return Steering(buff, (__force uint32_t)(ip->saddr ^ ip->daddr));

	}
	case Q_KEY_IP_SRC|Q_KEY_IP_DST|Q_KEY_SRC_PORT|Q_KEY_DST_PORT|Q_KEY_IP_PROTO|Q_KEY_SYMMETRIC: {

		ip = qbuff_ip_header_pointer(buff, 0, sizeof(_ip), &_ip);
		if (ip == NULL)
			return Drop(buff);

		if (ip->protocol != IPPROTO_UDP &&
		    ip->protocol != IPPROTO_TCP) {
		    	return Drop(buff);
		}

		udp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp == NULL)
			return Drop(buff);  /* broken */

		hash = ip->saddr ^ ip->daddr ^ (__force __be32)udp->source ^ (__force __be32)udp->dest;
		return Steering(buff, (__force uint32_t)hash);
	}

	}

	hash = 0; dst_hash = src_hash = 1;

        pfq_bitwise_foreach(key, field,
        {
                switch(field)
                {
                case Q_KEY_SYMMETRIC:
                {
                        symmetric = true;
                } break;

                case Q_KEY_ETH_TYPE:
                {
	                uint16_t * w = (uint16_t *)qbuff_eth_hdr(buff);
	                hash = (hash * 2654435761) + w[6];

                } break;

                case Q_KEY_ETH_SRC:
                {
	                uint16_t * w = (uint16_t *)qbuff_eth_hdr(buff);
	                src_hash = ((src_hash << 5) + src_hash) + w[3];
	                src_hash = ((src_hash << 5) + src_hash) + w[4];
	                src_hash = ((src_hash << 5) + src_hash) + w[5];

                } break;
                case Q_KEY_ETH_DST:
                {
	                uint16_t * w = (uint16_t *)qbuff_eth_hdr(buff);
	                dst_hash = ((dst_hash << 5) + dst_hash) + w[0];
	                dst_hash = ((dst_hash << 5) + dst_hash) + w[1];
	                dst_hash = ((dst_hash << 5) + dst_hash) + w[2];

                } break;

                case Q_KEY_IP_SRC:
                {
                        ip = qbuff_ip_header_pointer(buff, 0, sizeof(_ip), &_ip);
                        if (ip == NULL)
                                return Drop(buff);
	                src_hash = ((src_hash << 5) + src_hash) + ip->saddr;

                } break;

                case Q_KEY_IP_DST:
                {
                        ip = qbuff_ip_header_pointer(buff, 0, sizeof(_ip), &_ip);
                        if (ip == NULL)
                                return Drop(buff);
	                dst_hash = ((dst_hash << 5) + dst_hash) + ip->daddr;

                } break;
                case Q_KEY_IP_PROTO:
                {
                        ip = qbuff_ip_header_pointer(buff, 0, sizeof(_ip), &_ip);
                        if (ip == NULL)
                                return Drop(buff);
	                hash = ((hash << 5) + hash) + ip->protocol;

                } break;
                case Q_KEY_IP_ECN:
                {
                        ip = qbuff_ip_header_pointer(buff, 0, sizeof(_ip), &_ip);
                        if (ip == NULL)
                                return Drop(buff);
	                hash = ((hash << 5) + hash) + (ip->tos & IP_TOS_MASK);

                } break;

                case Q_KEY_IP_DSCP:
                {
                        ip = qbuff_ip_header_pointer(buff, 0, sizeof(_ip), &_ip);
                        if (ip == NULL)
                                return Drop(buff);
	                hash = ((hash << 5) + hash) + (ip->tos & IP_DSCP_MASK);

                } break;

                case Q_KEY_SRC_PORT:
                {
                        udp = qbuff_ip_header_pointer(buff, 0, sizeof(_udp), &_udp);
                        if (udp == NULL)
                                return Drop(buff);

	                src_hash = ((src_hash << 5) + src_hash) + udp->source;

                } break;

                case Q_KEY_DST_PORT:
                {
                        udp = qbuff_ip_header_pointer(buff, 0, sizeof(_udp), &_udp);
                        if (udp == NULL)
                                return Drop(buff);

	                dst_hash = ((dst_hash << 5) + dst_hash) + udp->dest;

                } break;

                case Q_KEY_ICMP_TYPE:
                {
                        icmp = qbuff_ip_header_pointer(buff, 0, sizeof(_icmp), &_icmp);
                        if (icmp == NULL)
                                return Drop(buff);

	                hash = ((hash << 5) + hash) + icmp->type;

                } break;

                case Q_KEY_ICMP_CODE:
                {
                        icmp = qbuff_ip_header_pointer(buff, 0, sizeof(_icmp), &_icmp);
                        if (icmp == NULL)
                                return Drop(buff);

	                hash = ((hash << 5) + hash) + icmp->code;

                } break;

                default: {
                        if (printk_ratelimit())
                                printk(KERN_INFO "[PFQ] steering_key: unknown field");
                }
                }
        });


        if (symmetric)
	        return Steering(buff, hash ^ src_hash ^ dst_hash);
	else
	        return Steering(buff, hash ^ (src_hash * 2654435761 + dst_hash));

}


static ActionQbuff
steering_rrobin(arguments_t args, struct qbuff * buff)
{
	return Steering(buff, buff->counter);
}



static ActionQbuff
steering_rss(arguments_t args, struct qbuff * buff)
{
	uint32_t hash = qbuff_get_rss_hash(buff);
	return Steering(buff, hash);
}


static ActionQbuff
steering_to(arguments_t args, struct qbuff * buff)
{
	return Steering(buff, GET_ARG_0(uint32_t, args));
}


static ActionQbuff
steering_field(arguments_t args, struct qbuff * buff)
{
	int offset = GET_ARG_0(int, args);
	int size   = GET_ARG_1(int, args);
	uint32_t *data, data_;

	if (size > 4) {
		if (printk_ratelimit())
			printk(KERN_INFO "[pfq-lang] steering_field: size too big (max. 4 bytes)!\n");
		return Drop(buff);
	}

	if (!(data = qbuff_header_pointer(buff, offset, size, &data_)))
		return Drop(buff);

	return Steering(buff, *data);
}


static ActionQbuff
steering_field_symmetric(arguments_t args, struct qbuff * buff)
{
	int offset1 = GET_ARG_0(int, args);
	int offset2 = GET_ARG_1(int, args);
	int size    = GET_ARG_2(int, args);
	uint32_t *data1, *data2, data1_, data2_;

	if (size > 4) {
		if (printk_ratelimit())
			printk(KERN_INFO "[pfq-lang] steering_field_symmetric: size too big (max. 4 bytes)!\n");
		return Drop(buff);
	}

	if (!(data1 = qbuff_header_pointer(buff, offset1, size, &data1_)))
		return Drop(buff);
	if (!(data2 = qbuff_header_pointer(buff, offset2, size, &data2_)))
		return Drop(buff);

	return Steering(buff, *data1 ^ *data2);
}


static ActionQbuff
double_steering_field(arguments_t args, struct qbuff * buff)
{
	int offset1 = GET_ARG_0(int, args);
	int offset2 = GET_ARG_1(int, args);
	int size    = GET_ARG_2(int, args);
	uint32_t *data1, *data2, data1_, data2_;

	if (size > 4) {
		if (printk_ratelimit())
			printk(KERN_INFO "[pfq-lang] double_steer_field: size too big (max. 4 bytes)!\n");
		return Drop(buff);
	}

	if (!(data1 = qbuff_header_pointer(buff, offset1, size, &data1_)))
		return Drop(buff);
	if (!(data2 = qbuff_header_pointer(buff, offset2, size, &data2_)))
		return Drop(buff);

	return DoubleSteering(buff, *data1, *data2);
}


static ActionQbuff
steering_link(arguments_t args, struct qbuff * buff)
{
	uint16_t * w;
	w = (uint16_t *)qbuff_eth_hdr(buff);

	if ((w[0] & w[1] & w[2]) == 0xffff ||
	    (w[3] & w[4] & w[5]) == 0xffff)
		return Broadcast(buff);

	return Steering(buff, w[0] ^ w[1] ^ w[2] ^ w[3] ^ w[4] ^ w[5]);
}


static int steering_local_link_init(arguments_t args)
{
	char *mac = GET_ARG(char *, args);
	char mac_addr[6];

	if (!mac_pton(mac, mac_addr)) {
		printk(KERN_INFO "[pfq-lang] steering_local_link: bad mac address format!\n");
		return -EINVAL;
	}

	memcpy(mac, mac_addr, 6);
	printk(KERN_INFO "[pfq-lang] steering_local_link: gateway MAC -> %*phC\n", 6, mac);
	return 0;
}


static ActionQbuff
steering_local_link(arguments_t args, struct qbuff * buff)
{
	uint16_t * gw_mac = GET_ARG(uint16_t *, args);
	uint16_t * w;
	w = (uint16_t *)qbuff_eth_hdr(buff);

	if ((w[0] & w[1] & w[2]) == 0xffff ||
	    (w[3] & w[4] & w[5]) == 0xffff)
		return Broadcast(buff);

	if (w[0] == gw_mac[0] &&
	    w[1] == gw_mac[1] &&
	    w[2] == gw_mac[2])
		return Steering(buff, w[3] ^ w[4] ^ w[5]);

	if (w[3] == gw_mac[0] &&
	    w[4] == gw_mac[1] &&
	    w[5] == gw_mac[2])
		return Steering(buff, w[0] ^ w[1] ^ w[2]);

	return DoubleSteering(buff, w[0] ^ w[1] ^ w[2], w[3] ^ w[4] ^ w[5]);
}


static ActionQbuff
double_steering_mac(arguments_t args, struct qbuff * buff)
{
	uint16_t * w;
	w = (uint16_t *)qbuff_eth_hdr(buff);

	if ((w[0] & w[1] & w[2]) == 0xffff ||
	    (w[3] & w[4] & w[5]) == 0xffff)
		return Broadcast(buff);

	return DoubleSteering(buff, w[0] ^ w[1] ^ w[2],
				    w[3] ^ w[4] ^ w[5]);
}


static ActionQbuff
steering_vlan_id(arguments_t args, struct qbuff * buff)
{
	uint16_t vid = qbuff_vlan_tci(buff) & Q_VLAN_VID_MASK;
	if (vid)
		return Steering(buff, vid);
	else
		return Drop(buff);
}


static ActionQbuff
steering_p2p(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return Drop(buff);

	if (ip->saddr == (__force __be32)0xffffffff ||
	    ip->daddr == (__force __be32)0xffffffff)
		return Broadcast(buff);

	return Steering(buff, (__force uint32_t)(ip->saddr ^ ip->daddr));
}


static ActionQbuff
double_steering_ip(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return Drop(buff);

	if (ip->saddr == (__force __be32)0xffffffff ||
	    ip->daddr == (__force __be32)0xffffffff)
		return Broadcast(buff);

	return DoubleSteering(buff, (__force uint32_t)ip->saddr,
				   (__force uint32_t)ip->daddr);
}

static int steering_local_ip_init(arguments_t args)
{
	CIDR_INIT(args, 0);
	return 0;
}

static ActionQbuff
steering_local_ip(arguments_t args, struct qbuff * buff)
{
	struct CIDR_ *data = GET_PTR_0(struct CIDR_, args);
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return Drop(buff);

	if (ip->saddr == (__force __be32)0xffffffff ||
	    ip->daddr == (__force __be32)0xffffffff)
		return Broadcast(buff);

        if ((ip->daddr & data->mask) == data->addr &&
            (ip->saddr & data->mask) == data->addr)
		return DoubleSteering(buff, (__force uint32_t)ip->saddr,
					    (__force uint32_t)ip->daddr);

        if ((ip->saddr & data->mask) == data->addr)
		return Steering(buff, (__force uint32_t)ip->saddr);

        if ((ip->daddr & data->mask) == data->addr)
		return Steering(buff, (__force uint32_t)ip->daddr);

	return Drop(buff);
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

	pr_devel("[PFQ|init] steer_local_net: addr=%pI4 mask=%pI4 submask=%pI4\n", &addr, &mask, &submask);

	return 0;
}


static ActionQbuff
steering_local_net(arguments_t args, struct qbuff * buff)
{
	__be32 addr    = GET_ARG_0(__be32, args);
	__be32 mask    = GET_ARG_1(__be32, args);
	__be32 submask = GET_ARG_2(__be32, args);

	struct iphdr _iph;
	const struct iphdr *ip;
	bool src_net, dst_net;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return Drop(buff);

	if (ip->saddr == (__force __be32)0xffffffff ||
	    ip->daddr == (__force __be32)0xffffffff)
		return Broadcast(buff);

	src_net = (ip->saddr & mask) == addr;
	dst_net = (ip->daddr & mask) == addr;

	if (src_net && dst_net)
		return DoubleSteering(buff, (__force uint32_t)(ip->saddr & submask),
					   (__force uint32_t)(ip->daddr & submask));
	if (src_net)
		return Steering(buff, (__force uint32_t)(ip->saddr & submask));

	if (dst_net)
		return Steering(buff, (__force uint32_t)(ip->daddr & submask));

	return Drop(buff);
}


static ActionQbuff
steering_flow(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	struct udphdr _udp;
	const struct udphdr *udp;
	__be32 hash;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip == NULL)
		return Drop(buff);

	if (ip->protocol != IPPROTO_UDP &&
	    ip->protocol != IPPROTO_TCP) {
		return Steering(buff, (__force uint32_t)ip->saddr ^ (__force uint32_t)ip->daddr);
	}

	udp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(_udp), &_udp);
	if (udp == NULL)
		return Drop(buff);  /* broken */

	hash = ip->saddr ^ ip->daddr ^ (__force __be32)udp->source ^ (__force __be32)udp->dest;
	return Steering(buff, (__force uint32_t)hash);
}


struct pfq_lang_function_descr steering_functions[] = {

	{ "steer_rrobin","Qbuff -> Action Qbuff", steering_rrobin  , NULL, NULL },
	{ "steer_rss",   "Qbuff -> Action Qbuff", steering_rss     , NULL, NULL },
	{ "steer_link",  "Qbuff -> Action Qbuff", steering_link    , NULL, NULL },
	{ "steer_local_link",  "String -> Qbuff -> Action Qbuff", steering_local_link, steering_local_link_init, NULL },
	{ "steer_vlan",  "Qbuff -> Action Qbuff", steering_vlan_id , NULL, NULL },
	{ "steer_local_ip","CIDR -> Qbuff -> Action Qbuff", steering_local_ip, steering_local_ip_init, NULL},

	{ "steer_p2p",   "Qbuff -> Action Qbuff", steering_p2p     , NULL, NULL },
	{ "steer_flow",  "Qbuff -> Action Qbuff", steering_flow    , NULL, NULL },
	{ "steer_to",    "CInt   -> Qbuff -> Action Qbuff", steering_to , NULL, NULL },

	{ "steer_field", "Word32 -> Word32 -> Qbuff -> Action Qbuff", steering_field , NULL, NULL},
	{ "steer_field_symmetric","Word32 -> Word32 -> Word32 -> Qbuff -> Action Qbuff", steering_field_symmetric, NULL, NULL},

	{ "double_steer_mac",  "Qbuff -> Action Qbuff", double_steering_mac, NULL, NULL },
	{ "double_steer_ip",   "Qbuff -> Action Qbuff", double_steering_ip, NULL, NULL },
	{ "double_steer_field","Word32 -> Word32 -> Word32 -> Qbuff -> Action Qbuff", double_steering_field, NULL, NULL},

	{ "steer_local_net", "Word32 -> Word32 -> Word32 -> Qbuff -> Action Qbuff", steering_local_net, steering_net_init, NULL },

	{ "steer_key",    "Word64 -> Qbuff -> Action Qbuff", steering_key, NULL, NULL },

	{ NULL }};

