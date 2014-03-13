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

#include <linux/kernel.h>
#include <linux/module.h>


#include <linux/pf_q-fun.h>

/*
 *      Functional combinator strategy:
 *
 *      input           FILTER          PAR comb.       PEND        NEG comb.       STEERING
 *      -----------------------------------------------------------------------------------------
 *      PASS            F(p) P/D        SKIP(pass)      PASS        DROP            hash(p)/D
 *
 *      DROP            DROP/-          PASS            DROP        PASS            DROP/-
 *
 *      SKIP[ret]       SKIP[ret]       SKIP[ret]       ret         SKIP[ret]       SKIP[ret]
 *
 *      CLONE           F(p) C/D        SKIP(clone)     CLONE       DROP            hash(p)/D
 *
 *      STEERING[n]     F(p) S(n)/D     SKIP(steer[n])  S(n)        DROP            hash(p)/D
 *
 *      STEAL           -               -               -           -               -
 */


static struct sk_buff *
id(context_t ctx, struct sk_buff *skb)
{
        return cont(skb);
}

/* ------------ filters ------------- */

static struct sk_buff *
filter_ip(context_t ctx, struct sk_buff *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip)
                {
                        return cont(skb);
                }
        }

	return drop(skb);
}


static struct sk_buff *
filter_udp(context_t ctx, struct sk_buff *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
 			return drop(skb);

		if (ip->protocol != IPPROTO_UDP)
			return drop(skb);

		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp)
		{
		        return cont(skb);
                }
	}

	return drop(skb);
}


static struct sk_buff *
filter_tcp(context_t ctx, struct sk_buff *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct tcphdr _tcp;
		const struct tcphdr *tcp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
	                return drop(skb);

		if (ip->protocol != IPPROTO_TCP)
	                return drop(skb);

		tcp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_tcp), &_tcp);
		if (tcp)
                {
                        return cont(skb);
                }
	}

        return drop(skb);
}


static struct sk_buff *
filter_icmp(context_t ctx, struct sk_buff *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct icmphdr _icmp;
		const struct icmphdr *icmp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
	                return drop(skb);

		if (ip->protocol != IPPROTO_ICMP)
	                return drop(skb);

		icmp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_icmp), &_icmp);
		if (icmp)
		{
                        return cont(skb);
                }
	}

        return drop(skb);
}


static struct sk_buff *
filter_flow(context_t ctx, struct sk_buff *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
 			return drop(skb);

		if (ip->protocol != IPPROTO_UDP &&
		    ip->protocol != IPPROTO_TCP)
			return drop(skb);

		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp)
		{
		        return cont(skb);
                }
	}

       	return drop(skb);
}


static struct sk_buff *
filter_vlan(context_t ctx, struct sk_buff *skb)
{
        if ((skb->vlan_tci & VLAN_VID_MASK) == 0)
                return drop(skb);
        else
                return cont(skb);
}


/* ------------ steering ------------- */


static struct sk_buff *
steering_mac(context_t ctx, struct sk_buff *skb)
{
        uint16_t * a;

        a = (uint16_t *)eth_hdr(skb);

	return steering(skb, Q_CLASS_DEFAULT, a[0] ^ a[1] ^ a[2] ^ a[3] ^ a[4] ^ a[5] );
}



static struct sk_buff *
steering_vlan_id(context_t ctx, struct sk_buff *skb)
{
        if (skb->vlan_tci & VLAN_VID_MASK)
 	        return steering(skb, Q_CLASS_DEFAULT, skb->vlan_tci & VLAN_VID_MASK);
        else
                return drop(skb);
}


static struct sk_buff *
steering_ipv4(context_t ctx, struct sk_buff *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return drop(skb);

        	return steering(skb, Q_CLASS_DEFAULT, ip->saddr ^ ip->daddr);
	}

        return drop(skb);
}


static struct sk_buff *
steering_flow(context_t ctx, struct sk_buff *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return drop(skb);

		if (ip->protocol != IPPROTO_UDP &&
		    ip->protocol != IPPROTO_TCP)
                        return drop(skb);

		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp == NULL)
			return drop(skb);  /* broken */

        	return steering(skb, Q_CLASS_DEFAULT, ip->saddr ^ ip->daddr ^ udp->source ^ udp->dest);
	}

        return drop(skb);
}


static struct sk_buff *
steering_ipv6(context_t ctx, struct sk_buff *skb)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IPV6))
	{
		struct ipv6hdr _ip6h;
    		const struct ipv6hdr *ip6;

		ip6 = skb_header_pointer(skb, skb->mac_len, sizeof(_ip6h), &_ip6h);
 		if (ip6 == NULL)
                        return drop(skb);

		return steering(skb, Q_CLASS_DEFAULT,
			ip6->saddr.in6_u.u6_addr32[0] ^
			ip6->saddr.in6_u.u6_addr32[1] ^
			ip6->saddr.in6_u.u6_addr32[2] ^
			ip6->saddr.in6_u.u6_addr32[3] ^
			ip6->daddr.in6_u.u6_addr32[0] ^
			ip6->daddr.in6_u.u6_addr32[1] ^
			ip6->daddr.in6_u.u6_addr32[2] ^
			ip6->daddr.in6_u.u6_addr32[3] );
	}

        return drop(skb);
}


/* ------------ forwarding ------------- */


static struct sk_buff *
forward_legacy(context_t ctx, struct sk_buff *skb)
{
        return to_kernel(drop(skb));
}


static struct sk_buff *
forward_clone(context_t ctx, struct sk_buff *skb)
{
        return broadcast(skb, Q_CLASS_DEFAULT);
}


static struct sk_buff *
forward_broadcast(context_t ctx, struct sk_buff *skb)
{
        return broadcast(skb, Q_CLASS_ANY);
}


static struct sk_buff *
forward_sink(context_t ctx, struct sk_buff *skb)
{
        kfree_skb(skb);
        return steal(skb);
}

struct pfq_function_descr default_functions[] = {

        { "id",                 id                      },
        { "ip",                 filter_ip               },
        { "udp",                filter_udp              },
        { "tcp",                filter_tcp              },
        { "icmp",               filter_icmp             },
        { "flow",               filter_flow             },
        { "vlan",               filter_vlan             },

	{ "steer-mac",          steering_mac            },
        { "steer-vlan-id",      steering_vlan_id        },
        { "steer-ipv4",         steering_ipv4           },
        { "steer-ipv6",         steering_ipv6           },
        { "steer-flow",         steering_flow           },

        { "legacy",             forward_legacy          },
        { "clone",              forward_clone           },
        { "broadcast",          forward_broadcast       },
        { "sink",               forward_sink            },
        { NULL, NULL}};

