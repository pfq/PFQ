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


ret_t
steering_mac(struct sk_buff *skb, ret_t ret)
{
        uint16_t * a;

        if (is_skip(ret) || is_drop(ret))
                return pfq_call(get_next_function(skb), skb, ret);

        a = (uint16_t *)eth_hdr(skb);

	return steering(Q_CLASS_DEFAULT, a[0] ^ a[1] ^ a[2] ^ a[3] ^ a[4] ^ a[5] );
}


ret_t
steering_vlan_id(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun = get_next_function(skb);

        if (is_skip(ret) || is_drop(ret))
                return pfq_call(get_next_function(skb), skb, ret);

        if (skb->vlan_tci & VLAN_VID_MASK)
 	        return steering(Q_CLASS_DEFAULT, skb->vlan_tci & VLAN_VID_MASK);
        else
                return pfq_call(fun, skb, drop());
}


ret_t
steering_ipv4(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun = get_next_function(skb);

        if (is_skip(ret) || is_drop(ret))
                return pfq_call(get_next_function(skb), skb, ret);

	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return pfq_call(fun, skb, drop());

        	return steering(Q_CLASS_DEFAULT, ip->saddr ^ ip->daddr);
	}

        return pfq_call(fun, skb, drop());
}


ret_t
steering_flow(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun = get_next_function(skb);

        if (is_skip(ret) || is_drop(ret))
                return pfq_call(get_next_function(skb), skb, ret);

	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return pfq_call(fun, skb, drop());

		if (ip->protocol != IPPROTO_UDP &&
		    ip->protocol != IPPROTO_TCP)
                        return pfq_call(fun, skb, drop());

		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp == NULL)
			return drop();  /* broken */

        	return steering(Q_CLASS_DEFAULT, ip->saddr ^ ip->daddr ^ udp->source ^ udp->dest);
	}

        return pfq_call(fun, skb, drop());
}


ret_t
steering_ipv6(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun = get_next_function(skb);

        if (is_skip(ret) || is_drop(ret))
                return pfq_call(get_next_function(skb), skb, ret);

	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IPV6))
	{
		struct ipv6hdr _ip6h;
    		const struct ipv6hdr *ip6;

		ip6 = skb_header_pointer(skb, skb->mac_len, sizeof(_ip6h), &_ip6h);
 		if (ip6 == NULL)
                        return pfq_call(fun, skb, drop());

		return steering(Q_CLASS_DEFAULT,
			ip6->saddr.in6_u.u6_addr32[0] ^
			ip6->saddr.in6_u.u6_addr32[1] ^
			ip6->saddr.in6_u.u6_addr32[2] ^
			ip6->saddr.in6_u.u6_addr32[3] ^
			ip6->daddr.in6_u.u6_addr32[0] ^
			ip6->daddr.in6_u.u6_addr32[1] ^
			ip6->daddr.in6_u.u6_addr32[2] ^
			ip6->daddr.in6_u.u6_addr32[3] );
	}

        return pfq_call(fun, skb, drop());
}


ret_t
fun_legacy(struct sk_buff *skb, ret_t ret)
{
        return to_kernel(drop());
}


ret_t
fun_clone(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun = get_next_function(skb);

        if (is_skip(ret) || is_drop(ret))
                return pfq_call(get_next_function(skb), skb, ret);

        return pfq_call(fun, skb, broadcast(Q_CLASS_DEFAULT));
}


ret_t
fun_broadcast(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun = get_next_function(skb);

        if (is_skip(ret) || is_drop(ret))
                return pfq_call(get_next_function(skb), skb, ret);

        return pfq_call(fun, skb, broadcast(Q_CLASS_ANY));
}


ret_t
fun_sink(struct sk_buff *skb, ret_t ret)
{
        struct sk_buff * mskb = (struct sk_buff *)skb;

        kfree_skb(mskb);

        return stolen();
}


ret_t
fun_id(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun = get_next_function(skb);

        return pfq_call(fun, skb, ret);
}

/* filters */

ret_t
strict_vlan(struct sk_buff *skb, ret_t ret)
{
        if (is_skip(ret))
                return pfq_call(get_next_function(skb), skb, ret);
        if (is_drop(ret))
                return drop();

        if ((skb->vlan_tci & VLAN_VID_MASK) == 0)
                return drop();
        else
                return pfq_call(get_next_function(skb), skb, ret);
}


ret_t
strict_ipv4(struct sk_buff *skb, ret_t ret)
{
        if (is_skip(ret))
                return pfq_call(get_next_function(skb), skb, ret);
        if (is_drop(ret))
                return drop();

	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip)
                {
                        return pfq_call(get_next_function(skb), skb, ret);
                }
        }

        return drop();
}


ret_t
strict_udp(struct sk_buff *skb, ret_t ret)
{
        if (is_skip(ret))
                return pfq_call(get_next_function(skb), skb, ret);
        if (is_drop(ret))
                return drop();

	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
 			return drop();

		if (ip->protocol != IPPROTO_UDP)
			return drop();

		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp)
		{
                        return pfq_call(get_next_function(skb), skb, ret);
                }
	}

	return drop();
}


ret_t
strict_tcp(struct sk_buff *skb, ret_t ret)
{
        if (is_skip(ret))
                return pfq_call(get_next_function(skb), skb, ret);
        if (is_drop(ret))
                return drop();

	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct tcphdr _tcp;
		const struct tcphdr *tcp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
 			return drop();

		if (ip->protocol != IPPROTO_TCP)
			return drop();

		tcp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_tcp), &_tcp);
		if (tcp)
                {
                        return pfq_call(get_next_function(skb), skb, ret);
                }
	}

	return drop();
}


ret_t
strict_icmp(struct sk_buff *skb, ret_t ret)
{
        if (is_skip(ret))
                return pfq_call(get_next_function(skb), skb, ret);
        if (is_drop(ret))
                return drop();

	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct icmphdr _icmp;
		const struct icmphdr *icmp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
 			return drop();

		if (ip->protocol != IPPROTO_ICMP)
			return drop();

		icmp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_icmp), &_icmp);
		if (icmp)
		{
                        return pfq_call(get_next_function(skb), skb, ret);
                }
	}

	return drop();
}


ret_t
strict_flow(struct sk_buff *skb, ret_t ret)
{
        if (is_skip(ret))
                return pfq_call(get_next_function(skb), skb, ret);
        if (is_drop(ret))
                return drop();

	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
 			return drop();

		if (ip->protocol != IPPROTO_UDP &&
		    ip->protocol != IPPROTO_TCP)
			return drop();

		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp)
		{
                        return pfq_call(get_next_function(skb), skb, ret);
                }
	}

	return drop();
}


ret_t
filter_vlan(struct sk_buff *skb, ret_t ret)
{
        if (is_skip(ret) || is_drop(ret))
                return pfq_call(get_next_function(skb), skb, ret);

        if ((skb->vlan_tci & VLAN_VID_MASK) == 0)
                return pfq_call(get_next_function(skb), skb, drop());
        else
                return pfq_call(get_next_function(skb), skb, ret);
}


ret_t
filter_ipv4(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun;

        if (is_skip(ret) || is_drop(ret))
                return pfq_call(get_next_function(skb), skb, ret);

        fun = get_next_function(skb);

	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip)
                {
                        return pfq_call(fun, skb, ret);
                }
        }

	return pfq_call(fun, skb, drop());
}


ret_t
filter_udp(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun;

        if (is_skip(ret) || is_drop(ret))
                return pfq_call(get_next_function(skb), skb, ret);

        fun = get_next_function(skb);

	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
	                return pfq_call(fun, skb, drop());

		if (ip->protocol != IPPROTO_UDP)
	                return pfq_call(fun, skb, drop());

		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp)
		{
                        return pfq_call(fun, skb, ret);
                }
	}

	return pfq_call(fun, skb, drop());
}


ret_t
filter_tcp(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun;

        if (is_skip(ret) || is_drop(ret))
                return pfq_call(get_next_function(skb), skb, ret);

        fun = get_next_function(skb);

	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct tcphdr _tcp;
		const struct tcphdr *tcp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
	                return pfq_call(fun, skb, drop());

		if (ip->protocol != IPPROTO_TCP)
	                return pfq_call(fun, skb, drop());

		tcp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_tcp), &_tcp);
		if (tcp)
                {
                        return pfq_call(fun, skb, ret);
                }
	}

	return pfq_call(fun, skb, drop());
}


ret_t
filter_icmp(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun;

        if (is_skip(ret) || is_drop(ret))
                return pfq_call(get_next_function(skb), skb, ret);

        fun = get_next_function(skb);

	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct icmphdr _icmp;
		const struct icmphdr *icmp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
	                return pfq_call(fun, skb, drop());

		if (ip->protocol != IPPROTO_ICMP)
	                return pfq_call(fun, skb, drop());

		icmp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_icmp), &_icmp);
		if (icmp)
		{
                        return pfq_call(fun, skb, ret);
                }
	}

	return pfq_call(fun, skb, drop());
}

ret_t
filter_flow(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun;

        if (is_skip(ret) || is_drop(ret))
                return pfq_call(get_next_function(skb), skb, ret);

        fun = get_next_function(skb);

	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
 			return pfq_call(fun, skb, drop());

		if (ip->protocol != IPPROTO_UDP &&
		    ip->protocol != IPPROTO_TCP)
			return pfq_call(fun, skb, drop());

		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp)
		{
                        return pfq_call(fun, skb, ret);
                }
	}

	return pfq_call(fun, skb, drop());
}


ret_t
comb_neg(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun = get_next_function(skb);

        if (is_skip(ret))
                return pfq_call(get_next_function(skb), skb, ret);

        return pfq_call(fun, skb, is_drop(ret) ? pass() : drop());
}



ret_t
comb_par(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun = get_next_function(skb);

        if (is_skip(ret))
                return pfq_call(get_next_function(skb), skb, ret);

        if (is_drop(ret))
                return pfq_call(fun, skb, pass());
        else
                return pfq_call(fun, skb, skip(ret));
}


ret_t
comb_pend(struct sk_buff *skb, ret_t ret)
{
        return pfq_call(get_next_function(skb), skb, clear_skip(ret));
}


/* regression test */

struct pair { int a; int b; };


ret_t
dummy_state_context(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun = get_next_function(skb);

        struct pair *p = (struct pair *)get_unsafe_context(skb);

        if (printk_ratelimit())
                printk(KERN_INFO "[PFQ][dummy_context] -> pair = %d %d\n", p->a, p->b);

        set_state(skb, 42);

        return pfq_call(fun, skb, ret);
}

struct sk_function_descr default_functions[] = {
	{ "steer-mac",           steering_mac        },
        { "steer-vlan-id",       steering_vlan_id    },
        { "steer-ipv4",          steering_ipv4       },
        { "steer-ipv6",          steering_ipv6       },
        { "steer-flow",          steering_flow       },
        { "legacy",              fun_legacy          },
        { "clone",               fun_clone           },
        { "broadcast",           fun_broadcast       },
        { "sink",                fun_sink            },
        { "id",                  fun_id              },
        { "vlan",                filter_vlan         },
        { "ipv4",                filter_ipv4         },
        { "udp",                 filter_udp          },
        { "tcp",                 filter_tcp          },
        { "icmp",                filter_icmp         },
        { "flow",                filter_flow         },
        { "strict-vlan",         strict_vlan         },
        { "strict-ipv4",         strict_ipv4         },
        { "strict-udp",          strict_udp          },
        { "strict-tcp",          strict_tcp          },
        { "strict-icmp",         strict_icmp         },
        { "strict-flow",         strict_flow         },
        { "neg",                 comb_neg            },
        { "par",                 comb_par            },
        { "pend",                comb_pend           },
        /* ---------------------------------------- */
        { "dummy-state",         dummy_state_context },
        { NULL, NULL}};

