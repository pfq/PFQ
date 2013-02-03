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

#include <linux/pf_q-fun.h>

ret_t
steering_mac(struct sk_buff *skb, ret_t ret)
{
        uint16_t * a; 
        
        if (ret.type == action_drop)
                return drop();
        
        a = (uint16_t *)eth_hdr(skb); 
	
	return steering(Q_CLASS_DEFAULT, a[0] ^ a[1] ^ a[2] ^ a[3] ^ a[4] ^ a[5] );		
}


ret_t
steering_vlan_untagged(struct sk_buff *skb, ret_t ret)
{
        if (ret.type == action_drop)
                return drop();
	
	return steering(Q_CLASS_DEFAULT, skb->vlan_tci == 0);
}


ret_t
steering_vlan_id(struct sk_buff *skb, ret_t ret)
{
        if (ret.type == action_drop)
                return drop();
 	
 	return steering(Q_CLASS_DEFAULT, skb->vlan_tci & VLAN_VID_MASK);
}


ret_t
steering_ipv4(struct sk_buff *skb, ret_t ret)
{       
        if (ret.type == action_drop)
                return drop();
	
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP)) 
	{ 
		struct iphdr _iph;
    		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
 			return drop();

        	return steering(Q_CLASS_DEFAULT, ip->saddr ^ ip->daddr);
	}

	return drop();
}


ret_t
steering_flow(struct sk_buff *skb, ret_t ret)
{       
        if (ret.type == action_drop)
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
		if (udp == NULL) 
			return drop();
		
        	return steering(Q_CLASS_DEFAULT, ip->saddr ^ ip->daddr ^ udp->source ^ udp->dest);
	}

	return drop();
}


ret_t
steering_ipv6(struct sk_buff *skb, ret_t ret)
{       
        if (ret.type == action_drop)
                return drop();
	
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IPV6)) 
	{ 
		struct ipv6hdr _ip6h;
    		const struct ipv6hdr *ip6;

		ip6 = skb_header_pointer(skb, skb->mac_len, sizeof(_ip6h), &_ip6h);
 		if (ip6 == NULL)
 			return drop();

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

	return drop();
}


ret_t
fun_legacy(struct sk_buff *skb, ret_t ret)
{
        if (ret.type == action_drop)
                return drop();
        
        return to_kernel(drop());
}


ret_t
fun_transparent(struct sk_buff *skb, ret_t ret)
{
        if (ret.type == action_drop)
                return drop();
        
        return to_kernel(broadcast(Q_CLASS_ANY));
}


ret_t
fun_clone(struct sk_buff *skb, ret_t ret)
{
        if (ret.type == action_drop)
                return drop();
        
        return broadcast(Q_CLASS_ANY);
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


ret_t
fun_state_id(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun;

        get_state(skb);    
        
        fun = get_next_function(skb);
        
        put_state(skb);

        return pfq_call(fun, skb, ret); 
}

/* filters */

ret_t
strict_ipv4(struct sk_buff *skb, ret_t ret)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP)) 
	{ 
		struct iphdr _iph;
    		const struct iphdr *ip;

                sk_function_t fun;
		
		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip)
                {
                        fun = get_next_function(skb);
                        return pfq_call(fun, skb, ret); 
                }
        }       

        return drop();
}


ret_t
strict_udp(struct sk_buff *skb, ret_t ret)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP)) 
	{ 
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;
                
                sk_function_t fun;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
 			return drop();

		if (ip->protocol != IPPROTO_UDP) 
			return drop();
		
		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp) 
		{
                        fun = get_next_function(skb);
                        return pfq_call(fun, skb, ret); 
                }
	}

	return drop();
}


ret_t
strict_tcp(struct sk_buff *skb, ret_t ret)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP)) 
	{ 
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct tcphdr _tcp;
		const struct tcphdr *tcp;
                
                sk_function_t fun;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
 			return drop();

		if (ip->protocol != IPPROTO_TCP) 
			return drop();
		
		tcp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_tcp), &_tcp);
		if (tcp)
                {
                        fun = get_next_function(skb);
                        return pfq_call(fun, skb, ret); 
                }
	}

	return drop();
}


ret_t
strict_flow(struct sk_buff *skb, ret_t ret)
{
	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP)) 
	{ 
		struct iphdr _iph;
    		const struct iphdr *ip;

		struct udphdr _udp;
		const struct udphdr *udp;
                
                sk_function_t fun;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
 			return drop();

		if (ip->protocol != IPPROTO_UDP &&
		    ip->protocol != IPPROTO_TCP) 
			return drop();
		
		udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(_udp), &_udp);
		if (udp) 
		{
                        fun = get_next_function(skb);
                        return pfq_call(fun, skb, ret); 
                }
	}

	return drop();
}


ret_t
filter_ipv4(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun = get_next_function(skb);
	
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
        sk_function_t fun = get_next_function(skb);
	
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
        sk_function_t fun = get_next_function(skb);

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
filter_flow(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun = get_next_function(skb);

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
        return pfq_call(fun, skb, ret.type == action_drop ? null() : drop());
}


ret_t
comb_par(struct sk_buff *skb, ret_t ret)
{
        sk_function_t fun = get_next_function(skb);

        if (ret.type == action_drop)
                return pfq_call(fun, skb, null());

        fun = get_next_function(skb);
        return pfq_call(fun, skb, ret);
}


struct sk_function_descr default_functions[] = {
        { "steer-vlan-untagged", steering_vlan_untagged },
	{ "steer-mac",           steering_mac        },
        { "steer-vlan-id",       steering_vlan_id    },
        { "steer-ipv4",          steering_ipv4       },
        { "steer-ipv6",          steering_ipv6       },
        { "steer-flow",          steering_flow       },
        { "legacy",              fun_legacy          },
        { "transparent",         fun_transparent     },
        { "clone",               fun_clone           },
        { "broadcast",           fun_clone           },
        { "sink",                fun_sink            },
        { "id",                  fun_id              },
        { "state",               fun_state_id        },
        { "ipv4",                filter_ipv4         },
        { "udp",                 filter_udp          },
        { "tcp",                 filter_tcp          },
        { "flow",                filter_flow         },
        { "strict-ipv4",         strict_ipv4         },
        { "strict-udp",          strict_udp          },
        { "strict-tcp",          strict_tcp          },
        { "strict-flow",         strict_flow         },
        { "neg",                 comb_neg            },
        { "par",                 comb_par            },
	{ NULL, NULL}};

