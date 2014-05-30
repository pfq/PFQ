/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola.bonelli@cnit.it>
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
#include <linux/crc16.h>

#include <pf_q-module.h>
#include <pf_q-sparse.h>

#include "inline.h"
#include "misc.h"

static struct sk_buff *
dummy(arguments_t args, struct sk_buff *skb)
{
        const int data = get_data(int,args);

        if (printk_ratelimit()) {
                printk(KERN_INFO "[PFQ] dummy context: %d\n", data);
        }

        return skb;
}


static int
dummy_init(arguments_t args)
{
	printk(KERN_INFO "[PFQ] %s :)\n", __PRETTY_FUNCTION__);
	return 0;
}

static int
dummy_fini(arguments_t args)
{
	printk(KERN_INFO "[PFQ] %s :(\n", __PRETTY_FUNCTION__);
	return 0;
}


static struct sk_buff *
inc_counter(arguments_t args, struct sk_buff *skb)
{
        const int idx = get_data(int,args);

        sparse_counter_t * ctr;

        ctr = get_counter(skb, idx);
        if (ctr)  {
                sparse_inc(ctr);
        }
        else {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] fun/count(%d): bad index!\n", idx);
        }

        return skb;
}

static struct sk_buff *
dec_counter(arguments_t args, struct sk_buff *skb)
{
        const int idx = get_data(int,args);

        sparse_counter_t * ctr;

        ctr = get_counter(skb, idx);
        if (ctr)  {
                sparse_dec(ctr);
        }
        else {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ] fun/count(%d): bad index!\n", idx);
        }

        return skb;
}


static struct sk_buff *
crc16_sum(arguments_t args, struct sk_buff *skb)
{
	u16 crc = crc16(0, (u8 const *)eth_hdr(skb), skb->len);
	set_state(skb, crc);

        return skb;
}


static struct sk_buff *
log_packet(arguments_t args, struct sk_buff *skb)
{
	if (!printk_ratelimit())
		return skb;

	if (eth_hdr(skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		ip = skb_header_pointer(skb, skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return false;

		switch(ip->protocol)
		{
		case IPPROTO_UDP: {
			struct udphdr _udph; const struct udphdr *udp;
			udp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(struct udphdr), &_udph);
			if (udp == NULL)
				return false;

			printk(KERN_INFO "[PFQ] IP %pI4.%d > %pI4.%d: UDP\n", &ip->saddr, ntohs(udp->source),
						         		      &ip->daddr, ntohs(udp->dest));
			return skb;
		}
		case IPPROTO_TCP: {
			struct tcphdr _tcph; const struct tcphdr *tcp;
			tcp = skb_header_pointer(skb, skb->mac_len + (ip->ihl<<2), sizeof(struct tcphdr), &_tcph);
			if (tcp == NULL)
				return false;

			printk(KERN_INFO "[PFQ] IP %pI4.%d > %pI4.%d: TCP\n", &ip->saddr, ntohs(tcp->source),
									      &ip->daddr, ntohs(tcp->dest));
			return skb;
		}
		case IPPROTO_ICMP: {

			printk(KERN_INFO "[PFQ] IP %pI4 > %pI4: ICMP\n", &ip->saddr, &ip->daddr);
			return skb;
		}
		default: {

			printk(KERN_INFO "[PFQ] IP %pI4 > %pI4: proto %x\n", &ip->saddr, &ip->daddr,
									     ip->protocol);
			return skb;
		}

		}

	} else {

		printk(KERN_INFO "[PFQ] ETH proto %x\n", ntohs(eth_hdr(skb)->h_proto));
	}

        return skb;
}


struct pfq_function_descr misc_functions[] = {

        { "dummy",      "Int -> SkBuff -> Action SkBuff",     	dummy, dummy_init,  dummy_fini },
        { "inc", 	"Int -> SkBuff -> Action SkBuff",     	inc_counter 	},
        { "dec", 	"Int -> SkBuff -> Action SkBuff",    	dec_counter 	},
 	{ "mark", 	"CULong -> SkBuff -> Action SkBuff",  	INLINE_FUN(mark)},
        { "crc16", 	"SkBuff -> Action SkBuff", 		crc16_sum	},
        { "log_packet", "SkBuff -> Action SkBuff", 		log_packet	},

        { NULL }};


