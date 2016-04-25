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
#include <linux/ip.h>
#include <linux/ipv6.h>
#include <linux/tcp.h>
#include <linux/udp.h>
#include <linux/icmp.h>
#include <linux/icmpv6.h>
#include <linux/crc16.h>

#include <pragma/diagnostic_pop>

#include <lang/module.h>
#include <lang/headers.h>
#include <lang/misc.h>

#include <pf_q-sparse.h>


static ActionSkBuff
inc_counter(arguments_t args, SkBuff skb)
{
        const int idx = GET_ARG(int,args);
        struct pfq_group_counters * ctrs = get_group_counters(skb);

	if (idx < 0 || idx >= Q_MAX_COUNTERS) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] counter[%d]: bad index!\n", idx);
	}
	else {
		local_inc(&ctrs->value[idx]);
	}

        return Pass(skb);
}


static ActionSkBuff
dec_counter(arguments_t args, SkBuff skb)
{
        const int idx = GET_ARG(int,args);
        struct pfq_group_counters * ctrs = get_group_counters(skb);

	if (idx < 0 || idx >= Q_MAX_COUNTERS) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] counter[%d]: bad index!\n", idx);
	}
	else {
		local_dec(&ctrs->value[idx]);
	}

        return Pass(skb);
}


static ActionSkBuff
crc16_sum(arguments_t args, SkBuff skb)
{
	u16 crc = crc16(0, (u8 const *)eth_hdr(PFQ_SKB(skb)), skb->len);
	set_state(skb, crc);
        return Pass(skb);
}


static ActionSkBuff
log_msg(arguments_t args, SkBuff skb)
{
	const char *msg = GET_ARG(const char *, args);

	if (printk_ratelimit())
		printk(KERN_INFO "[pfq-lang] log_msg: %s\n", msg);

	return Pass(skb);
}


static ActionSkBuff
log_buff(arguments_t args, SkBuff skb)
{
	int maxlen;

	if (!printk_ratelimit())
		return Pass(skb);

	printk(KERN_INFO "[pfq-lang] [%p] len=%u head=%u tail=%u\n", skb,
								skb->len,
								skb_headroom(PFQ_SKB(skb)),
								skb_tailroom(PFQ_SKB(skb)));

	maxlen = (int)min(skb->len, 34U);
	printk(KERN_INFO "[pfq-lang] [%*ph ...]\n", maxlen, skb->data);
	return Pass(skb);
}

static void
log_ip6_packet(arguments_t args, SkBuff skb)
{
	struct ipv6hdr _iph;
	const struct ipv6hdr *ip6;

	ip6 = skb_ip6_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip6)
	{
		switch(ip6->nexthdr)
		{
		case IPPROTO_UDP: {
			struct udphdr _udph; const struct udphdr *udp;
			udp = skb_ip6_header_pointer(skb, sizeof(struct ipv6hdr), sizeof(struct udphdr), &_udph);
			if (udp)
			{
				printk(KERN_INFO "[pfq-lang] IP6 %pI6.%d > %pI6.%d: UDP\n",
							&ip6->saddr, be16_to_cpu(udp->source),
							&ip6->daddr, be16_to_cpu(udp->dest));
			}
			else
			{
				printk(KERN_INFO "[pfq-lang] IP6 %pI6 > %pI6: UDP (broken)\n", &ip6->saddr, &ip6->daddr);
			}
		} break;
		case IPPROTO_TCP: {
			struct tcphdr _tcph; const struct tcphdr *tcp;
			tcp = skb_ip6_header_pointer(skb, sizeof(struct ipv6hdr), sizeof(struct tcphdr), &_tcph);
			if (tcp)
			{
				printk(KERN_INFO "[pfq-lang] IP6 %pI6.%d > %pI6.%d: TCP\n",
							&ip6->saddr, be16_to_cpu(tcp->source),
							&ip6->daddr, be16_to_cpu(tcp->dest));
			}
			else
			{
				printk(KERN_INFO "[pfq-lang] IP6 %pI6 > %pI6: TCP (broken)\n", &ip6->saddr, &ip6->daddr);
			}
		} break;
		case IPPROTO_ICMPV6: {
			struct icmp6hdr _icmp6; const struct icmp6hdr *icmp6;
			icmp6 = skb_ip6_header_pointer(skb, sizeof(struct ipv6hdr), sizeof(struct icmp6hdr), &_icmp6);
                        if (icmp6)
			{
				printk(KERN_INFO "[pfq-lang] IP6 %pI6 > %pI6: ICMP6 type=%d (code=%d)\n",
							&ip6->saddr, &ip6->daddr, icmp6->icmp6_type, icmp6->icmp6_code);
			}
			else
			{
				printk(KERN_INFO "[pfq-lang] IP6 %pI6 > %pI6: ICMP6 (broken)\n", &ip6->saddr, &ip6->daddr);
			}
		} break;
		default: {

			printk(KERN_INFO "[pfq-lang] IP6 %pI6 > %pI6: proto %x\n",
							&ip6->saddr, &ip6->daddr, ip6->nexthdr);
		} break;
		}
	}
	else
	{
		printk(KERN_INFO "[pfq-lang] IP6 (broken)\n");
	}
}


static void
log_ip4_packet(arguments_t args, SkBuff skb)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = skb_ip_header_pointer(skb, 0, sizeof(_iph), &_iph);
	if (ip)
	{
		switch(ip->protocol)
		{
		case IPPROTO_UDP: {
			struct udphdr _udph; const struct udphdr *udp;
			udp = skb_ip_header_pointer(skb, (ip->ihl<<2), sizeof(struct udphdr), &_udph);
			if (udp)
			{
				printk(KERN_INFO "[pfq-lang] IP4 %pI4.%d > %pI4.%d: UDP\n",
							&ip->saddr, be16_to_cpu(udp->source),
							&ip->daddr, be16_to_cpu(udp->dest));
			}
			else
			{
				printk(KERN_INFO "[pfq-lang] IP4 %pI4 > %pI4: UDP (broken)\n", &ip->saddr, &ip->daddr);
			}
		} break;
		case IPPROTO_TCP: {
			struct tcphdr _tcph; const struct tcphdr *tcp;
			tcp = skb_ip_header_pointer(skb, (ip->ihl<<2), sizeof(struct tcphdr), &_tcph);
			if (tcp)
			{
				printk(KERN_INFO "[pfq-lang] IP4 %pI4.%d > %pI4.%d: TCP\n",
							&ip->saddr, be16_to_cpu(tcp->source),
							&ip->daddr, be16_to_cpu(tcp->dest));
			}
			else
			{
				printk(KERN_INFO "[pfq-lang] IP4 %pI4 > %pI4: TCP (broken)\n", &ip->saddr, &ip->daddr);
			}
		} break;
		case IPPROTO_ICMP: {
			struct icmphdr _icmp; const struct icmphdr *icmp;
			icmp = skb_ip_header_pointer(skb, (ip->ihl<<2), sizeof(struct icmphdr), &_icmp);
                        if (icmp)
			{
				printk(KERN_INFO "[pfq-lang] IP4 %pI4 > %pI4: ICMP type=%d (code=%d)\n",
							&ip->saddr, &ip->daddr, icmp->type, icmp->code);
			}
			else
			{
				printk(KERN_INFO "[pfq-lang] IP4 %pI4 > %pI4: ICMP (broken)\n", &ip->saddr, &ip->daddr);
			}
		} break;
		default: {

			printk(KERN_INFO "[pfq-lang] IP4 %pI4 > %pI4: proto %x\n",
							&ip->saddr, &ip->daddr, ip->protocol);
		} break;
		}
	}
	else
	{
		printk(KERN_INFO "[pfq-lang] IP4 (broken)\n");
	}
}


static ActionSkBuff
log_packet(arguments_t args, SkBuff skb)
{
	if (!printk_ratelimit())
		return Pass(skb);

	switch(skb_ip_version(skb))
	{
	case 4: log_ip4_packet(args, skb); break;
	case 6: log_ip6_packet(args, skb); break;
	}

        return Pass(skb);
}

static ActionSkBuff
trace(arguments_t args, SkBuff skb)
{
	struct pfq_lang_monad *mon = PFQ_CB(skb)->monad;

	skb_ip_version(skb);

	if (printk_ratelimit())
		printk(KERN_INFO "[pfq-lang] TRACE state:%u fanout:{%lu %u %u %u} shift:%d ipoff:%d ipproto:%d\n"
					, mon->state
					, mon->fanout.class_mask
					, mon->fanout.hash
					, mon->fanout.hash2
					, mon->fanout.type
					, mon->shift
					, mon->ipoff
					, mon->ipproto
					);

	return Pass(skb);
}


struct pfq_lang_function_descr misc_functions[] = {

        { "inc",	"CInt    -> SkBuff -> Action SkBuff",	inc_counter	},
        { "dec",	"CInt    -> SkBuff -> Action SkBuff",	dec_counter	},
	{ "mark",	"Word32  -> SkBuff -> Action SkBuff",	mark		},
	{ "put_state",	"Word32  -> SkBuff -> Action SkBuff",	put_state	},

        { "crc16",	"SkBuff -> Action SkBuff",		crc16_sum	},
        { "log_msg",	"String -> SkBuff -> Action SkBuff",	log_msg		},
        { "log_buff",   "SkBuff -> Action SkBuff",		log_buff	},
        { "log_packet", "SkBuff -> Action SkBuff",		log_packet	},
        { "trace",	"SkBuff -> Action SkBuff",		trace		},

        { NULL }};


