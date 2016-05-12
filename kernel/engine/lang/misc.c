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

#include <engine/lang/module.h>
#include <engine/lang/headers.h>
#include <engine/lang/misc.h>

#include <pfq/sparse.h>


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
	case IPPROTO_IP: {
		log_ip4_packet(args, skb);
	} break;
	}

        return Pass(skb);
}

static ActionSkBuff
trace(arguments_t args, SkBuff skb)
{
	struct pfq_lang_monad *mon = PFQ_CB(skb)->monad;

	skb_ip_version(skb);

	if (printk_ratelimit())
	{
		printk(KERN_INFO "[pfq-lang] TRACE SKB: counter:%u state:%u direct:%d group_mask:%lx (num_devs=%zu kernel:%d)\n"
					, PFQ_CB(skb)->counter
					, PFQ_CB(skb)->state
					, PFQ_CB(skb)->direct
					, PFQ_CB(skb)->group_mask
					, PFQ_CB(skb)->log->num_devs
					, PFQ_CB(skb)->log->to_kernel
					);

		printk(KERN_INFO "[pfq-lang]     MONAD: state:%u fanout:{cl=%lx h1=%u h2=%u tp=%u} shift:%d ipoff:%d ipproto:%d ep_ctx:%d\n"
					, mon->state
					, mon->fanout.class_mask
					, mon->fanout.hash
					, mon->fanout.hash2
					, mon->fanout.type
					, mon->shift
					, mon->ipoff
					, mon->ipproto
					, mon->ep_ctx
					);

	}
	return Pass(skb);
}


struct pfq_lang_function_descr misc_functions[] = {

        { "inc",	"CInt    -> SkBuff -> Action SkBuff",	inc_counter	},
        { "dec",	"CInt    -> SkBuff -> Action SkBuff",	dec_counter	},
	{ "mark",	"Word32  -> SkBuff -> Action SkBuff",	mark		},
	{ "put_state",	"Word32  -> SkBuff -> Action SkBuff",	put_state	},

        { "log_msg",	"String -> SkBuff -> Action SkBuff",	log_msg		},
        { "log_buff",   "SkBuff -> Action SkBuff",		log_buff	},
        { "log_packet", "SkBuff -> Action SkBuff",		log_packet	},
        { "trace",	"SkBuff -> Action SkBuff",		trace		},

        { NULL }};


