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

#include <core/lang/module.h>
#include <core/lang/headers.h>
#include <core/lang/misc.h>

#include <pfq/sparse.h>


static ActionQbuff
inc_counter(arguments_t args, struct qbuff * buff)
{
        const int idx = GET_ARG(int,args);
        struct core_group_counters * ctrs = get_group_counters(buff);

	if (idx < 0 || idx >= Q_MAX_COUNTERS) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] counter[%d]: bad index!\n", idx);
	}
	else {
		local_inc(&ctrs->value[idx]);
	}

        return Pass(buff);
}


static ActionQbuff
dec_counter(arguments_t args, struct qbuff * buff)
{
        const int idx = GET_ARG(int,args);
        struct core_group_counters * ctrs = get_group_counters(buff);

	if (idx < 0 || idx >= Q_MAX_COUNTERS) {
                if (printk_ratelimit())
                        printk(KERN_INFO "[pfq-lang] counter[%d]: bad index!\n", idx);
	}
	else {
		local_dec(&ctrs->value[idx]);
	}

        return Pass(buff);
}


static ActionQbuff
log_msg(arguments_t args, struct qbuff * buff)
{
	const char *msg = GET_ARG(const char *, args);

	if (printk_ratelimit())
		printk(KERN_INFO "[pfq-lang] log_msg: %s\n", msg);

	return Pass(buff);
}


static ActionQbuff
log_buff(arguments_t args, struct qbuff * buff)
{
	int maxlen;

	if (!printk_ratelimit())
		return Pass(buff);

	printk(KERN_INFO "[pfq-lang] [%p] len=%u head=%u tail=%u\n", buff,
								qbuff_len(buff),
								qbuff_headroom(buff),
								qbuff_tailroom(buff));

	maxlen = (int)min(qbuff_len(buff), 34U);
	printk(KERN_INFO "[pfq-lang] [%*ph ...]\n", maxlen, (unsigned char *)qbuff_eth_hdr(buff));
	return Pass(buff);
}


static void
log_ip4_packet(arguments_t args, struct qbuff * buff)
{
	struct iphdr _iph;
	const struct iphdr *ip;

	ip = qbuff_ip_header_pointer(buff, 0, sizeof(_iph), &_iph);
	if (ip)
	{
		switch(ip->protocol)
		{
		case IPPROTO_UDP: {
			struct udphdr _udph; const struct udphdr *udp;
			udp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(struct udphdr), &_udph);
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
			tcp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(struct tcphdr), &_tcph);
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
			icmp = qbuff_ip_header_pointer(buff, (ip->ihl<<2), sizeof(struct icmphdr), &_icmp);
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


static ActionQbuff
log_packet(arguments_t args, struct qbuff * buff)
{
	if (!printk_ratelimit())
		return Pass(buff);

	switch(qbuff_ip_version(buff))
	{
	case 4: log_ip4_packet(args, buff); break;
	}

        return Pass(buff);
}

static ActionQbuff
trace(arguments_t args, struct qbuff * buff)
{
	struct pfq_lang_monad *mon = buff->monad;

	qbuff_ip_version(buff);

	if (printk_ratelimit())
	{
		printk(KERN_INFO "[pfq-lang] TRACE SKB: counter:%u state:%u group_mask:%lx (num_devs=%zu kernel:%d)\n"
					, buff->counter
					, buff->state
					, buff->group_mask
					, buff->log->num_devs
					, buff->log->to_kernel
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
	return Pass(buff);
}


struct pfq_lang_function_descr misc_functions[] = {

        { "inc",	"CInt    -> Qbuff -> Action Qbuff",	inc_counter, NULL, NULL	},
        { "dec",	"CInt    -> Qbuff -> Action Qbuff",	dec_counter, NULL, NULL	},
	{ "mark",	"Word32  -> Qbuff -> Action Qbuff",	mark	   , NULL, NULL },
	{ "put_state",	"Word32  -> Qbuff -> Action Qbuff",	put_state  , NULL, NULL },

        { "log_msg",	"String -> Qbuff -> Action Qbuff",	log_msg	   , NULL, NULL },
        { "log_buff",   "Qbuff -> Action Qbuff",		log_buff   , NULL, NULL },
        { "log_packet", "Qbuff -> Action Qbuff",		log_packet , NULL, NULL },
        { "trace",	"Qbuff -> Action Qbuff",		trace	   , NULL, NULL },

        { NULL }};


