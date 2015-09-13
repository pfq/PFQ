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

#include <pragma/diagnostic_push>

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/crc16.h>

#include <pragma/diagnostic_pop>

#include <pf_q-module.h>
#include <pf_q-sparse.h>

#include "headers.h"
#include "misc.h"



static ActionSkBuff
inc_counter(arguments_t args, SkBuff skb)
{
        const int idx = GET_ARG(int,args);

        sparse_counter_t * ctr;

        ctr = get_counter(skb, idx);
        if (ctr)  {
                sparse_inc(ctr);
        }
        else {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ/lang] counter[%d]: bad index!\n", idx);
        }

        return Pass(skb);
}


static ActionSkBuff
dec_counter(arguments_t args, SkBuff skb)
{
        const int idx = GET_ARG(int,args);

        sparse_counter_t * ctr;

        ctr = get_counter(skb, idx);
        if (ctr)  {
                sparse_dec(ctr);
        }
        else {
                if (printk_ratelimit())
                        printk(KERN_INFO "[PFQ/lang] counter[%d]: bad index!\n", idx);
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
		printk(KERN_INFO "[PFQ/lang] log_msg: %s\n", msg);

	return Pass(skb);
}


static ActionSkBuff
log_buff(arguments_t args, SkBuff skb)
{
	if (!printk_ratelimit())
		return Pass(skb);

	printk(KERN_INFO "[PFQ/lang] [%p] len=%u head=%u tail=%u\n", skb,
								skb->len,
								skb_headroom(PFQ_SKB(skb)),
								skb_tailroom(PFQ_SKB(skb)));

	printk(KERN_INFO "      [%p] %2x %2x %2x %2x %2x %2x %2x %2x %2x %2x %2x %2x %2x %2x...\n",
				skb->data,
				skb->data[0],
				skb->data[1],
				skb->data[2],
				skb->data[3],
				skb->data[4],
				skb->data[5],
				skb->data[6],
				skb->data[7],
				skb->data[8],
				skb->data[9],
				skb->data[10],
				skb->data[11],
				skb->data[12],
				skb->data[13]);

	return Pass(skb);
}


static ActionSkBuff
log_packet(arguments_t args, SkBuff skb)
{
	if (!printk_ratelimit())
		return Pass(skb);

	if (eth_hdr(PFQ_SKB(skb))->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
		const struct iphdr *ip;

		ip = skb_header_pointer(PFQ_SKB(skb), skb->mac_len, sizeof(_iph), &_iph);
		if (ip == NULL)
			return Pass(skb);

		switch(ip->protocol)
		{
		case IPPROTO_UDP: {
			struct udphdr _udph; const struct udphdr *udp;
			udp = skb_header_pointer(PFQ_SKB(skb), skb->mac_len + (ip->ihl<<2), sizeof(struct udphdr), &_udph);
			if (udp == NULL)
				return Pass(skb);

			printk(KERN_INFO "[PFQ/lang] IP %pI4.%d > %pI4.%d: UDP\n",
						&ip->saddr, ntohs(udp->source),
						&ip->daddr, ntohs(udp->dest));
			return Pass(skb);
		}
		case IPPROTO_TCP: {
			struct tcphdr _tcph; const struct tcphdr *tcp;
			tcp = skb_header_pointer(PFQ_SKB(skb), skb->mac_len + (ip->ihl<<2), sizeof(struct tcphdr), &_tcph);
			if (tcp == NULL)
				return Pass(skb);

			printk(KERN_INFO "[PFQ/lang] IP %pI4.%d > %pI4.%d: TCP\n", &ip->saddr, ntohs(tcp->source),
									      &ip->daddr, ntohs(tcp->dest));
			return Pass(skb);
		}
		case IPPROTO_ICMP: {

			printk(KERN_INFO "[PFQ/lang] IP %pI4 > %pI4: ICMP\n", &ip->saddr, &ip->daddr);
			return Pass(skb);
		}
		default: {

			printk(KERN_INFO "[PFQ/lang] IP %pI4 > %pI4: proto %x\n", &ip->saddr, &ip->daddr,
									     ip->protocol);
			return Pass(skb);
		}

		}

	} else
		printk(KERN_INFO "[PFQ/lang] ETH proto %x\n", ntohs(eth_hdr(PFQ_SKB(skb))->h_proto));

        return Pass(skb);
}


static ActionSkBuff
inv(arguments_t args, SkBuff skb)
{
	function_t expr = GET_ARG(function_t, args);
	SkBuff nskb = EVAL_FUNCTION(expr, skb).skb;

	if (!nskb || is_drop(PFQ_CB(nskb)->monad->fanout))
		return Copy(nskb);

	return Drop(nskb);
}


static ActionSkBuff
par(arguments_t args, SkBuff skb)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	fanout_t fout = PFQ_CB(skb)->monad->fanout;
        ActionSkBuff a;

	a = EVAL_FUNCTION(f0, skb);
	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	return EVAL_FUNCTION(f1, skb);
}


static ActionSkBuff
par3(arguments_t args, SkBuff skb)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	fanout_t fout = PFQ_CB(skb)->monad->fanout;
        ActionSkBuff a;

	a = EVAL_FUNCTION(f0, skb);
	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	return EVAL_FUNCTION(f2, skb);
}


static ActionSkBuff
par4(arguments_t args, SkBuff skb)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	function_t f3 = GET_ARG_3(function_t, args);
	fanout_t fout = PFQ_CB(skb)->monad->fanout;
        ActionSkBuff a;

	a = EVAL_FUNCTION(f0, skb);
	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f2, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	return EVAL_FUNCTION(f3, skb);
}


static ActionSkBuff
par5(arguments_t args, SkBuff skb)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	function_t f3 = GET_ARG_3(function_t, args);
	function_t f4 = GET_ARG_4(function_t, args);
	fanout_t fout = PFQ_CB(skb)->monad->fanout;
        ActionSkBuff a;

	a = EVAL_FUNCTION(f0, skb);
	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f2, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f3, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	return EVAL_FUNCTION(f4, skb);
}

static ActionSkBuff
par6(arguments_t args, SkBuff skb)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	function_t f3 = GET_ARG_3(function_t, args);
	function_t f4 = GET_ARG_4(function_t, args);
	function_t f5 = GET_ARG_5(function_t, args);
	fanout_t fout = PFQ_CB(skb)->monad->fanout;
        ActionSkBuff a;

	a = EVAL_FUNCTION(f0, skb);
	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f2, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f3, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f4, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	return EVAL_FUNCTION(f5, skb);
}


static ActionSkBuff
par7(arguments_t args, SkBuff skb)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	function_t f3 = GET_ARG_3(function_t, args);
	function_t f4 = GET_ARG_4(function_t, args);
	function_t f5 = GET_ARG_5(function_t, args);
	function_t f6 = GET_ARG_6(function_t, args);
	fanout_t fout = PFQ_CB(skb)->monad->fanout;
        ActionSkBuff a;

	a = EVAL_FUNCTION(f0, skb);
	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f2, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f3, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f4, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f5, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	return EVAL_FUNCTION(f6, skb);
}


static ActionSkBuff
par8(arguments_t args, SkBuff skb)
{
	function_t f0 = GET_ARG_0(function_t, args);
	function_t f1 = GET_ARG_1(function_t, args);
	function_t f2 = GET_ARG_2(function_t, args);
	function_t f3 = GET_ARG_3(function_t, args);
	function_t f4 = GET_ARG_4(function_t, args);
	function_t f5 = GET_ARG_5(function_t, args);
	function_t f6 = GET_ARG_6(function_t, args);
	function_t f7 = GET_ARG_7(function_t, args);
	fanout_t fout = PFQ_CB(skb)->monad->fanout;
        ActionSkBuff a;

	a = EVAL_FUNCTION(f0, skb);
	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f1, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f2, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f3, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f4, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f5, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	a = EVAL_FUNCTION(f6, skb);

	if (a.skb && !is_drop(PFQ_CB(a.skb)->monad->fanout))
		return a;

	PFQ_CB(skb)->monad->fanout = fout;
	return EVAL_FUNCTION(f7, skb);
}


struct pfq_function_descr misc_functions[] = {

        { "inc",	"CInt    -> SkBuff -> Action SkBuff",	inc_counter	},
        { "dec",	"CInt    -> SkBuff -> Action SkBuff",	dec_counter	},
	{ "mark",	"Word32  -> SkBuff -> Action SkBuff",	mark		},
	{ "put_state",	"Word32  -> SkBuff -> Action SkBuff",	put_state	},

        { "crc16",	"SkBuff -> Action SkBuff",		crc16_sum	},
        { "log_msg",	"String -> SkBuff -> Action SkBuff",	log_msg		},
        { "log_buff",   "SkBuff -> Action SkBuff",		log_buff	},
        { "log_packet", "SkBuff -> Action SkBuff",		log_packet	},

        { "inv",	"(SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", inv },

        { "par",	"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", par },

	{ "par3",	"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", par3 },
	{ "par4",	"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", par4 },
	{ "par5",	"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", par5 },
	{ "par6",	"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", par6 },
	{ "par7",	"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", par7 },
	{ "par8",	"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> "
			"(SkBuff -> Action SkBuff) -> (SkBuff -> Action SkBuff) -> SkBuff -> Action SkBuff", par8 },
        { NULL }};


