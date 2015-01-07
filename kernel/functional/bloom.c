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

#include <pf_q-module.h>

#include "bloom.h"


static bool
bloom_src(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;
		unsigned int mask;
		char *mem;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		mem = get_arg1(char *, args);
        	mask = get_arg0(unsigned int, args);

		if ( BF_TEST(mem, hfun1(ip->saddr) & mask ) &&
		     BF_TEST(mem, hfun2(ip->saddr) & mask ) &&
		     BF_TEST(mem, hfun3(ip->saddr) & mask ) &&
		     BF_TEST(mem, hfun4(ip->saddr) & mask ) )
		     	return true;
	}

        return false;
}


static bool
bloom_dst(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;
		unsigned int mask;
		char *mem;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		mem = get_arg1(char *, args);
        	mask = get_arg0(unsigned int, args);

		if ( BF_TEST(mem, hfun1(ip->daddr) & mask ) &&
		     BF_TEST(mem, hfun2(ip->daddr) & mask ) &&
		     BF_TEST(mem, hfun3(ip->daddr) & mask ) &&
		     BF_TEST(mem, hfun4(ip->daddr) & mask ) )
		     	return true;
	}

        return false;
}

static bool
bloom(arguments_t args, SkBuff b)
{
	if (eth_hdr(b.skb)->h_proto == __constant_htons(ETH_P_IP))
	{
		struct iphdr _iph;
    		const struct iphdr *ip;
		unsigned int mask;
		char *mem;

		ip = skb_header_pointer(b.skb, b.skb->mac_len, sizeof(_iph), &_iph);
 		if (ip == NULL)
                        return false;

		mem  = get_arg1(char *, args);
        	mask = get_arg0(unsigned int, args);

		if ( BF_TEST(mem, hfun1(ip->daddr) & mask ) &&
		     BF_TEST(mem, hfun2(ip->daddr) & mask ) &&
		     BF_TEST(mem, hfun3(ip->daddr) & mask ) &&
		     BF_TEST(mem, hfun4(ip->daddr) & mask ) )
		     	return true;

		if ( BF_TEST(mem, hfun1(ip->saddr) & mask ) &&
		     BF_TEST(mem, hfun2(ip->saddr) & mask ) &&
		     BF_TEST(mem, hfun3(ip->saddr) & mask ) &&
		     BF_TEST(mem, hfun4(ip->saddr) & mask ) )
		     	return true;
	}

        return false;
}


static Action_SkBuff
bloom_filter(arguments_t args, SkBuff b)
{
	if (bloom(args, b))
		return Pass(b);
	return Drop(b);
}


static Action_SkBuff
bloom_src_filter(arguments_t args, SkBuff b)
{
	if (bloom_src(args, b))
		return Pass(b);
	return Drop(b);
}

static Action_SkBuff
bloom_dst_filter(arguments_t args, SkBuff b)
{
	if (bloom_dst(args, b))
		return Pass(b);
	return Drop(b);
}


static int bloom_init(arguments_t args)
{
	unsigned int m = get_arg0(int, args);
	unsigned int n = get_len_array1(args);
	uint32_t *ips  = get_array1(uint32_t, args);
	unsigned int i;
	size_t size;
	char *mem;

	m = clp2(m);

	set_arg0(args, m-1); 	/* fold mask */

	if (m > (1UL << 24)) {
	       	printk(KERN_INFO "[PFQ|init] bloom filter: maximum number of bins exceeded (2^24)!\n");
	       	return -EPERM;
	}

	size = (m >> 3);

        mem = kzalloc(size, GFP_KERNEL);
        if (!mem) {
	       	printk(KERN_INFO "[PFQ|init] bloom filter: out of memory!\n");
	       	return -ENOMEM;
	}

	set_arg1(args, mem);

	pr_devel("[PFQ|init] bloom filter@%p: k=4, n=%d, m=%d size=%zu bytes.\n", mem, n, m, size);

	for(i = 0; i < n; i++)
	{
		uint32_t h1 = hfun1(ips[i]) & (m-1);
		uint32_t h2 = hfun2(ips[i]) & (m-1);
		uint32_t h3 = hfun3(ips[i]) & (m-1);
		uint32_t h4 = hfun4(ips[i]) & (m-1);

		BF_SET(mem, h1);
		BF_SET(mem, h2);
		BF_SET(mem, h3);
		BF_SET(mem, h4);

		pr_devel("[PFQ|init] bloom filter: -> address %pI4\n", ips+i);
	}

	return 0;
}


static int bloom_fini(arguments_t args)
{
	char *mem = get_arg1(char *, args);

	kfree(mem);

	pr_devel("[PFQ|init] bloom filter: memory freed@%p!\n", mem);

	return 0;
}


struct pfq_function_descr bloom_functions[] = {

        { "bloom",	  	"CInt -> [Word32] -> SkBuff -> Bool", 		bloom, 			bloom_init, 	bloom_fini },
        { "bloom_src",	  	"CInt -> [Word32] -> SkBuff -> Bool", 		bloom_src, 		bloom_init, 	bloom_fini },
        { "bloom_dst",	  	"CInt -> [Word32] -> SkBuff -> Bool", 		bloom_dst, 		bloom_init, 	bloom_fini },
        { "bloom_filter", 	"CInt -> [Word32] -> SkBuff -> Action SkBuff", 	bloom_filter, 		bloom_init, 	bloom_fini },
        { "bloom_src_filter", 	"CInt -> [Word32] -> SkBuff -> Action SkBuff", 	bloom_src_filter, 	bloom_init, 	bloom_fini },
        { "bloom_dst_filter", 	"CInt -> [Word32] -> SkBuff -> Action SkBuff", 	bloom_dst_filter, 	bloom_init, 	bloom_fini },
        { NULL }};

