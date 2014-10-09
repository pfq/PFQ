/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
 *             Loris Gazzarrini <loris.gazzarrini@iet.unipi.it>
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
#include <linux/version.h>
#include <linux/module.h>
#include <linux/filter.h>
#include <linux/slab.h>
#include <linux/uaccess.h>

#include <net/sock.h>

#include <pf_q-bpf.h>

struct sk_filter *
pfq_alloc_sk_filter(struct sock_fprog *fprog)
{
       	struct sock sk;
       	int rv;

       	sock_init_data(NULL, &sk);
       	sock_reset_flag(&sk, SOCK_FILTER_LOCKED);

	if ((rv = sk_attach_filter(fprog, &sk))) {
		pr_devel("[PFQ] sk_attach_filter (%d)!\n", rv);
        	return NULL;
	}

	return sk.sk_filter;
}

void pfq_free_sk_filter(struct sk_filter *filter)
{
       	struct sock sk;
       	int rv;

       	sock_init_data(NULL, &sk);
       	sock_reset_flag(&sk, SOCK_FILTER_LOCKED);

	sk.sk_filter = filter;
	if ((rv = sk_detach_filter(&sk))) {
		pr_devel("[PFQ] sk_detach_filter (%d)!\n", rv);
	}
}


