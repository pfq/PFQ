/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#include <pragma/diagnostic_push>
#include <linux/kernel.h>
#include <linux/version.h>
#include <linux/module.h>
#include <linux/filter.h>
#include <linux/slab.h>
#include <linux/uaccess.h>
#include <net/sock.h>
#include <pragma/diagnostic_pop>

#include <pfq/bpf.h>

struct sk_filter *
pfq_alloc_sk_filter(struct sock_fprog *fprog)
{
	struct sock sk;
	int rv;

	sock_init_data(NULL, &sk);
	sk.sk_filter = NULL;
	atomic_set(&sk.sk_omem_alloc, 0);
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(3,9,0))
	sock_reset_flag(&sk, SOCK_FILTER_LOCKED);
#endif
        pr_devel("[PFQ] BPF: new fprog (len %d)\n", fprog->len);

	if ((rv = sk_attach_filter(fprog, &sk))) {
		pr_devel("[PFQ] BPF: sk_attach_filter error: (%d)!\n", rv);
		return NULL;
	}

	return sk.sk_filter;
}


void
pfq_free_sk_filter(struct sk_filter *filter)
{
	struct sock sk;
	int rv;

	sock_init_data(NULL, &sk);
	sk.sk_filter = NULL;
	atomic_set(&sk.sk_omem_alloc, 0);
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(3,9,0))
	sock_reset_flag(&sk, SOCK_FILTER_LOCKED);
#endif
	sk.sk_filter = filter;
	if ((rv = sk_detach_filter(&sk)))
		pr_devel("[PFQ] BPF: sk_detach_filter error: (%d)!\n", rv);
}


