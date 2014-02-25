/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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
#include <linux/module.h>
#include <linux/filter.h>
#include <linux/slab.h>
#include <linux/uaccess.h>


struct sk_filter *
pfq_alloc_sk_filter(struct sock_fprog *fprog)
{
        struct sk_filter *fp;
        unsigned int fsize = sizeof(struct sock_filter) * fprog->len;
        int err;

        /* Make sure new filter is there and in the right amounts. */
        if (fprog->filter == NULL)
                return NULL;

        fp = (struct sk_filter *)kmalloc(fsize+sizeof(*fp), GFP_KERNEL);
        if (!fp)
                return NULL;
        if (copy_from_user(fp->insns, fprog->filter, fsize)) {
        	kfree(fp);
                return NULL;
        }

        fp->len = fprog->len;
        fp->bpf_func = sk_run_filter;

	err = sk_chk_filter(fp->insns, fp->len);
	if (err)
	{
		/* bpf_jit_free(fp); */
		kfree(fp);
		return NULL;
	}

        return fp;
}


void pfq_free_sk_filter(struct sk_filter *filter)
{
        if (filter)
	{
		/* bpf_jit_free(fp); */
		kfree(filter);
	}
}



