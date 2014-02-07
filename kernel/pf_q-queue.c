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

#include <linux/vmalloc.h>
#include <linux/printk.h>
#include <linux/mm.h>

#include <linux/pf_q.h>
#include <pf_q-common.h>


void * pfq_queue_alloc(struct pfq_rx_opt *rq, size_t queue_mem, size_t *tot_mem)
{
	/* calculate the size of the buffer */

	size_t tm = PAGE_ALIGN(queue_mem);

	/* align bufflen to page size */

	size_t num_pages = tm / PAGE_SIZE; void *addr;

	num_pages += (num_pages + (PAGE_SIZE-1)) & (PAGE_SIZE-1);
	*tot_mem = num_pages*PAGE_SIZE;

	/* Memory is already zeroed */

        addr = vmalloc_user(*tot_mem);
	if (addr == NULL)
	{
		printk(KERN_WARNING "[PFQ|%d] pfq_queue_alloc: out of memory!", rq->id);
		*tot_mem = 0;
		return NULL;
	}

	pr_devel("[PFQ|%d] queue caplen:%lu mem:%lu\n", rq->id, rq->caplen, *tot_mem);
	return addr;
}


void pfq_queue_free(struct pfq_rx_opt *rq)
{
	if (rq->addr) {
		pr_devel("[PFQ|%d] queue freed.\n", rq->id);
		vfree(rq->addr);

		rq->addr = NULL;
		rq->queue_mem = 0;
	}
}


