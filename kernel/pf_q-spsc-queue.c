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

#include <asm/shmparam.h>
#include <linux/skbuff.h>
#include <linux/pf_q.h>

#include <pf_q-common.h>

#if 0
int
pfq_spsc_queue_alloc(struct pfq_tx_opt *to)
{
        /* calculate the size of the buffer */

        int slot_size = PFQ_SLOT_ALIGN( sizeof(struct pfq_pkt_hdr) + SKB_DATA_ALIGN(1520), 64);

        int tot_mem = PAGE_ALIGN(sizeof(struct pfq_tx_queue_hdr) + PFQ_TX_RING_SIZE * slot_size);

        /* align bufflen to page size */

        int num_pages = tot_mem / PAGE_SIZE;

        num_pages += (num_pages + (SHMLBA-1)) % SHMLBA;
        tot_mem = num_pages*PAGE_SIZE;

        /* Memory is already zeroed */
        to->q_mem = vmalloc_user(tot_mem);
        if (to->q_mem == NULL)
        {
                printk(KERN_INFO "[PFQ] spsc_queue_alloc: out of memory");
                return -1;
        }

        printk(KERN_INFO "[PFQ] tx slot_size: %d\n", slot_size);
        to->q_tot_mem = tot_mem;
        return 0;
}


void
pfq_spsc_queue_free(struct pfq_tx_opt *to)
{
        vfree(to->q_mem);
        to->q_mem = NULL;
}

#endif
