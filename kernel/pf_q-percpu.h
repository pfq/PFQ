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

#ifndef _PF_Q_PERCPU_H_
#define _PF_Q_PERCPU_H_

#include <linux/percpu.h>

#include <pf_q-skbuff-list.h>
#include <pf_q-macro.h>
#include <pf_q-GC.h>

int pfq_percpu_init(void);
int pfq_percpu_flush(void);

/* per-cpu data... */

struct local_data
{
        unsigned long           eligible_mask;
        unsigned long           sock_mask [Q_MAX_ID];

        int                     sock_cnt;

	struct gc_data 		gc;	/* garbage collector */

        atomic_t                enable_skb_pool;

        struct pfq_sk_buff_list tx_pool;
        struct pfq_sk_buff_list rx_pool;

} ____cacheline_aligned;

#endif /* _PF_Q_PERCPU_H_ */
