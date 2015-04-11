/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
 *             Andrea Di Pietro <andrea.dipietro@for.unipi.it>
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

#ifndef PF_Q_SHARED_QUEUE_H
#define PF_Q_SHARED_QUEUE_H

#include <linux/skbuff.h>
#include <linux/pf_q.h>
#include <linux/if_vlan.h>

#include <pf_q-group.h>
#include <pf_q-macro.h>
#include <pf_q-sock.h>
#include <pf_q-GC.h>


int pfq_shared_queue_enable(struct pfq_sock *so, unsigned long addr);
int pfq_shared_queue_disable(struct pfq_sock *so);

extern size_t pfq_mpsc_enqueue_batch(struct pfq_rx_opt *ro,
		                     struct pfq_skbuff_batch *skbs,
		                     unsigned long long skbs_mask,
		                     int burst_len,
		                     pfq_gid_t gid);


static inline size_t pfq_queue_mpsc_mem(struct pfq_sock *so)
{
        return so->rx_opt.queue_size * so->rx_opt.slot_size * 2;
}

static inline size_t pfq_queue_spsc_mem(struct pfq_sock *so)
{
        return so->tx_opt.queue_size * so->tx_opt.slot_size * 2;
}


static inline
size_t pfq_mpsc_queue_len(struct pfq_sock *p)
{
	struct pfq_shared_queue *q = pfq_get_shared_queue(p);
	if (!q)
		return 0;
        return Q_SHARED_QUEUE_LEN(q->rx.data);
}


static inline
int pfq_mpsc_queue_index(struct pfq_sock *p)
{
	struct pfq_shared_queue *q = pfq_get_shared_queue(p);
	if (!q)
		return 0;
        return Q_SHARED_QUEUE_INDEX(q->rx.data) & 1;
}


#endif /* PF_Q_SHARED_QUEUE_H */
