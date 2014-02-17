/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#ifndef _PF_Q_MPDB_QUEUE_H_
#define _PF_Q_MPDB_QUEUE_H_

#include <linux/skbuff.h>
#include <linux/pf_q.h>
#include <linux/if_vlan.h>

#include <pf_q-prefetch-queue.h>
#include <pf_q-common.h>
#include <pf_q-sock.h>


extern bool   mpdb_enqueue(struct pfq_rx_opt *ro, struct sk_buff *skb);
extern size_t mpdb_enqueue_batch(struct pfq_rx_opt *ro, unsigned long queue_mask, int len, struct pfq_prefetch_skb *skbs, int gid);


static inline
size_t mpdb_queue_len(struct pfq_sock *p)
{
    return MPDB_QUEUE_LEN(get_pfq_queue_hdr(p)->rx.data);
}


static inline
int mpdb_queue_index(struct pfq_sock *p)
{
    return MPDB_QUEUE_INDEX(get_pfq_queue_hdr(p)->rx.data) & 1;
}


static inline
size_t mpdb_queue_size(struct pfq_sock *p)
{
    struct pfq_rx_queue_hdr *rx = & get_pfq_queue_hdr(p)->rx;
    return rx->size * rx->slot_size;
}

#endif /* _PF_Q_MPDB_QUEUE_H_ */
