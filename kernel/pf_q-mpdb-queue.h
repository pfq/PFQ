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

#ifndef _MPDB_QUEUE_H_
#define _MPDB_QUEUE_H_

#include <linux/skbuff.h>
#include <linux/pf_q.h>
#include <linux/if_vlan.h>

#include <pf_q-queue.h>
#include <pf_q-common.h>


extern bool   mpdb_enqueue(struct pfq_opt *pq, struct sk_buff *skb);
extern size_t mpdb_enqueue_batch(struct pfq_opt *pq, unsigned long queue_mask, int len, struct pfq_queue_skb *skbs, int gid);
extern void * mpdb_queue_alloc(struct pfq_opt *pq, size_t queue_mem, size_t * tot_mem);
extern void   mpdb_queue_free(struct pfq_opt *pq);


static inline
size_t mpdb_queue_len(struct pfq_opt *p)
{
    struct pfq_queue_descr *qd = (struct pfq_queue_descr *)p->q_addr;
    return DBMP_QUEUE_LEN(qd->data);
}


static inline
int mpdb_queue_index(struct pfq_opt *p)
{
    struct pfq_queue_descr *qd = (struct pfq_queue_descr *)p->q_addr;
    return DBMP_QUEUE_INDEX(qd->data) & 1;
}


static inline
size_t mpdb_queue_size(struct pfq_opt *pq)
{
    return pq->q_slot_size * pq->q_slots;
}


static inline
size_t mpdb_queue_tot_mem(struct pfq_opt *pq)
{
    return sizeof(struct pfq_queue_descr) + mpdb_queue_size(pq) * 2;
}

#endif /* _MPDB_QUEUE_H_ */
