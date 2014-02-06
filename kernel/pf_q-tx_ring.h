/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#ifndef _PFQ_TX_RING_H_
#define _PFQ_TX_RING_H_

#include <pf_q-common.h>

extern int pfq_tx_ring_alloc(struct pfq_tx_opt *tq);
extern void pfq_tx_ring_free(struct pfq_tx_opt *tq);

inline
struct sk_buff *
pfq_tx_ring_skb_get(struct pfq_tx_opt *tq, size_t n)
{
        return tq->skb_slot[n & PFQ_TX_RING_MASK];
}

#endif /* _PFQ_TX_RING_H_ */
