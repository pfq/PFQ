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

#ifndef _PF_COMMON_H_
#define _PF_COMMON_H_

#include <linux/kernel.h>
#include <linux/pf_q.h>

#include <pf_q-sparse-counter.h>

#define Q_MAX_CPU               (sizeof(long)<<3)
#define Q_MAX_ID                (sizeof(long)<<3)
#define Q_MAX_GROUP             (sizeof(long)<<3)
#define Q_PREFETCH_MAX_LEN      (sizeof(long)<<3)

#define Q_MAX_DEVICE            256
#define Q_MAX_DEVICE_MASK       (Q_MAX_DEVICE-1)
#define Q_MAX_HW_QUEUE          256
#define Q_MAX_HW_QUEUE_MASK     (Q_MAX_HW_QUEUE-1)

#define Q_GRACE_PERIOD 100      /* msec */

#define Q_TX_RING_SIZE          (8192)
#define Q_TX_RING_MASK          (PFQ_TX_RING_SIZE-1)
#define Q_SLOT_ALIGN(s, n)      ((s+(n-1)) & ~(n-1))


/* sparse_counter_t stats */

typedef struct pfq_kstats
{
    sparse_counter_t  recv;    /* received by the queue */
    sparse_counter_t  lost;    /* packets lost due to queue congestion */
    sparse_counter_t  drop;    /* dropped by filters */

} pfq_kstat_t;


#endif /* _PF_COMMON_H_ */
