/***************************************************************
 *
 * (C) 2011-15 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PF_Q_MACRO_H
#define PF_Q_MACRO_H

#include <pf_q-types.h>

#define Q_MAX_ID                ((int)sizeof(long)<<3)
#define Q_MAX_GID		((int)sizeof(long)<<3)
#define Q_SKBUFF_BATCH		((int)sizeof(long)<<3)

#define Q_GC_LOG_QUEUE_LEN	16
#define Q_GC_POOL_QUEUE_LEN	512

#define Q_MAX_SOCK_MASK		1024
#define Q_MAX_DEVICE		1024
#define Q_MAX_HW_QUEUE          256

#define Q_MAX_TX_SKB_COPY	256

#define Q_GRACE_PERIOD		50 /* msec */

#define Q_SLOT_ALIGN(s, n)      ((s+(n-1)) & ~(n-1))

#define Q_FUN_SYMB_LEN          256

#define Q_MAX_CPU               256
#define Q_MAX_CPU_MASK          (Q_MAX_CPU-1)

#define Q_GROUP_PERSIST_MEM	64
#define Q_GROUP_PERSIST_DATA	1024

#define Q_MAX_POOL_SIZE         16384
#define Q_MAX_SOCKQUEUE_LEN	262144


#define Q_INVALID_ID	(__force pfq_id_t)-1


#endif /* PF_Q_MACRO_H */
