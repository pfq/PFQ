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

#ifndef _PF_Q_MACRO_H_
#define _PF_Q_MACRO_H_

#define Q_MAX_ID                (sizeof(long)<<3)
#define Q_MAX_GROUP             (sizeof(long)<<3)
#define Q_BOUNDED_QUEUE_LEN     (sizeof(long)<<3)

#define Q_MAX_DEVICE           	256
#define Q_MAX_DEVICE_MASK       (Q_MAX_DEVICE-1)

#define Q_MAX_HW_QUEUE          256
#define Q_MAX_HW_QUEUE_MASK     (Q_MAX_HW_QUEUE-1)

#define Q_GRACE_PERIOD 			500      /* msec */

#define Q_TX_RING_SIZE          (8192)
#define Q_TX_RING_MASK          (PFQ_TX_RING_SIZE-1)

#define Q_SLOT_ALIGN(s, n)      ((s+(n-1)) & ~(n-1))

#define Q_FUN_SYMB_LEN          256
#define Q_PERSISTENT_MEM 		64

#define Q_GC_LOG_QUEUE_LEN		16
#define Q_GC_POOL_QUEUE_LEN 	128

#endif /* _PF_Q_MACRO_H_ */
