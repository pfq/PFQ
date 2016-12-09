/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PFQ_PERCPU_H
#define PFQ_PERCPU_H

#include <linux/spinlock.h>

#include <pfq/pool.h>

extern int  pfq_percpu_init(void);
extern int  pfq_percpu_GC_reset(void);
extern int  pfq_percpu_destruct(void);


struct pfq_percpu_pool
{
        struct spinlock		tx_lock;

	struct pfq_skb_pools	tx_multi;
	struct pfq_skb_pools	rx_multi;

} ____pfq_cacheline_aligned;


#endif /* PFQ_PERCPU_H */

