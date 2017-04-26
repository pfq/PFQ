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

#ifndef PFQ_GLOBAL_H
#define PFQ_GLOBAL_H

#include <lang/symtable.h>

#include <pfq/define.h>
#include <pfq/group.h>
#include <pfq/thread.h>

#include <linux/pf_q.h>

struct pfq_kernel_stats __percpu;
struct pfq_memory_stats __percpu;
struct pfq_percpu_data  __percpu;
struct pfq_percpu_pool   __percpu;


struct pfq_global_data
{
	int capt_slot_size;
	int xmit_slot_size;

	int xmit_batch_len;
	int capt_batch_len;
	int skb_pool_size;

	int vlan_untag;

	int tx_cpu[Q_MAX_CPU];
	int tx_cpu_nr;
	int tx_retry;

	atomic_long_t   socket_ptr[Q_MAX_ID];
	atomic_t        socket_count;
	struct mutex	socket_lock;

	atomic_long_t   devmap [Q_MAX_DEVICE][Q_MAX_QUEUE];
	atomic_t        devmap_toggle [Q_MAX_DEVICE];
	struct mutex	devmap_lock;

	atomic_t	pool_enabled;

	struct pfq_group groups[Q_MAX_GID];
	struct mutex	  groups_lock;

	struct pfq_kernel_stats	__percpu   * percpu_stats;
	struct pfq_memory_stats	__percpu   * percpu_memory;
	struct pfq_percpu_data		__percpu   * percpu_data;
	struct pfq_percpu_pool		__percpu   * percpu_pool;

	struct symtable	 functions;
	struct rw_semaphore symtable_sem;
};


extern struct pfq_global_data * global;
extern struct pfq_global_data * pfq_global_init(void);


#endif /* PFQ_GLOBAL_H */
