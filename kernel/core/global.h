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

#ifndef Q_CORE_GLOBAL_H
#define Q_CORE_GLOBAL_H

#include <core/lang/symtable.h>

#include <core/define.h>
#include <core/group.h>

#include <pfq/thread.h>

#include <linux/pf_q.h>

struct core_kernel_stats __percpu;
struct core_memory_stats __percpu;
struct core_percpu_data  __percpu;
struct pfq_percpu_pool   __percpu;


struct core_global_data
{
	int capt_slot_size;
	int xmit_slot_size;

	int xmit_batch_len;
	int capt_batch_len;
	int skb_pool_size;

	int vlan_untag;

	int tx_cpu[Q_CORE_MAX_CPU];
	int tx_cpu_nr;
	int tx_retry;

	atomic_long_t   socket_ptr[Q_CORE_MAX_ID];
	atomic_t        socket_count;
	struct mutex	socket_lock;

	atomic_long_t   devmap [Q_CORE_MAX_DEVICE][Q_CORE_MAX_QUEUE];
	atomic_t        devmap_toggle [Q_CORE_MAX_DEVICE];
	struct mutex	devmap_lock;

	atomic_t	pool_enabled;

	struct core_group groups[Q_CORE_MAX_GID];
	struct mutex	  groups_lock;

	struct core_kernel_stats	__percpu   * percpu_stats;
	struct core_memory_stats	__percpu   * percpu_memory;
	struct core_percpu_data		__percpu   * percpu_data;
	struct pfq_percpu_pool		__percpu   * percpu_pool;

	struct symtable	 functions;
	struct rw_semaphore symtable_sem;
};


extern struct core_global_data * global;
extern struct core_global_data * core_global_init(void);


#endif /* Q_CORE_GLOBAL_H */
