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

#ifndef Q_ENGINE_GLOBAL_H
#define Q_ENGINE_GLOBAL_H

#include <engine/define.h>
#include <engine/group.h>

#include <pfq/thread.h>

struct pfq_kernel_stats __percpu;
struct pfq_memory_stats __percpu;
struct pfq_percpu_data  __percpu;
struct pfq_percpu_sock  __percpu;
struct pfq_percpu_pool  __percpu;

struct global_data
{
	int capture_incoming;
	int capture_outgoing;

	int capt_slot_size;
	int xmit_slot_size;

	int xmit_batch_len;
	int capt_batch_len;
	int skb_pool_size;

	int vl_untag;

	int tx_affinity[Q_MAX_CPU];
	int tx_thread_nr;
	int tx_rate_control_eager;

	atomic_t        sockets_count;
	atomic_long_t   sockets_vector[Q_MAX_ID];
	struct mutex	sockets_lock;

	atomic_long_t   devmap [Q_MAX_DEVICE][Q_MAX_HW_QUEUE];
	atomic_t        devmap_monitor [Q_MAX_DEVICE];
	struct mutex	devmap_lock;

	struct pfq_group groups[Q_MAX_GID];
	struct mutex	 groups_lock;

	struct rw_semaphore symtable_sem;

	struct pfq_kernel_stats __percpu   * percpu_stats;
	struct pfq_memory_stats __percpu   * percpu_mem_stats;
	struct pfq_percpu_data __percpu    * percpu_data;
	struct pfq_percpu_sock __percpu    * percpu_sock;
	struct pfq_percpu_pool __percpu    * percpu_pool;
};


extern struct global_data * global;
extern struct global_data * pfq_global_init(void);


#endif /* Q_ENGINE_GLOBAL_H */
