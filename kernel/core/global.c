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

#include <core/global.h>
#include <pfq/global.h>

#include <pfq/thread.h>

struct core_global_data default_global =
{
	.capture_incoming	= 1,
	.capture_outgoing	= 0,
	.capt_slot_size		= 1514,
	.xmit_slot_size		= 1514,
	.xmit_batch_len		= 1,
	.capt_batch_len		= 1,

	.vlan_untag		= 0,

	.skb_pool_size		= 1024,
	.tx_cpu			= {0},
	.tx_cpu_nr		= 0,

	.socket_ptr		= {{0}},
	.socket_count		= {0},
     // .socket_lock		= {{0}},

	.devmap			= {{{0}}},
	.devmap_monitor		= {{0}},
     // .devmap_lock		= {{0}},

	.groups			= {{}},
     // .groups_lock		= {{0}},


	.percpu_stats		= NULL,
	.percpu_mem_stats	= NULL,
	.percpu_data		= NULL,
	.percpu_sock		= NULL,
	.percpu_pool		= NULL,
	.percpu_queue		= NULL,

	.functions		= {}
     // .symtable_sem		= {0},
};


struct core_global_data * global;


struct core_global_data *
core_global_init(void)
{
	static struct core_global_data *data;
	if (!data)
	{
		if ((data = pfq_global_alloc()))
		{
			memcpy(data, &default_global, sizeof(default_global));
			mutex_init(&data->socket_lock);
			mutex_init(&data->devmap_lock);
			mutex_init(&data->groups_lock);
			init_rwsem(&data->symtable_sem);
		}
	}

	return data;
}


