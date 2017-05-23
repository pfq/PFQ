/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
 *             Andrea Di Pietro <andrea.dipietro@for.unipi.it>
 * 	       Loris Gazzarrini <loris.gazzarrini@iet.unipi.it>
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

#include <pfq/global.h>

struct pfq_global_data default_global =
{
	.max_slot_size		= 2048,

	.xmit_batch_len		= 1,
	.capt_batch_len		= 1,

	.vlan_untag		= 0,

	.skb_tx_pool_size	= 1024,
	.skb_rx_pool_size	= 1024,

	.tx_cpu			= {0},
	.tx_cpu_nr		= 0,
	.tx_retry		= 1,

	.socket_ptr		= {{0}},
	.socket_count		= {0},
     // .socket_lock		= {{0}},

	.devmap			= {{{0}}},
	.devmap_toggle		= {{0}},
     // .devmap_lock		= {{0}},

	.pool_enabled		= {0},
	.groups			= {{}},
     // .groups_lock		= {{0}},

	.percpu_stats		= NULL,
	.percpu_memory		= NULL,
	.percpu_data		= NULL,
	.percpu_pool		= NULL,

	.functions		= {}
     // .symtable_sem		= {0},
};


struct pfq_global_data * global;

struct pfq_global_data *
pfq_global_alloc(void)
{
	return &default_global;
}

struct pfq_global_data *
pfq_global_init(void)
{
	static struct pfq_global_data *data;
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


extern struct pfq_global_data default_global;

