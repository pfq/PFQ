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

#include <pfq/global.h>
#include <pfq/define.h>

#include <linux/module.h>


extern struct pfq_global_data default_global;


module_param_named(max_slot_size,	 default_global.max_slot_size,		int, 0644);
module_param_named(max_pool_size,	 default_global.max_pool_size,		int, 0644);

module_param_named(capt_batch_len,	 default_global.capt_batch_len,		int, 0644);
module_param_named(xmit_batch_len,	 default_global.xmit_batch_len,		int, 0644);
module_param_named(skb_tx_pool_size,	 default_global.skb_tx_pool_size,	int, 0644);
module_param_named(skb_rx_pool_size,	 default_global.skb_rx_pool_size,	int, 0644);
module_param_named(vlan_untag,		 default_global.vlan_untag,		int, 0644);
module_param_named(tx_retry,		 default_global.tx_retry,		int, 0644);

module_param_array_named(tx_cpu,	 default_global.tx_cpu,	  int, &default_global.tx_cpu_nr, 0644);

MODULE_PARM_DESC(max_slot_size,		" Maximum socket slot size (default=2048 bytes)");
MODULE_PARM_DESC(max_pool_size,		" Maximum socket buffer pool size (default=2048)");
MODULE_PARM_DESC(capt_batch_len,	" Capture batch queue length");
MODULE_PARM_DESC(xmit_batch_len,	" Transmit batch queue length");
MODULE_PARM_DESC(vlan_untag,		" Enable vlan untagging (default=0)");

#ifdef PFQ_USE_SKB_POOL
MODULE_PARM_DESC(skb_tx_pool_size,	" Socket buffer Tx pool size (default=1024)");
MODULE_PARM_DESC(skb_rx_pool_size,	" Socket buffer Rx pool size (default=1024)");
#endif

MODULE_PARM_DESC(tx_cpu,		" Tx k-threads cpu");
MODULE_PARM_DESC(tx_retry,		" Tx retry attempts (default 1)");

