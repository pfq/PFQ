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

#include <engine/global.h>
#include <engine/define.h>

#include <linux/module.h>

extern struct global_data default_global;


module_param_named(capture_incoming,	 default_global.capture_incoming, int, 0644);
module_param_named(capture_outgoing,	 default_global.capture_outgoing, int, 0644);

module_param_named(capt_slot_size,	 default_global.capt_slot_size,		int, 0644);
module_param_named(xmit_slot_size,	 default_global.xmit_slot_size,		int, 0644);
module_param_named(capt_batch_len,	 default_global.capt_batch_len,		int, 0644);
module_param_named(xmit_batch_len,	 default_global.xmit_batch_len,		int, 0644);
module_param_named(skb_pool_size,	 default_global.skb_pool_size,		int, 0644);
module_param_named(vl_untag,		 default_global.vl_untag,		int, 0644);
module_param_named(tx_rate_control_eager,default_global.tx_rate_control_eager,	int, 0644);

module_param_array_named(tx_affinity,	 default_global.tx_affinity,		int, &default_global.tx_thread_nr, 0644);

MODULE_PARM_DESC(capture_incoming,	" Capture incoming packets: (1 default)");
MODULE_PARM_DESC(capture_outgoing,	" Capture outgoing packets: (0 default)");
MODULE_PARM_DESC(capt_slot_size,	" Maximum capture length (bytes)");
MODULE_PARM_DESC(xmit_slot_size,	" Maximum transmission length (default=1514 bytes)");
MODULE_PARM_DESC(capt_batch_len,	" Capture batch queue length");
MODULE_PARM_DESC(xmit_batch_len,	" Transmit batch queue length");
MODULE_PARM_DESC(vl_untag,		" Enable vlan untagging (default=0)");

#ifdef PFQ_USE_SKB_POOL
MODULE_PARM_DESC(skb_pool_size,		" Socket buffer pool size (default=1024)");
#endif

MODULE_PARM_DESC(tx_affinity,		" Tx threads cpus' affinity");
MODULE_PARM_DESC(tx_rate_control_eager, " Tx rate control eager (default enabled = 1)");

