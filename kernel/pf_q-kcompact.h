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

#ifndef PF_Q_KCOMPACT_H
#define PF_Q_KCOMPACT_H

#include <linux/netdevice.h>

#if (LINUX_VERSION_CODE <= KERNEL_VERSION(3,14,0))
static inline bool netif_xmit_frozen_or_drv_stopped(const struct netdev_queue *queue)
{
#if (LINUX_VERSION_CODE < KERNEL_VERSION(3,3,0))
	return netif_tx_queue_frozen_or_stopped(queue);
#else
	return netif_xmit_frozen_or_stopped(queue);
#endif
}
#endif

#if (LINUX_VERSION_CODE <= KERNEL_VERSION(3,10,0))
static inline struct net_device *
netdev_notifier_info_to_dev(void *data)
{
	return data;
}
#endif

#endif
