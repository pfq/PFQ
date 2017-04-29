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

#ifndef PFQ_ENDPOINT_H
#define PFQ_ENDPOINT_H

#include <pfq/qbuff.h>
#include <pfq/define.h>
#include <pfq/types.h>
#include <pfq/kcompat.h>


struct pfq_qbuff_queue;
struct pfq_sock;
struct net_device;
struct qbuff;


enum pfq_endpoint_type
{
	Q_ENDPOINT_SOCKET,
	Q_ENDPOINT_DEVICE
};


struct pfq_endpoint_info
{
	struct net_device * dev[Q_BUFF_LOG_LEN];
	size_t cnt [Q_BUFF_LOG_LEN];
	size_t cnt_total;
	size_t num;
};


extern void pfq_add_dev_to_endpoints(struct net_device *dev, struct pfq_endpoint_info *ts);

extern size_t pfq_copy_to_endpoint_qbuffs( struct pfq_sock *so
					 , struct pfq_qbuff_queue *buffs
					 , unsigned __int128 mask
					 , int cpu);

extern void pfq_get_lazy_endpoints(struct pfq_qbuff_queue *qb, struct pfq_endpoint_info *ts);

#endif /* PFQ_ENDPOINT_H */
