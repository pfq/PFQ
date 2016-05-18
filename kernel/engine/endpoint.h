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

#ifndef Q_ENGINE_ENDPOINT_H
#define Q_ENGINE_ENDPOINT_H

#include <engine/qbuff.h>
#include <engine/define.h>

#include <pfq/types.h>
#include <pfq/kcompat.h>

struct pfq_sock;
struct net_device;
struct qbuff;

enum pfq_endpoint_type
{
	pfq_endpoint_socket,
	pfq_endpoint_device
};


struct pfq_endpoint_info
{
	struct net_device * dev[Q_BUFF_LOG_LEN];
	size_t cnt [Q_BUFF_LOG_LEN];
	size_t cnt_total;
	size_t num;
};


void add_dev_to_endpoints(struct net_device *dev, struct pfq_endpoint_info *ts);


extern size_t copy_to_endpoint_qbuffs(struct pfq_sock *so,
				      struct pfq_qbuff_refs *buffs,
				      unsigned long long mask,
				      int cpu, pfq_gid_t gid);

#endif /* Q_ENGINE_ENDPOINT_H */
