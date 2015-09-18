/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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


#ifndef PF_Q_DEV_H
#define PF_Q_DEV_H

#include <pragma/diagnostic_push>
#include <linux/netdevice.h>
#include <pragma/diagnostic_pop>

#include <pf_q-dev.h>

static
int dev_put_by_index(struct net *net, int ifindex)
{
	struct net_device *dev;
	int err = -EPERM;
	rcu_read_lock();
	dev = dev_get_by_index_rcu(net, ifindex);
	if (dev) {
		dev_put(dev);
		err = 0;
	}
	rcu_read_unlock();
	return err;
}


#endif /* PF_Q_DEV_H */

