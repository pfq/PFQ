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

#include <lang/monad.h>
#include <pfq/qbuff.h>

bool
qbuff_ingress(struct qbuff const *buff, struct iphdr const *ip)
{
	struct in_device *in_dev;
	bool ret = false;
        bool ctx = buff->monad->ep_ctx;

	rcu_read_lock();
	in_dev = __in_dev_get_rcu(QBUFF_SKB(buff)->dev);
	if (in_dev != NULL) {
		for_primary_ifa(in_dev) {
			if (((ifa->ifa_address == ip->daddr) && (ctx & EPOINT_DST)) ||
			    ((ifa->ifa_address == ip->saddr) && (ctx & EPOINT_SRC))){
				ret = true;
				break;
			}
		} endfor_ifa(in_dev);
	}
	rcu_read_unlock();

	return ret;
}


