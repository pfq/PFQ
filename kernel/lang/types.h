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

#ifndef PFQ_LANG_TYPES_H
#define PFQ_LANG_TYPES_H

#include <linux/inetdevice.h>

/* skb owned by garbage collector */

#define __GC __attribute__((address_space(7)))


/* CIDR notation */

struct CIDR
{
	__be32	addr;
	int	prefix;
};

struct CIDR_
{
	__be32	addr;
	__be32  mask;
};


/* note: use can use inet_mask_len(data->mask) to get the prefix */

static inline
void to_CIDR_(struct CIDR *data)
{
	struct CIDR_ * data_ = (struct CIDR_ *)data;
	__be32 mask = inet_make_mask(data->prefix);
	data_->addr &= mask;
	data_->mask  = mask;
}

#define CIDR_INIT(a,i)		to_CIDR_((struct CIDR *)&ARGS_TYPE(a)->arg[i].value)

#endif /* PFQ_LANG_TYPES_H */
