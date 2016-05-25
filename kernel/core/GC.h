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

#ifndef Q_CORE_GC_H
#define Q_CORE_GC_H

#include <core/endpoint.h>
#include <core/define.h>
#include <core/qbuff.h>

#include <pfq/kcompat.h>


struct net_device;

struct GC_log
{
	struct net_device * dev[Q_CORE_BUFF_LOG_LEN];
	size_t num_devs;
	size_t xmit_todo;
	bool   to_kernel;
};


struct GC_data
{
	struct core_qbuff_long_queue pool;
	struct GC_log log[Q_CORE_BUFF_QUEUE_LEN];
};


extern struct qbuff * GC_make_buff (struct GC_data *gc, void *addr);
extern void  GC_get_lazy_endpoints(struct GC_data *gc, struct core_endpoint_info *info);
extern void  GC_reset(struct GC_data *gc);


static inline size_t
GC_count_dev_in_log(struct net_device *dev, struct GC_log *log)
{
	size_t n, ret = 0;
	for(n = 0; n < log->num_devs; n++)
	{
		if (dev == log->dev[n])
			ret++;
	}
	return ret;
}


static inline
void GC_data_init(struct GC_data *gc)
{
	memset(gc, 0, sizeof(struct GC_data));
}


static inline
void GC_log_init(struct GC_log *log)
{
	log->to_kernel = false;
	log->xmit_todo = 0;
	log->num_devs  = 0;
}


static inline
size_t GC_size(struct GC_data *gc)
{
	return gc->pool.len;
}


#endif /* Q_CORE_GC_H */
