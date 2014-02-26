/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#ifndef _PF_Q_GLOBAL_H_
#define _PF_Q_GLOBAL_H_

#include <linux/types.h>

extern atomic_t timestamp_toggle;

extern struct local_data __percpu * cpu_data;

extern int direct_capture;

extern int capture_incoming;
extern int capture_outgoing;
extern int capture_loopback;

extern int tx_queue_slots;
extern int rx_queue_slots;

extern int cap_len;
extern int max_len;

extern int prefetch_len;
extern int batch_len;

extern int flow_control;
extern int vl_untag;

extern int recycle_len;

#endif /* _PF_Q_GLOBAL_H_ */
