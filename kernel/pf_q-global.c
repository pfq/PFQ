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

#include <linux/kernel.h>
#include <linux/module.h>

#include <linux/types.h>

atomic_t timestamp_toggle;

struct local_data __percpu    * cpu_data;

int direct_capture 	= 0;

int capture_incoming 	= 1;
int capture_outgoing 	= 0;
int capture_loopback 	= 0;

int tx_queue_slots  	= 131072;       /* slots per queue (both rx and tx) */
int rx_queue_slots  	= 131072;       /* slots per queue (both rx and tx) */

int cap_len      	= 1514;
int max_len      	= 1514;

int prefetch_len 	= 1;
int batch_len 		= 1;

int flow_control 	= 0;
int vl_untag     	= 0;

int recycle_len  	= 4096;
