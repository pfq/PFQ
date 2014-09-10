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

#ifndef _PF_Q_STATS_H_
#define _PF_Q_STATS_H_

#include <linux/kernel.h>
#include <linux/pf_q.h>

#include <pf_q-sparse.h>


/* sparse_counter_t stats */


struct pfq_socket_rx_stats
{
        sparse_counter_t  recv;         /* received by the queue */
        sparse_counter_t  lost;         /* packets lost due to queue congestion */
        sparse_counter_t  drop;         /* dropped by filters */
};


struct pfq_socket_tx_stats
{
        sparse_counter_t  sent;         /* sent by the driver */
        sparse_counter_t  disc;         /* discarded by the driver */
};



struct pfq_group_stats
{
        sparse_counter_t recv;
        sparse_counter_t drop;
        sparse_counter_t frwd;
        sparse_counter_t kern;
};


struct pfq_global_stats
{
	sparse_counter_t recv; 	    	/* received by PFQ */
	sparse_counter_t lost; 	    	/* lost during capture, due to PFQ problem (e.g. memory problem) */
        sparse_counter_t sent;  	/* transmitted from user-space */
        sparse_counter_t frwd;  	/* forwarded to devices */
        sparse_counter_t kern;  	/* passed to kernel */
        sparse_counter_t fail; 		/* discarded due to PFQ problem (e.g. memory problems) */
        sparse_counter_t disc;  	/* discarded due to driver congestion */
};


struct pfq_memory_stats
{
 	sparse_counter_t os_alloc;
	sparse_counter_t os_free;
	sparse_counter_t rc_alloc;
	sparse_counter_t rc_free;
	sparse_counter_t rc_error;
};


#endif /* _PF_Q_STATS_H_ */
