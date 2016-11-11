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

#ifndef PFQ_INT_H
#define PFQ_INT_H

#include <stddef.h>

/*! PFQ descriptor. */

typedef struct pfq_data_int pfq_t;

/*! PFQ iterator type. */

typedef char * pfq_iterator_t;


/*! pfq_net_queue_t is a struct which represents a net queue. */

struct pfq_net_queue
{
	pfq_iterator_t queue;		/* net queue */
	size_t         len;		/* number of packets in the queue */
	size_t         slot_size;
	unsigned int   index;		/* current queue index */

};


/*! PFQ data */

struct pfq_data_int
{
	void * shm_hugepages;
	void * shm_addr;

	size_t shm_size;

	void * tx_queue_addr;
	size_t tx_queue_size;

	void * rx_queue_addr;
	size_t rx_queue_size;

	size_t rx_slots;
	size_t rx_slot_size;

        size_t tx_slots;
	size_t tx_slot_size;

	size_t tx_attempt;
	size_t tx_num_async;
        size_t tx_forward;

	const char * error;

	int fd;
	int hd;

	int id;
	int gid;

	struct pfq_net_queue nq;
};

#endif /* PFQ_INT_H */
