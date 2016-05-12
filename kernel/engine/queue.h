/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
 *             Andrea Di Pietro <andrea.dipietro@for.unipi.it>
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

#ifndef Q_ENGINE_QUEUE_H
#define Q_ENGINE_QUEUE_H

#include <engine/lang/GC.h>

#include <engine/group.h>
#include <engine/sock.h>
#include <engine/define.h>



int pfq_shared_queue_enable(struct pfq_sock *so, unsigned long addr);
int pfq_shared_queue_disable(struct pfq_sock *so);


static inline size_t pfq_mpsc_queue_mem(struct pfq_sock *so)
{
        return so->opt.rx_queue_len * so->opt.rx_slot_size * 2;
}

static inline size_t pfq_spsc_queue_mem(struct pfq_sock *so)
{
        return so->opt.tx_queue_len * so->opt.tx_slot_size * 2;
}


static inline
size_t pfq_mpsc_queue_len(struct pfq_sock *p)
{
	struct pfq_shared_queue *q = pfq_get_shared_queue(p);
	if (!q)
		return 0;
        return Q_SHARED_QUEUE_LEN(q->rx.data);
}


static inline
int pfq_mpsc_queue_index(struct pfq_sock *p)
{
	struct pfq_shared_queue *q = pfq_get_shared_queue(p);
	if (!q)
		return 0;
        return Q_SHARED_QUEUE_INDEX(q->rx.data) & 1;
}


static inline
char *pfq_mpsc_slot_ptr(struct pfq_sock_opt *opt, struct pfq_rx_queue *qd, size_t qindex, size_t slot)
{
	return (char *)(opt->rxq.base_addr) + (opt->rx_queue_len * (qindex & 1) + slot) * opt->rx_slot_size;
}


#endif /* Q_ENGINE_QUEUE_H */
