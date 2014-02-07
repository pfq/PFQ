/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#include <linux/pf_q.h>

#include <pf_q-sock.h>

int   pfq_queue_alloc(struct pfq_sock *p, size_t queue_mem);
void  pfq_queue_free (struct pfq_sock *p);

inline size_t pfq_queue_mpdb_mem(struct pfq_sock *so)
{
    return so->rx_opt.size * so->rx_opt.slot_size * 2;
}

inline size_t pfq_queue_spsc_mem(struct pfq_sock *so)
{
    return so->tx_opt.size * so->tx_opt.slot_size;
}

inline size_t pfq_queue_total_mem(struct pfq_sock *so)
{
    return sizeof(struct pfq_queue_hdr) + pfq_queue_mpdb_mem(so) + pfq_queue_spsc_mem(so);
}

