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

#ifndef _PF_Q_SHMEM_H_
#define _PF_Q_SHMEM_H_

#include <pf_q-mpdb-queue.h>


static inline size_t pfq_total_shared_mem(struct pfq_sock *so)
{
        return sizeof(struct pfq_queue_hdr) + pfq_queue_mpdb_mem(so) + pfq_queue_spsc_mem(so);
}


int pfq_mmap(struct file *file, struct socket *sock, struct vm_area_struct *vma);

int pfq_shared_memory_alloc(struct pfq_sock *so, size_t shmem);
void pfq_shared_memory_free(struct pfq_sock *so);


#endif /* _PF_Q_SHMEM_H_ */
