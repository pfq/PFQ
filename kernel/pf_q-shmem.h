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

#ifndef PF_Q_SHMEM_H
#define PF_Q_SHMEM_H

#include <linux/vmalloc.h>
#include <linux/net.h>

struct pfq_sock;

enum pfq_shmem_kind {
	pfq_shmem_virt,
	pfq_shmem_user
};


struct pfq_shmem_descr
{
	void *			addr;
	size_t  		size;
	enum pfq_shmem_kind     kind;

	struct page** 		hugepages;
	size_t 			npages;
};


size_t pfq_total_queue_mem(struct pfq_sock *so);


int pfq_mmap(struct file *file, struct socket *sock, struct vm_area_struct *vma);

int pfq_shared_memory_alloc(struct pfq_shmem_descr *shmem, size_t size);
void pfq_shared_memory_free(struct pfq_shmem_descr *shmem);
size_t pfq_shared_memory_size(struct pfq_sock *so);

int pfq_hugepage_map(struct pfq_shmem_descr *shmem, unsigned long addr, size_t size);
int pfq_hugepage_unmap(struct pfq_shmem_descr *shmem);


#endif /* PF_Q_SHMEM_H */
