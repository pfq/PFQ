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

#ifndef PFQ_SHMEM_H
#define PFQ_SHMEM_H

#include <linux/vmalloc.h>
#include <linux/net.h>

struct pfq_sock;


enum pfq_shmem_kind
{
	pfq_shmem_virt,
	pfq_shmem_user
};


struct pfq_pages_descr
{
	struct page**		hugepages;
	size_t			npages;
	void *			addr;
	size_t			size;
};


struct pfq_shmem_descr
{
	int			id;
	void *			addr;
	size_t			size;
	enum pfq_shmem_kind     kind;
	struct pfq_pages_descr *hugepages_descr;
};


extern size_t pfq_total_queue_mem(struct pfq_sock *so);
extern size_t pfq_total_queue_mem_aligned(struct pfq_sock *so);

extern int    pfq_mmap(struct file *file, struct socket *sock, struct vm_area_struct *vma);
extern int    pfq_vmalloc_user(pfq_id_t, struct pfq_shmem_descr *shmem, size_t size);

extern int    pfq_hugepages_map(pfq_id_t, struct pfq_shmem_descr *shmem, unsigned long user_addr, size_t user_size, size_t hugepage_size, size_t req_size);
extern int    pfq_hugepages_unmap(struct pfq_shmem_descr *shmem);


extern int    pfq_shared_memory_alloc(pfq_id_t, struct pfq_shmem_descr *shmem, unsigned long user_addr, size_t user_size, size_t huge_size, size_t req_size);
extern void   pfq_shared_memory_free(struct pfq_shmem_descr *shmem);


#endif /* PFQ_SHMEM_H */
