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

#include <linux/kernel.h>
#include <linux/version.h>
#include <linux/module.h>
#include <linux/vmalloc.h>

#include <pf_q-shmem.h>

static inline
int
pfq_memory_mmap(struct vm_area_struct *vma,
                unsigned long size, char *ptr, unsigned int flags)
{
        vma->vm_flags |= flags;

        if (remap_vmalloc_range(vma, ptr, 0) != 0) {

                printk(KERN_WARNING "[PFQ] remap_vmalloc_range!\n");
                return -EAGAIN;
        }

        return 0;
}


int
pfq_mmap(struct file *file, struct socket *sock, struct vm_area_struct *vma)
{
        struct pfq_sock *so = pfq_sk(sock->sk);

        unsigned long size = (unsigned long)(vma->vm_end - vma->vm_start);
        int ret;

        if(size & (PAGE_SIZE-1)) {
                printk(KERN_WARNING "[PFQ] pfq_mmap: size not multiple of PAGE_SIZE!\n");
                return -EINVAL;
        }

        if(size > so->shmem_size) {
                printk(KERN_WARNING "[PFQ] pfq_mmap: area too large!\n");
                return -EINVAL;
        }

        if((ret = pfq_memory_mmap(vma, size, so->shmem_addr, VM_LOCKED)) < 0)
                return ret;

        return 0;
}


int
pfq_shared_memory_alloc(struct pfq_sock *so, size_t shmem)
{
        /* calculate the size of the buffer */

	size_t tm = PAGE_ALIGN(shmem);
        size_t tot_mem;

	/* align bufflen to page size */

	size_t num_pages = tm / PAGE_SIZE; void *addr;

	num_pages += (num_pages + (PAGE_SIZE-1)) & (PAGE_SIZE-1);
	tot_mem = num_pages*PAGE_SIZE;

	/* Memory is already zeroed */

        addr = vmalloc_user(tot_mem);
	if (addr == NULL) {
		printk(KERN_WARNING "[PFQ|%d] shared memory alloc: out of memory (vmalloc %zu bytes)!", so->id, tot_mem);
		return -ENOMEM;
	}

        so->shmem_addr = addr;
        so->shmem_size = tot_mem;

	pr_devel("[PFQ|%d] total shared memory: %zu bytes.\n", so->id, tot_mem);
	return 0;
}


void
pfq_shared_memory_free(struct pfq_sock *so)
{
	if (so->shmem_addr) {

		vfree(so->shmem_addr);
		so->shmem_addr = NULL;
		so->shmem_size = 0;

		pr_devel("[PFQ|%d] shared memory freed.\n", so->id);
	}
}

