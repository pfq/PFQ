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
#include <pf_q-mpdb-queue.h>


static int
pfq_memory_map(struct vm_area_struct *vma, unsigned long size, char *ptr, unsigned int flags, enum pfq_shmem_kind kind)
{
	unsigned long addr = vma->vm_start;

        vma->vm_flags |= flags;

	switch(kind)
	{
	case pfq_shmem_virt: {
		if (remap_vmalloc_range(vma, ptr, 0) != 0) {
			printk(KERN_WARNING "[PFQ] remap_vmalloc_range error.\n");
			return -EAGAIN;
		}
	} break;

	case pfq_shmem_phys: {
		if (remap_pfn_range(vma, addr, virt_to_phys(ptr) >> PAGE_SHIFT, vma->vm_end - vma->vm_start, PAGE_SHARED) != 0) {
			printk(KERN_WARNING "[PFQ] remap_vmalloc_range error.\n");
			return -EAGAIN;
		}
	} break;

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

        if((ret = pfq_memory_map(vma, size, so->shmem_addr, VM_LOCKED, so->shmem_kind)) < 0)
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

	size_t num_pages = tm / PAGE_SIZE;
	void *addr;

	num_pages += (num_pages + (PAGE_SIZE-1)) & (PAGE_SIZE-1);
	tot_mem = num_pages*PAGE_SIZE;

	so->shmem_kind = pfq_shmem_phys;

	addr = kzalloc(tot_mem, GFP_KERNEL);
	if (!addr) {
		printk(KERN_WARNING "[PFQ|%d] trying with vmalloc...\n", so->id);
        	addr = vmalloc_user(tot_mem);
		so->shmem_kind = pfq_shmem_virt;
	}

	if (addr == NULL) {
		printk(KERN_WARNING "[PFQ|%d] shared memory alloc: out of memory (vmalloc %zu bytes)!", so->id, tot_mem);
		return -ENOMEM;
	}

        so->shmem_addr = addr;
        so->shmem_size = tot_mem;

	pr_devel("[PFQ|%d] total shared memory: %zu bytes (%s contiguous).\n", so->id, tot_mem, (so->shmem_kind == pfq_shmem_virt ? "virtually" : "physically"));
	return 0;
}


void
pfq_shared_memory_free(struct pfq_sock *so)
{
	if (so->shmem_addr) {

		switch(so->shmem_kind)
		{
		case pfq_shmem_virt: vfree(so->shmem_addr); break;
		case pfq_shmem_phys: kfree(so->shmem_addr); break;

		}

		so->shmem_addr = NULL;
		so->shmem_size = 0;

		pr_devel("[PFQ|%d] shared memory freed.\n", so->id);
	}
}


size_t pfq_total_shared_mem(struct pfq_sock *so)
{
        return sizeof(struct pfq_queue_hdr) + pfq_queue_mpdb_mem(so) * 2 + pfq_queue_spsc_mem(so) * Q_MAX_TX_QUEUES;
}

