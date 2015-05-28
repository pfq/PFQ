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

#include <warning/push>

#include <linux/kernel.h>
#include <linux/version.h>
#include <linux/module.h>
#include <linux/vmalloc.h>
#include <linux/pagemap.h>

#include <warning/pop>

#include <pf_q-shmem.h>
#include <pf_q-shared-queue.h>


static int
pfq_memory_map(struct vm_area_struct *vma, unsigned long size, char *ptr, unsigned int flags, enum pfq_shmem_kind kind)
{
	// unsigned long addr = vma->vm_start;

        vma->vm_flags |= flags;

	switch(kind)
	{
	case pfq_shmem_virt: {
		if (remap_vmalloc_range(vma, ptr, 0) != 0) {
			printk(KERN_WARNING "[PFQ] remap_vmalloc_range error.\n");
			return -EAGAIN;
		}
	} break;

	//case pfq_shmem_phys: {
	//	if (remap_pfn_range(vma, addr, virt_to_phys(ptr) >> PAGE_SHIFT, vma->vm_end - vma->vm_start, PAGE_SHARED) != 0) {
	//		printk(KERN_WARNING "[PFQ] remap_vmalloc_range error.\n");
	//		return -EAGAIN;
	//	}
	//} break;

	case pfq_shmem_user:
		break;
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

        if(size > so->shmem.size) {
                printk(KERN_WARNING "[PFQ] pfq_mmap: area too large!\n");
                return -EINVAL;
        }

        if((ret = pfq_memory_map(vma, size, so->shmem.addr, VM_LOCKED, so->shmem.kind)) < 0)
                return ret;

        return 0;
}


int
pfq_hugepage_map(struct pfq_shmem_descr *shmem, unsigned long addr, size_t size)
{
	int nid;

	printk(KERN_INFO "[PFQ] mapping user memory (HugePages)...\n");

	shmem->npages = PAGE_ALIGN(size) / PAGE_SIZE;
	shmem->hugepages = vmalloc(shmem->npages * sizeof(struct page *));

	if (get_user_pages_fast(addr, shmem->npages, 1, shmem->hugepages) != shmem->npages) {

		vfree(shmem->hugepages);

		shmem->npages = 0;
		shmem->hugepages = NULL;
		printk(KERN_WARNING "[PFQ] could not get user pages!\n");
		return -EPERM;
	}

	nid = page_to_nid(shmem->hugepages[0]);

	shmem->addr = vm_map_ram(shmem->hugepages, shmem->npages, nid, PAGE_KERNEL);
	if (!shmem->addr) {
		printk(KERN_INFO "[PFQ] mapping memory failure.\n");
		return -EPERM;
	}

	shmem->kind = pfq_shmem_user;
        shmem->size = size;

	pr_devel("[PFQ] total mapped memory: %zu bytes.\n", size);
	return 0;
}


int
pfq_hugepage_unmap(struct pfq_shmem_descr *shmem)
{
	int i;

	if (current->mm)
		down_read(&current->mm->mmap_sem);

	for(i = 0; i < shmem->npages; i++)
	{

		if (!PageReserved(shmem->hugepages[i]))
		    SetPageDirty(shmem->hugepages[i]);

		page_cache_release(shmem->hugepages[i]);

	}

	if (current->mm)
		up_read(&current->mm->mmap_sem);

	vfree(shmem->hugepages);

	shmem->hugepages = NULL;
	shmem->npages = 0;

	return 0;
}


int
pfq_shared_memory_alloc(struct pfq_shmem_descr *shmem, size_t mem_size)
{
	size_t tot_mem = PAGE_ALIGN(mem_size);

	pr_devel("[PFQ] allocating shared memory...\n");

        shmem->addr = vmalloc_user(tot_mem);
        shmem->size = tot_mem;
	shmem->kind = pfq_shmem_virt;

	if (shmem->addr == NULL) {
		printk(KERN_WARNING "[PFQ] shared memory alloc: out of memory (vmalloc %zu bytes)!", tot_mem);
		return -ENOMEM;
	}

	pr_devel("[PFQ] total shared memory: %zu bytes.\n", tot_mem);
	return 0;
}


void
pfq_shared_memory_free(struct pfq_shmem_descr *shmem)
{
	if (shmem->addr) {

		switch(shmem->kind)
		{
		case pfq_shmem_virt: vfree(shmem->addr); break;
		case pfq_shmem_user: pfq_hugepage_unmap(shmem); break;
		}

		shmem->addr = NULL;
		shmem->size = 0;

		pr_devel("[PFQ] shared memory freed.\n");
	}
}


size_t pfq_total_queue_mem(struct pfq_sock *so)
{
        return sizeof(struct pfq_shared_queue) + pfq_queue_mpsc_mem(so) + pfq_queue_spsc_mem(so) * Q_MAX_TX_QUEUES;
}



#define HUGEPAGE_SIZE  (2*1024*1024)

size_t pfq_shared_memory_size(struct pfq_sock *so)
{
	size_t tot_mem = pfq_total_queue_mem(so);

	return (1 + tot_mem/HUGEPAGE_SIZE) * HUGEPAGE_SIZE;
}


