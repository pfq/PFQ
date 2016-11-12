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

#include <pragma/diagnostic_push>
#include <linux/kernel.h>
#include <linux/version.h>
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/vmalloc.h>
#include <linux/pagemap.h>
#include <pragma/diagnostic_pop>

#include <core/queue.h>

#include <pfq/shmem.h>


static DEFINE_MUTEX(pfq_hugepages_mutex);

struct pfq_hugepages_descr pfq_hugepages[Q_CORE_MAX_ID];
size_t pfq_hugepages_numb;


static struct pfq_hugepages_descr *
__free_HugePages(void)
{
	int n = 0;
	for(; n < pfq_hugepages_numb; n++)
	{
		struct pfq_hugepages_descr *descr = &pfq_hugepages[n];
		if (descr->pid == 0) {
			descr->id = n;
			return descr;
		}
	}

	if (pfq_hugepages_numb < Q_CORE_MAX_ID) {
		struct pfq_hugepages_descr *descr = &pfq_hugepages[pfq_hugepages_numb];
		descr->id = pfq_hugepages_numb++;
		return descr;
	}

	return NULL;
}


struct pfq_hugepages_descr *
get_HugePages(unsigned long addr, size_t huge_size, size_t size)
{
	struct pfq_hugepages_descr *ret = NULL, *descr;
	int n;

	mutex_lock(&pfq_hugepages_mutex);

	for(n = 0; n < pfq_hugepages_numb; n++)
	{
		descr = &pfq_hugepages[n];

		if (descr->pid == current->tgid) {

			if ((descr->offset + size) > descr->huge_size) {
				printk(KERN_WARNING "[PFQ] error: could not allocate %zu in HugePages!\n", size);
				goto done;
			}

			descr->refcnt++;
			descr->offset += size;
			ret = descr;
			goto done;
		}
	}

        descr = __free_HugePages();
	if (descr) {

		struct page ** hugepages;
		int nid, pinned, npages;
                void *base_addr;

		if (size > huge_size) {
			printk(KERN_WARNING "[PFQ] error: could not allocate %zu bytes in HugePages (%zu bytes)!\n", size, huge_size);
			goto done;
		}

		npages = PAGE_ALIGN(huge_size) / PAGE_SIZE;
		hugepages = vmalloc(npages * sizeof(struct page *));
        	if (hugepages == NULL) {
			printk(KERN_WARNING "[PFQ] error: could not allocate the HugePages vector (%d pages)!\n", npages);
			goto done;
		}

		printk(KERN_INFO "[PFQ] HugePages[%d]: mapping %zu bytes in %d pages (pid=%d)...\n", descr->id, huge_size, npages, current->tgid);

		pinned = get_user_pages_fast(addr, npages, 1, hugepages);
		if (pinned != npages) {
			vfree(hugepages);
			printk(KERN_WARNING "[PFQ] error: could not get user HugePages (pinned pages = %d)!\n", pinned);
			goto done;
		}

		nid = page_to_nid(hugepages[0]);

		base_addr = vm_map_ram(hugepages, npages, nid, PAGE_KERNEL);
		if (!base_addr) {
			// FIXME (get_user_pages_fast...)
			printk(KERN_WARNING "[PFQ] vm_map_ram: mapping memory failure!\n");
			goto done;
		}

                /* commit the slot */

		ret = descr;

		ret->refcnt    = 1;
		ret->pid       = current->tgid;
		ret->hugepages = hugepages;
		ret->npages    = npages;
		ret->base_addr = base_addr;
		ret->offset    = size;
		ret->huge_size = huge_size;
	}
	else {
		printk(KERN_WARNING "[PFQ] get_HugePages: no slots available!\n");
	}

done:
	mutex_unlock(&pfq_hugepages_mutex);
	return ret;
}


int put_HugePages(void)
{
	int i, n;
	mutex_lock(&pfq_hugepages_mutex);

	for(n = 0; n < pfq_hugepages_numb; n++)
	{
		struct pfq_hugepages_descr *descr = &pfq_hugepages[n];

		if (descr->pid == current->tgid) {
			if (--descr->refcnt == 0) {

				if (current->mm)
					up_read(&current->mm->mmap_sem);

				for(i = 0; i < descr->npages; i++)
				{
					if (!PageReserved(descr->hugepages[i]))
					    SetPageDirty(descr->hugepages[i]);

					/* page_cache_release(shmem->hugepages[i]); */
					put_page(descr->hugepages[i]);
				}

				if (current->mm)
					down_read(&current->mm->mmap_sem);

				vfree(descr->hugepages);

				printk(KERN_INFO "[PFQ] HugePages[%d]: releasing memory of %zu pages (pid=%d)...\n", descr->id, descr->npages, descr->pid);

				descr->pid = 0;
				descr->hugepages = NULL;
				descr->npages = 0;
				descr->base_addr = NULL;
				descr->offset = 0;
			}

			mutex_unlock(&pfq_hugepages_mutex);
			return 0;
		}
	}

	mutex_unlock(&pfq_hugepages_mutex);
	printk(KERN_WARNING "[PFQ] put_HugePages: pid (%d) not found!\n", current->tgid);
	return -EPERM;
}


static int
pfq_memory_map(struct vm_area_struct *vma, unsigned long size, char *ptr, unsigned int flags, enum pfq_shmem_kind kind)
{
        vma->vm_flags |= flags;

	switch(kind)
	{
	case pfq_shmem_virt: {
		if (remap_vmalloc_range(vma, ptr, 0) != 0) {
			printk(KERN_WARNING "[PFQ] error: remap_vmalloc_range failed!\n");
			return -EAGAIN;
		}
	} break;

	case pfq_shmem_user:
		break;
	}
        return 0;
}


int
pfq_mmap(struct file *file, struct socket *sock, struct vm_area_struct *vma)
{
        struct core_sock *so = pfq_sk(sock->sk);

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

	printk(KERN_INFO "[PFQ] memory user memory: %lu bytes...\n", size);

        if((ret = pfq_memory_map(vma, size, so->shmem.addr, VM_LOCKED, so->shmem.kind)) < 0)
                return ret;

        return 0;
}


int
pfq_hugepages_map(struct pfq_shmem_descr *shmem, unsigned long addr, size_t huge_size, size_t size)
{

        struct pfq_hugepages_descr * hpages = get_HugePages(addr, huge_size, size);
	if (!hpages) {
		printk(KERN_INFO "[PFQ] mapping memory failure.\n");
		return -EPERM;
	}

	shmem->addr = (char *)hpages->base_addr + hpages->offset - size;
        shmem->size = size;
	shmem->kind = pfq_shmem_user;
        shmem->hugepages = hpages;

	pr_devel("[PFQ] total mapped memory: %zu bytes.\n", size);
	return 0;
}


int
pfq_hugepages_unmap(struct pfq_shmem_descr *shmem)
{
	int rc;
	rc = put_HugePages();
        if (rc < 0)
        	return rc;

	shmem->addr = NULL;
	shmem->hugepages = NULL;
	shmem->size = 0;
	return 0;
}


int
pfq_vmalloc_user(struct pfq_shmem_descr *shmem, size_t mem_size)
{
	size_t tot_mem = PAGE_ALIGN(mem_size);

	pr_devel("[PFQ] allocating shared memory...\n");

        shmem->addr = vmalloc_user(tot_mem);
        shmem->size = tot_mem;
	shmem->kind = pfq_shmem_virt;
        shmem->hugepages = NULL;

	if (shmem->addr == NULL) {
		printk(KERN_WARNING "[PFQ] shmem: out of memory (vmalloc %zu bytes)!", tot_mem);
		return -ENOMEM;
	}

	pr_devel("[PFQ] total shared memory: %zu bytes.\n", tot_mem);
	return 0;
}


int
pfq_shared_memory_alloc(struct pfq_shmem_descr *shmem, size_t huge_size, size_t mem_size, unsigned long user_addr)
{
	if (user_addr) {

		if (pfq_hugepages_map(shmem, user_addr, huge_size, mem_size) < 0)
			return -ENOMEM;
	}
	else {
		if (pfq_vmalloc_user(shmem, mem_size) < 0)
			return -ENOMEM;
	}

	return 0;
}


void
pfq_shared_memory_free(struct pfq_shmem_descr *shmem)
{
	if (shmem->addr) {

		switch(shmem->kind)
		{
			case pfq_shmem_virt: vfree(shmem->addr); break;
			case pfq_shmem_user: pfq_hugepages_unmap(shmem); break;
		}

		shmem->addr = NULL;
		shmem->hugepages = NULL;
		shmem->size = 0;

		pr_devel("[PFQ] shared memory freed.\n");
	}
}


size_t pfq_total_queue_mem(struct core_sock *so)
{
        return sizeof(struct pfq_shared_queue) + core_mpsc_queue_mem(so) + core_spsc_queue_mem(so) * (1 + Q_MAX_TX_QUEUES);
}


#define HUGEPAGE_SIZE  (2*1024*1024)

size_t pfq_total_queue_mem_aligned(struct core_sock *so)
{
	size_t tot_mem = pfq_total_queue_mem(so);
	return (1 + tot_mem/HUGEPAGE_SIZE) * HUGEPAGE_SIZE;
}


