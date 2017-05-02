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

#include <pfq/queue.h>
#include <pfq/shmem.h>

#include <linux/kernel.h>
#include <linux/version.h>
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/vmalloc.h>
#include <linux/pagemap.h>


static DEFINE_MUTEX(pfq_pages_mutex);

static struct pfq_pages_descr __pfq_hugepages[Q_MAX_ID];



static const char *__size_HugePage(size_t size)
{
	static char buff[256];
	switch(size)
	{
	case 2048*1024:		return "2M";
	case 4096*1024:		return "4M";
	case 16*1024*1024:	return "16M";
	case 1024*1024*1024:	return "1G";
	}
	sprintf(buff, "%zu-Byte", size);
	return buff;
}


static struct pfq_pages_descr *
get_HugePages(int id, unsigned long user_addr, size_t user_size, size_t hugepage_size, size_t size)
{
	struct pfq_pages_descr *ret = NULL, *descr;
	struct page ** hugepages;
	int nid, pinned, npages;
        void *base_addr;

	mutex_lock(&pfq_pages_mutex);

        descr = &__pfq_hugepages[id];
        if (descr->addr) {
		printk(KERN_WARNING "[PFQ] error: get_HugePages[%d]: already in use!\n", id);
		ret = NULL;
		goto done;
	}

	if (size > user_size) {
		printk(KERN_WARNING "[PFQ] error: could not allocate %zu bytes in HugePages (%zu bytes)!\n", size, hugepage_size);
		ret = NULL;
		goto done;
	}

	npages = (hugepage_size == 1024*1024*1024) ?
			1 : PAGE_ALIGN(user_size) / PAGE_SIZE;


	printk(KERN_INFO "[PFQ] %s HugePages: backed to %d pages.\n", __size_HugePage(hugepage_size), npages);

	hugepages = vmalloc(npages * sizeof(struct page *));
	if (hugepages == NULL) {
		printk(KERN_WARNING "[PFQ] error: could not allocate the pages for %s HugePages (%d pages)!\n", __size_HugePage(hugepage_size), npages);
		ret = NULL;
		goto done;
	}

	pinned = get_user_pages_fast(user_addr, npages, 1, hugepages);
	if (pinned != npages) {
		vfree(hugepages);
		printk(KERN_WARNING "[PFQ] error: could not get user HugePages (pinned pages = %d)!\n", pinned);
		ret = NULL;
		goto done;
	}

	if (hugepage_size == 1024*1024*1024) {
		base_addr = page_address(hugepages[0]);
	}
	else {
		nid = page_to_nid(hugepages[0]);
		base_addr = vm_map_ram(hugepages, npages, nid, PAGE_KERNEL);
	}

	if (!base_addr) {
		vfree(hugepages);
		printk(KERN_WARNING "[PFQ] error: vm_map_ram: mapping memory failure!\n");
		ret = NULL;
		goto done;
	}

        /* commit the slot */

	ret = descr;

	ret->hugepages = hugepages;
	ret->npages    = npages;
	ret->addr      = base_addr;
	ret->size      = user_size;

done:
	mutex_unlock(&pfq_pages_mutex);
	return ret;
}


static
int put_HugePages(int id)
{
	struct pfq_pages_descr *descr;
	int i;

	mutex_lock(&pfq_pages_mutex);

	descr = &__pfq_hugepages[id];

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

	pr_devel("[PFQ] HugePages[%d]: releasing memory of %zu pages...\n", id, descr->npages);

	descr->hugepages = NULL;
	descr->npages = 0;
	descr->addr = NULL;
	descr->size = 0;

	mutex_unlock(&pfq_pages_mutex);
	return 0;
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
        struct pfq_sock *so = pfq_sk(sock->sk);

        unsigned long size = (unsigned long)(vma->vm_end - vma->vm_start);

        if(size & (PAGE_SIZE-1)) {
                printk(KERN_WARNING "[PFQ] error: pfq_mmap: size not multiple of PAGE_SIZE!\n");
                return -EINVAL;
        }

        if(size > so->shmem.size) {
                printk(KERN_WARNING "[PFQ] error: pfq_mmap: area too large!\n");
                return -EINVAL;
        }

	printk(KERN_INFO "[PFQ] memory user memory: %lu bytes...\n", size);

        return pfq_memory_map(vma, size, so->shmem.addr, VM_LOCKED, so->shmem.kind);
}


int
pfq_hugepages_map(pfq_id_t id, struct pfq_shmem_descr *shmem, unsigned long user_addr, size_t user_size, size_t hugepage_size, size_t req_size)
{
        struct pfq_pages_descr * hpages = get_HugePages((int)id, user_addr, user_size, hugepage_size, req_size);

	if (!hpages) {
		printk(KERN_WARNING "[PFQ] mapping memory failure.\n");
		return -EPERM;
	}

	shmem->addr = hpages->addr;
	shmem->id   = (int)id;
        shmem->size = req_size;
	shmem->kind = pfq_shmem_user;
        shmem->hugepages_descr = hpages;

	printk(KERN_INFO "[PFQ|%d] mapped memory: %zu bytes.\n", (int)id, req_size);
	return 0;
}


int
pfq_hugepages_unmap(struct pfq_shmem_descr *shmem)
{
	int rc;
	printk(KERN_INFO "[PFQ|%d] unmapping memory: %zu bytes...\n", shmem->id, shmem->size);
	rc = put_HugePages(shmem->id);
        if (rc < 0)
		return rc;

	shmem->addr = NULL;
	shmem->hugepages_descr = NULL;
	shmem->size = 0;
	return 0;
}


int
pfq_vmalloc_user(pfq_id_t id, struct pfq_shmem_descr *shmem, size_t mem_size)
{
	size_t tot_mem = PAGE_ALIGN(mem_size);
        void *addr;

	pr_devel("[PFQ] allocating shared memory...\n");

	addr = vmalloc_user(tot_mem);
	if (addr == NULL) {
		printk(KERN_WARNING "[PFQ] error: shmem: out of memory (vmalloc %zu bytes)!", tot_mem);
		return -ENOMEM;
	}

	shmem->addr = addr;
	shmem->id   = (int)id;
        shmem->size = tot_mem;
	shmem->kind = pfq_shmem_virt;
        shmem->hugepages_descr = NULL;

	pr_devel("[PFQ] total shared memory: %zu bytes.\n", tot_mem);
	return 0;
}


int
pfq_shared_memory_alloc(pfq_id_t id, struct pfq_shmem_descr *shmem, unsigned long user_addr, size_t user_size, size_t hugepage_size, size_t req_size)
{
	if (hugepage_size) {
		if (pfq_hugepages_map(id, shmem, user_addr, user_size, hugepage_size, req_size) < 0)
			return -ENOMEM;
	}
	else {
		if (pfq_vmalloc_user(id, shmem, req_size) < 0)
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
		shmem->hugepages_descr = NULL;
		shmem->size = 0;

		pr_devel("[PFQ] shared memory freed.\n");
	}
}


size_t pfq_total_queue_mem(struct pfq_sock *so)
{
        return sizeof(struct pfq_shared_queue) + pfq_mpsc_queue_mem(so) + pfq_spsc_queue_mem(so) * (1 + Q_MAX_TX_QUEUES);
}


#define HUGEPAGE_SIZE  (2*1024*1024)

size_t pfq_total_queue_mem_aligned(struct pfq_sock *so)
{
	size_t tot_mem = pfq_total_queue_mem(so);
	return (1 + tot_mem/HUGEPAGE_SIZE) * HUGEPAGE_SIZE;
}


