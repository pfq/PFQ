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

#include <pfq/percpu.h>
#include <pfq/global.h>
#include <pfq/memory.h>
#include <pfq/pool.h>
#include <pfq/printk.h>


static
struct sk_buff *
pfq_build_skb(void *base, void *data, unsigned int frag_size)
{
	struct skb_shared_info *shinfo;
	struct sk_buff *skb;
	unsigned int size = frag_size;

	skb = (struct sk_buff *)base;

	size -= SKB_DATA_ALIGN(sizeof(struct skb_shared_info));
	memset(skb, 0, offsetof(struct sk_buff, tail));

	skb->truesize = SKB_TRUESIZE(size);
	atomic_set(&skb->users, 1);
	skb->head = data;
	skb->data = data;
	skb_reset_tail_pointer(skb);
	skb->end = skb->tail + size;
	skb->mac_header = (typeof(skb->mac_header))~0U;
	skb->transport_header = (typeof(skb->transport_header))~0U;

	/* make sure we initialize shinfo sequentially */

	shinfo = skb_shinfo(skb);
	memset(shinfo, 0, offsetof(struct skb_shared_info, dataref));
	atomic_set(&shinfo->dataref, 1);
	kmemcheck_annotate_variable(shinfo->destructor_arg);
	return skb;
}


static
size_t
pfq_skb_pool_flush(struct pfq_skb_pool *pool)
{
	printk(KERN_INFO "[PFQ] pool: releasing base@%p (%zu bytes)...\n", pool->base, pool->base_size);
	printk(KERN_INFO "[PFQ] pool: releasing data@%p (%zu bytes)...\n", pool->data, pool->data_size);

	if (pool->base) {
		pfq_free_pages(pool->base, pool->base_size);
		pool->base = NULL;
		pool->base_size = 0;
	}

	if (pool->data) {
		pfq_free_pages(pool->data, pool->data_size);
		pool->data = NULL;
		pool->data_size = 0;
	}

	return 0;
}


static
int pfq_skb_pool_init(struct pfq_skb_pool *pool, size_t pool_size, size_t skb_len, int idx, int cpu)
{
	struct sk_buff *skb;
	int total = 0;

	if (!pool)
		return -1;

	if (pool->fifo != NULL)
		return 0;

	/* allocate pages for skb */

	pool->base = pfq_malloc_pages( global->max_pool_size * 2 * sizeof(struct sk_buff), GFP_KERNEL);
	pool->base_size = pool->base ? global->max_pool_size * 2 * sizeof(struct sk_buff) :  0;
	if (!pool->base) {
		printk(KERN_ERR "[PFQ] pfq_skb_pool_init(base): could not allocate memory!\n");
		goto err;
	}

	pool->data = pfq_malloc_pages( global->max_pool_size * global->max_slot_size, GFP_KERNEL);
	pool->data_size = pool->data ? global->max_pool_size * global->max_slot_size: 0;
	if (!pool->data) {
		printk(KERN_ERR "[PFQ] pfq_skb_pool_init(data): could not allocate memory!\n");
		goto err;
	}

	printk(KERN_INFO "[PFQ] pool: base@%p (%zu bytes).\n", pool->base, pool->base_size);
	printk(KERN_INFO "[PFQ] pool: data@%p (%zu bytes).\n", pool->data, pool->data_size);

	/* one slot is added by the queue to distinguish between full and empty state */
	pool->fifo = pfq_spsc_init(pool_size + PFQ_POOL_CACHELINE_PAD-1, cpu);
	if (!pool->fifo) {
		printk(KERN_ERR "[PFQ] pfq_skb_pool_init(fifo): out of memory!\n");
		goto err;
	}

	for(; total < pool_size; total++)
	{
                void *buf, *data;

                buf  = pool->base + total * sizeof(struct sk_buff);
		data = pool->data + total * global->max_slot_size;

		skb = pfq_build_skb(buf, data, global->max_slot_size);

		skb->peeked = 1;

		PFQ_CB(skb)->id = total;
		PFQ_CB(skb)->pool = idx;
		PFQ_CB(skb)->head = skb->head;

		memcpy(skb + global->max_pool_size, skb, sizeof(struct sk_buff));

		pfq_spsc_push(pool->fifo, skb);
		sparse_inc(global->percpu_memory, os_alloc);
	}

	return pool_size;
err:
	pfq_skb_pool_flush(pool);
	return -ENOMEM;
}



static size_t
pfq_skb_pool_free(struct pfq_skb_pool *pool, size_t pool_size)
{
	if (pool) {
		pfq_skb_pool_flush(pool);
		pfq_spsc_free(pool_size, pool->fifo, NULL);
		pool->fifo = NULL;
	}
	return 0;
}


struct pfq_pool_stats
pfq_get_skb_pool_stats(void)
{
        return (struct pfq_pool_stats)
        {
           .os_alloc         = sparse_read(global->percpu_memory, os_alloc)
        ,  .os_free          = sparse_read(global->percpu_memory, os_free)

        ,  .pool_pop[0]	     = sparse_read(global->percpu_memory, pool_pop[0])
        ,  .pool_pop[1]      = sparse_read(global->percpu_memory, pool_pop[1])

        ,  .pool_push[0]     = sparse_read(global->percpu_memory, pool_push[0])
        ,  .pool_push[1]     = sparse_read(global->percpu_memory, pool_push[1])

        ,  .pool_empty[0]    = sparse_read(global->percpu_memory, pool_empty[0])
        ,  .pool_empty[1]    = sparse_read(global->percpu_memory, pool_empty[1])

        ,  .pool_norecycl[0] = sparse_read(global->percpu_memory, pool_norecycl[0])
        ,  .pool_norecycl[1] = sparse_read(global->percpu_memory, pool_norecycl[1])

        ,  .err_shared       = sparse_read(global->percpu_memory, err_shared)
        ,  .err_cloned       = sparse_read(global->percpu_memory, err_cloned)
        ,  .err_memory       = sparse_read(global->percpu_memory, err_memory)
        ,  .err_irqdis       = sparse_read(global->percpu_memory, err_irqdis)
        ,  .err_fclone 	     = sparse_read(global->percpu_memory, err_fclone)
        ,  .err_nolinr       = sparse_read(global->percpu_memory, err_nolinr)
        ,  .err_nfound 	     = sparse_read(global->percpu_memory, err_nfound)
        ,  .err_broken	     = sparse_read(global->percpu_memory, err_broken)

	, .dbg_dst_drop       = sparse_read(global->percpu_memory, dbg_dst_drop)
	, .dbg_skb_dtor	      = sparse_read(global->percpu_memory, dbg_skb_dtor)
	, .dbg_skb_frag_unref = sparse_read(global->percpu_memory, dbg_skb_frag_unref)
	, .dbg_skb_free_frag  = sparse_read(global->percpu_memory, dbg_skb_free_frag)
	, .dbg_skb_free_head  = sparse_read(global->percpu_memory, dbg_skb_free_head)

	};
}

/* public */

int pfq_skb_pool_init_all(void)
{
	int cpu;
	for_each_present_cpu(cpu)
	{
		struct pfq_percpu_pool *pool = per_cpu_ptr(global->percpu_pool, cpu);
		if (pool)
		{
			spin_lock_init(&pool->tx_lock);

			if (pfq_skb_pool_init(&pool->rx, global->skb_rx_pool_size, global->max_slot_size, 0, cpu) < 0)
				goto err;
			if (pfq_skb_pool_init(&pool->tx, global->skb_tx_pool_size, global->max_slot_size, 1, cpu) < 0)
				goto err;
		}
	}

	return 0;
err:
	pfq_skb_pool_free_all();
	return -ENOMEM;
}


int pfq_skb_pool_free_all(void)
{
	int cpu;

	for_each_present_cpu(cpu)
	{
		struct pfq_percpu_pool *pool = per_cpu_ptr(global->percpu_pool, cpu);
		if (pool) {
			spin_lock(&pool->tx_lock);
			pfq_skb_pool_free(&pool->rx, global->skb_rx_pool_size);
			pfq_skb_pool_free(&pool->tx, global->skb_tx_pool_size);
			spin_unlock(&pool->tx_lock);
		}
	}

	return 0;
}



