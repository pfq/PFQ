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

#include <core/percpu.h>
#include <core/global.h>

#include <pfq/memory.h>
#include <pfq/percpu.h>
#include <pfq/pool.h>
#include <pfq/printk.h>


/* private */

static
size_t
pfq_skb_pool_flush(struct core_spsc_fifo *pool)
{
	size_t total = 0, in_use = 0;
	struct sk_buff *skb;
	while ((skb = core_spsc_pop(pool)))
	{
		sparse_inc(global->percpu_memory, os_free);
		if (atomic_read(&skb->users) > 1) {
			in_use++;
		}
		kfree_skb(skb);
		total++;
	}

	if (in_use)
		printk(KERN_WARNING "[PFQ] pfq_skb_pool_flush: pool@%p -> %zu buffers still in use!!\n", pool, in_use);

	return total;
}


static
int pfq_skb_pool_init (struct core_spsc_fifo **pool, size_t pool_size, size_t skb_len, int idx, int cpu)
{
	int total = 0;
	if (!*pool) {

		struct sk_buff *skb;

		/* one slot is already added by the queue to distinguish between full and empty state */
		*pool = core_spsc_init(pool_size + PFQ_POOL_CACHELINE_PAD-1, cpu);
		if (!*pool) {
			printk(KERN_ERR "[PFQ] pfq_skb_pool_init: out of memory!\n");
			return -ENOMEM;
		}

		for(; total < pool_size; total++)
		{
			skb = __alloc_skb(skb_len, GFP_KERNEL, 0, cpu_to_node(cpu));
			if (!skb)
				return total;

			if(skb_linearize(skb) < 0) {
				__kfree_skb(skb);
				return total;
			}

			skb->nf_trace = 1;

			PFQ_CB(skb)->id = total;
			PFQ_CB(skb)->addr = skb;
			PFQ_CB(skb)->pool = idx;

			core_spsc_push(*pool, skb);
			sparse_inc(global->percpu_memory, os_alloc);
		}
	}

	return pool_size;
}



static size_t
skb_pool_free(struct core_spsc_fifo **pool)
{
	size_t total = 0;
	if (*pool) {
		total = pfq_skb_pool_flush(*pool);
		kfree(*pool);
		*pool = NULL;
	}
	return total;
}


struct core_pool_stats
pfq_get_skb_pool_stats(void)
{
        return (struct core_pool_stats)
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
	int cpu, total = 0;
	for_each_present_cpu(cpu)
	{
		struct pfq_percpu_pool *pool = per_cpu_ptr(global->percpu_pool, cpu);
		if (pool) {
                        int n;

			spin_lock_init(&pool->tx_lock);

			if ((n = pfq_skb_pool_init(&pool->rx.fifo, global->skb_pool_size, PFQ_POOL_SKB_SIZE, 0, cpu)) < 0)
				return -ENOMEM;
			total += n;
			if ((n = pfq_skb_pool_init(&pool->tx.fifo, global->skb_pool_size, PFQ_POOL_SKB_SIZE, 1, cpu)) < 0)
				return -ENOMEM;
			total += n;
		}
	}

	printk(KERN_INFO "[PFQ] %d sk_buff allocated!\n", total);

	return 0;
}


int pfq_skb_pool_free_all(void)
{
	int cpu, total = 0;

	for_each_present_cpu(cpu)
	{
		struct pfq_percpu_pool *pool = per_cpu_ptr(global->percpu_pool, cpu);
		if (pool) {
			total += skb_pool_free(&pool->rx.fifo);
			spin_lock(&pool->tx_lock);
			total += skb_pool_free(&pool->tx.fifo);
			spin_unlock(&pool->tx_lock);
		}
	}

	printk(KERN_INFO "[PFQ] %d sk_buff freed!\n", total);
	return total;
}



