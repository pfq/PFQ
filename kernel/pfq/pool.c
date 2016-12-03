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
pfq_skb_pool_flush(pfq_skb_pool_t *pool)
{
	size_t total = 0, in_use = 0;
	struct sk_buff *skb;
	while ((skb = pfq_skb_pool_pop(pool)))
	{
		sparse_inc(global->percpu_mem_stats, os_free);
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
int pfq_skb_pool_init (pfq_skb_pool_t **pool, size_t size, size_t skb_len, int cpu)
{
	int total = 0;
	if (!*pool) {

		struct sk_buff *skb;

		*pool = core_spsc_init(size, cpu);
		if (!*pool) {
			printk(KERN_ERR "[PFQ] pfq_skb_pool_init: out of memory!\n");
			return -ENOMEM;
		}

		for(; total < size; total++)
		{
			skb = __alloc_skb(skb_len, GFP_KERNEL, 0, cpu_to_node(cpu));
			if (!skb)
				return total;

			if (skb_linearize(skb) < 0) {
				kfree_skb(skb);
				return total;
			}

			skb->nf_trace = 1;

			pfq_skb_pool_push(*pool, skb);
			sparse_inc(global->percpu_mem_stats, os_alloc);
		}
	}

	return size;
}



size_t pfq_skb_pool_free(pfq_skb_pool_t **pool)
{
	size_t total = 0;
	if (*pool) {
		total = pfq_skb_pool_flush(*pool);
		kfree(*pool);
		*pool = NULL;
	}
	return total;
}


struct core_pool_stat
pfq_get_skb_pool_stats(void)
{
        struct core_pool_stat ret =
        {
           .os_alloc      = sparse_read(global->percpu_mem_stats, os_alloc)
        ,  .os_free       = sparse_read(global->percpu_mem_stats, os_free)

        ,  .pool_pop[0]   = sparse_read(global->percpu_mem_stats, pool_pop[0])
        ,  .pool_pop[1]   = sparse_read(global->percpu_mem_stats, pool_pop[1])
        ,  .pool_pop[2]   = sparse_read(global->percpu_mem_stats, pool_pop[2])

        ,  .pool_push[0]  = sparse_read(global->percpu_mem_stats, pool_push[0])
        ,  .pool_push[1]  = sparse_read(global->percpu_mem_stats, pool_push[1])
        ,  .pool_push[2]  = sparse_read(global->percpu_mem_stats, pool_push[2])

        ,  .pool_empty[0]  = sparse_read(global->percpu_mem_stats, pool_empty[0])
        ,  .pool_empty[1]  = sparse_read(global->percpu_mem_stats, pool_empty[1])
        ,  .pool_empty[2]  = sparse_read(global->percpu_mem_stats, pool_empty[2])

        ,  .pool_norecycl[0]  = sparse_read(global->percpu_mem_stats, pool_norecycl[0])
        ,  .pool_norecycl[1]  = sparse_read(global->percpu_mem_stats, pool_norecycl[1])
        ,  .pool_norecycl[2]  = sparse_read(global->percpu_mem_stats, pool_norecycl[2])

        ,  .err_shared    = sparse_read(global->percpu_mem_stats, err_shared)
        ,  .err_cloned    = sparse_read(global->percpu_mem_stats, err_cloned)
        ,  .err_memory    = sparse_read(global->percpu_mem_stats, err_memory)
        ,  .err_irqdis    = sparse_read(global->percpu_mem_stats, err_irqdis)
        ,  .err_nolinr    = sparse_read(global->percpu_mem_stats, err_nolinr)
	};
	return ret;
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

			if ((n = pfq_skb_pool_init(&pool->tx_multi.fifo_sml, global->skb_pool_size, PFQ_SKB_POOL_SML, cpu)) < 0)
				return -ENOMEM;
			total += n;
			if ((n = pfq_skb_pool_init(&pool->tx_multi.fifo_mid, global->skb_pool_size, PFQ_SKB_POOL_MID, cpu)) < 0)
				return -ENOMEM;
			total += n;
			if ((n = pfq_skb_pool_init(&pool->tx_multi.fifo_lrg, global->skb_pool_size, PFQ_SKB_POOL_LRG, cpu)) < 0)
				return -ENOMEM;
			total += n;

			if ((n = pfq_skb_pool_init(&pool->rx_multi.fifo_sml, global->skb_pool_size, PFQ_SKB_POOL_SML, cpu)) < 0)
				return -ENOMEM;
			total += n;
			if ((n = pfq_skb_pool_init(&pool->rx_multi.fifo_mid, global->skb_pool_size, PFQ_SKB_POOL_MID, cpu)) < 0)
				return -ENOMEM;
			total += n;
			if ((n = pfq_skb_pool_init(&pool->rx_multi.fifo_lrg, global->skb_pool_size, PFQ_SKB_POOL_LRG, cpu)) < 0)
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
			total += pfq_skb_pool_free(&pool->rx_multi.fifo_sml);
			total += pfq_skb_pool_free(&pool->rx_multi.fifo_mid);
			total += pfq_skb_pool_free(&pool->rx_multi.fifo_lrg);
			spin_lock(&pool->tx_lock);
			total += pfq_skb_pool_free(&pool->tx_multi.fifo_sml);
			total += pfq_skb_pool_free(&pool->tx_multi.fifo_mid);
			total += pfq_skb_pool_free(&pool->tx_multi.fifo_lrg);
			spin_unlock(&pool->tx_lock);
		}
	}

	printk(KERN_INFO "[PFQ] %d sk_buff freed!\n", total);
	return total;
}


void pfq_skb_pool_toggle(bool value)
{
	int cpu;

	smp_wmb();
	for_each_present_cpu(cpu)
	{
		struct pfq_percpu_pool *pool = per_cpu_ptr(global->percpu_pool, cpu);
		if (pool)
			atomic_set(&pool->enable, value);
	}
	smp_wmb();
}

