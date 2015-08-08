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

#include <pf_q-skbuff-pool.h>

#include <pf_q-percpu.h>
#include <pf_q-global.h> /* cpu_data */

#include <linux/printk.h>


void pfq_skb_pool_enable(bool value)
{
	int cpu;

	smp_wmb();
	for_each_online_cpu(cpu)
	{
		struct local_data *this_cpu = per_cpu_ptr(cpu_data, cpu);
		atomic_set(&this_cpu->enable_skb_pool, value);
	}
	smp_wmb();
}


int pfq_skb_pool_init_all(void)
{
	int cpu;
	for_each_online_cpu(cpu)
	{
		struct local_data *this_cpu = per_cpu_ptr(cpu_data, cpu);
		if (pfq_skb_pool_init(&this_cpu->tx_pool, skb_pool_size) != 0)
			return -ENOMEM;
		if (pfq_skb_pool_init(&this_cpu->rx_pool, skb_pool_size) != 0)
			return -ENOMEM;
	}

	return 0;
}


int pfq_skb_pool_free_all(void)
{
	int cpu, total = 0;

	printk(KERN_INFO "[PFQ] flushing skbuff memory pool...\n");
	for_each_online_cpu(cpu)
	{
		struct local_data *local = per_cpu_ptr(cpu_data, cpu);
		total += pfq_skb_pool_free(&local->rx_pool);
		total += pfq_skb_pool_free(&local->tx_pool);
	}

	return total;
}


int pfq_skb_pool_flush_all(void)
{
	int cpu, total = 0;

	printk(KERN_INFO "[PFQ] flushing skbuff memory pool...\n");
	for_each_online_cpu(cpu)
	{
		struct local_data *local = per_cpu_ptr(cpu_data, cpu);
		total += pfq_skb_pool_flush(&local->rx_pool);
		total += pfq_skb_pool_flush(&local->tx_pool);
	}

	return total;
}


int pfq_skb_pool_init (struct pfq_skb_pool *pool, size_t size)
{
	if (size > 0) {
		pool->skbs = kzalloc(sizeof(struct skb *) * size, GFP_KERNEL);
		if (pool->skbs == NULL) {
			printk(KERN_INFO "[PFQ] pfq_skb_pool_init: out of memory!\n");
			return -ENOMEM;
		}
	}
	else {
		pool->skbs = NULL;
	}

	pool->size  = size;
	pool->p_idx = 0;
	pool->c_idx = 0;
	return 0;
}


size_t
pfq_skb_pool_flush(struct pfq_skb_pool *pool)
{
	size_t n, total = 0;
	for(n = 0; n < pool->size; n++)
	{
		if (pool->skbs[n]) {
			total++;
			SPARSE_INC(&memory_stats.os_free);
			kfree_skb(pool->skbs[n]);
			pool->skbs[n] = NULL;
		}
	}

	pool->p_idx = 0;
	pool->c_idx = 0;
	return total;
}


size_t pfq_skb_pool_free(struct pfq_skb_pool *pool)
{
	size_t total = pfq_skb_pool_flush(pool);
	kfree(pool->skbs);
	pool->skbs = NULL;
	pool->size = 0;
	return total;
}


struct pfq_pool_stat
pfq_get_skb_pool_stats(void)
{
        struct pfq_pool_stat ret =
        {
		sparse_read(&memory_stats.os_alloc),
		sparse_read(&memory_stats.os_free),

		sparse_read(&memory_stats.pool_alloc),
		sparse_read(&memory_stats.pool_free),
		sparse_read(&memory_stats.pool_push),
		sparse_read(&memory_stats.pool_pop),

                sparse_read(&memory_stats.err_norecyl),
                sparse_read(&memory_stats.err_pop),
                sparse_read(&memory_stats.err_push),
                sparse_read(&memory_stats.err_intdis),
                sparse_read(&memory_stats.err_shared),
                sparse_read(&memory_stats.err_cloned),
                sparse_read(&memory_stats.err_memory),
	};
	return ret;
}

