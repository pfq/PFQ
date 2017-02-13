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
#include <linux/module.h>
#include <linux/proc_fs.h>
#include <linux/seq_file.h>
#include <linux/pf_q.h>
#include <net/net_namespace.h>
#include <pragma/diagnostic_pop>

#include <core/lang/module.h>
#include <core/global.h>
#include <core/define.h>
#include <core/group.h>
#include <core/bitops.h>

#include <pfq/sparse.h>
#include <pfq/proc.h>
#include <pfq/memory.h>
#include <pfq/printk.h>

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,10,0)
#define PDE_DATA(a) PDE(a)->data
#endif

struct proc_dir_entry *pfq_proc_dir = NULL;

static const char proc_computations[] = "computations";
static const char proc_groups[]       = "groups";
static const char proc_stats[]        = "stats";
static const char proc_memory[]       = "memory";


static void
seq_printf_functional_node(struct seq_file *m, struct pfq_lang_functional_node const *node, size_t index)
{
	char buffer[256];

	snprintf_functional_node(buffer, sizeof(buffer), node, index);

	seq_printf(m, "%s\n", buffer);
}


static void
seq_printf_computation_tree(struct seq_file *m, struct pfq_lang_computation_tree const *tree)
{
	size_t n;

	if (tree == NULL) {
		seq_printf(m, "computation (unspecified)\n");
		return;
	}

	seq_printf(m, "computation size=%zu entry_point=%p\n", tree->size, tree->entry_point);
	for(n = 0; n < tree->size; n++)
	{
		seq_printf_functional_node(m, &tree->node[n], n);
	}
}


static int pfq_proc_comp(struct seq_file *m, void *v)
{
	struct pfq_lang_computation_tree *comp;
	size_t n;

	core_group_lock();

	for(n = 0; n < Q_CORE_MAX_GID; n++)
	{
		pfq_gid_t gid = (__force pfq_gid_t)n;

		struct core_group *this_group = core_group_get(gid);

		if (!this_group->policy)
			continue;

		comp = (struct pfq_lang_computation_tree *)atomic_long_read(&this_group->comp);

		seq_printf(m, "group=%zu ", n);
		seq_printf_computation_tree(m, comp);
	}

	core_group_unlock();
	return 0;
}

static int pfq_proc_groups(struct seq_file *m, void *v)
{
	size_t n;

	seq_printf(m, "group: recv      lost      drop      sent      disc.     failed    forward   kernel    pol pid   def.    uplane   cplane    ctrl\n");

	core_group_lock();

	for(n = 0; n < Q_CORE_MAX_GID; n++)
	{
		pfq_gid_t gid = (__force pfq_gid_t)n;

		struct core_group *this_group = core_group_get(gid);
		if (!this_group->enabled)
			continue;

		seq_printf(m, "%5zu: %-9lu %-9lu %-9lu %-9lu %-9lu %-9lu %-9lu %-9lu", n,
			   sparse_read(this_group->stats, recv),
			   sparse_read(this_group->stats, lost),
			   sparse_read(this_group->stats, drop),

			   sparse_read(this_group->stats, sent),
			   sparse_read(this_group->stats, disc),
			   sparse_read(this_group->stats, fail),

			   sparse_read(this_group->stats, frwd),
			   sparse_read(this_group->stats, kern));

		seq_printf(m, "%3d %3d ", this_group->policy, this_group->pid);

		seq_printf(m, "%08lx %08lx %08lx %08lx \n",
			   atomic_long_read(&this_group->sock_id[core_ctz(Q_CLASS_DEFAULT)]),
			   atomic_long_read(&this_group->sock_id[core_ctz(Q_CLASS_USER_PLANE)]),
			   atomic_long_read(&this_group->sock_id[core_ctz(Q_CLASS_CONTROL_PLANE)]),
			   atomic_long_read(&this_group->sock_id[Q_CLASS_MAX-1]));

	}

	core_group_unlock();
	return 0;
}

static int pfq_proc_stats(struct seq_file *m, void *v)
{
	seq_printf(m, "INPUT:\n");
	seq_printf(m, "  received  : %ld\n", sparse_read(global->percpu_stats, recv));
	seq_printf(m, "  lost      : %ld\n", sparse_read(global->percpu_stats, lost));
	seq_printf(m, "  drop      : %ld\n", sparse_read(global->percpu_stats, drop));
	seq_printf(m, "OUTPUT:\n");
	seq_printf(m, "  sent      : %ld\n", sparse_read(global->percpu_stats, sent));
	seq_printf(m, "  discarded : %ld\n", sparse_read(global->percpu_stats, disc));
	seq_printf(m, "  failed    : %ld\n", sparse_read(global->percpu_stats, fail));
	seq_printf(m, "FORWARD:\n");
	seq_printf(m, "  forwarded : %ld\n", sparse_read(global->percpu_stats, frwd));
	seq_printf(m, "  kernel    : %ld\n", sparse_read(global->percpu_stats, kern));
	return 0;
}


static int pfq_proc_memory(struct seq_file *m, void *v)
{
#ifdef PFQ_USE_SKB_POOL

	int i;

	long int push_0 = sparse_read(global->percpu_memory, pool_push[0]);
	long int push_1 = sparse_read(global->percpu_memory, pool_push[1]);

	long int pop_0  = sparse_read(global->percpu_memory, pool_pop[0]);
	long int pop_1  = sparse_read(global->percpu_memory, pool_pop[1]);

	long int empty_0  = sparse_read(global->percpu_memory, pool_empty[0]);
	long int empty_1  = sparse_read(global->percpu_memory, pool_empty[1]);

	long int norecycl_0  = sparse_read(global->percpu_memory, pool_norecycl[0]);
	long int norecycl_1  = sparse_read(global->percpu_memory, pool_norecycl[1]);

	seq_printf(m, "\nPFQ POOL (%d)        %10s %10s\n", atomic_read(&global->pool_enabled), "Rx", "Tx");
	seq_printf(m, "  push           : %10ld %10ld\n", push_0, push_1);
	seq_printf(m, "  pop            : %10ld %10ld\n", pop_0, pop_1);
	seq_printf(m, "  empty          : %10ld %10ld\n", empty_0, empty_1);
	seq_printf(m, "  norecycl       : %10ld %10ld\n\n", norecycl_0, norecycl_1);

	for_each_present_cpu(i)
	{
		struct pfq_percpu_pool *pool = per_cpu_ptr(global->percpu_pool, i);

		seq_printf(m, "CPU-%d:\n", i);
		if (pool)
		{
			long int rx = core_spsc_len(pool->rx.fifo);
			long int tx = core_spsc_len(pool->tx.fifo);

			seq_printf(m, "     pool size   : %10ld %10ld\n", rx, tx);
		}
	}

#if defined(PFQ_USE_EXTRA_COUNTERS)
	seq_printf(m, "\nPFQ POOL error\n");
	seq_printf(m, "  error shared   : %10ld\n", sparse_read(global->percpu_memory, err_shared));
	seq_printf(m, "  error cloned   : %10ld\n", sparse_read(global->percpu_memory, err_cloned));
	seq_printf(m, "  error memory   : %10ld\n", sparse_read(global->percpu_memory, err_memory));
	seq_printf(m, "  error irqdis   : %10ld\n", sparse_read(global->percpu_memory, err_irqdis));
	seq_printf(m, "  error fclone   : %10ld\n", sparse_read(global->percpu_memory, err_fclone));
	seq_printf(m, "  error nolinr   : %10ld\n", sparse_read(global->percpu_memory, err_nolinr));
	seq_printf(m, "  error nfound   : %10ld\n", sparse_read(global->percpu_memory, err_nfound));
	seq_printf(m, "  error broken   : %10ld\n", sparse_read(global->percpu_memory, err_broken));

	seq_printf(m, "\nPFQ POOL stats\n");
	seq_printf(m, "  skb.dst_drop   : %10ld\n", sparse_read(global->percpu_memory, dbg_dst_drop));
	seq_printf(m, "  skb.dtor       : %10ld\n", sparse_read(global->percpu_memory, dbg_skb_dtor));
	seq_printf(m, "  skb.frag_unref : %10ld\n", sparse_read(global->percpu_memory, dbg_skb_frag_unref));
	seq_printf(m, "  skb.free_frag  : %10ld\n", sparse_read(global->percpu_memory, dbg_skb_free_frag));
	seq_printf(m, "  skb.free_head  : %10ld\n", sparse_read(global->percpu_memory, dbg_skb_free_head));
#endif

#endif
	seq_printf(m, "\nKernel\n");
	seq_printf(m, "  skb_alloc      : %10ld\n", sparse_read(global->percpu_memory, os_alloc));
	seq_printf(m, "  skb_free       : %10ld\n", sparse_read(global->percpu_memory, os_free));

	return 0;
}

static int pfq_proc_memory_open(struct inode *inode, struct file *file)
{
	return single_open(file, pfq_proc_memory, PDE_DATA(inode));
}


static ssize_t
pfq_proc_memory_reset(struct file *file, const char __user *buf, size_t length, loff_t *ppos)
{
	return 1;
}


static const struct file_operations pfq_proc_memory_fops = {
	.owner   = THIS_MODULE,
	.open    = pfq_proc_memory_open,
	.read    = seq_read,
	.write   = pfq_proc_memory_reset,
	.llseek  = seq_lseek,
	.release = single_release,
};


static int pfq_proc_groups_open(struct inode *inode, struct file *file)
{
	return single_open(file, pfq_proc_groups, PDE_DATA(inode));
}

static int pfq_proc_comp_open(struct inode *inode, struct file *file)
{
	return single_open(file, pfq_proc_comp, PDE_DATA(inode));
}

static int pfq_proc_stats_open(struct inode *inode, struct file *file)
{
	return single_open(file, pfq_proc_stats, PDE_DATA(inode));
}

static ssize_t
pfq_proc_stats_reset(struct file *file, const char __user *buf, size_t length, loff_t *ppos)
{
	core_global_stats_reset(global->percpu_stats);
	return 1;
}


static const struct file_operations pfq_proc_stats_fops = {
	.owner   = THIS_MODULE,
	.open    = pfq_proc_stats_open,
	.read    = seq_read,
	.write   = pfq_proc_stats_reset,
	.llseek  = seq_lseek,
	.release = single_release,
};


static const struct file_operations pfq_proc_groups_fops = {
	.owner   = THIS_MODULE,
	.open    = pfq_proc_groups_open,
	.read    = seq_read,
	.llseek  = seq_lseek,
	.release = single_release,
};

static const struct file_operations pfq_proc_comp_fops = {
	.owner   = THIS_MODULE,
	.open    = pfq_proc_comp_open,
	.read    = seq_read,
	.llseek  = seq_lseek,
	.release = single_release,
};

int pfq_proc_init(void)
{
	pfq_proc_dir = proc_mkdir("pfq", init_net.proc_net);
	if (!pfq_proc_dir) {
		printk(KERN_ERR "[PFQ] error: could not create /proc/net/pfq!");
		return -ENOMEM;
	}

	proc_create(proc_computations,	0644, pfq_proc_dir, &pfq_proc_comp_fops);
	proc_create(proc_groups,	0644, pfq_proc_dir, &pfq_proc_groups_fops);
	proc_create(proc_stats,		0644, pfq_proc_dir, &pfq_proc_stats_fops);
	proc_create(proc_memory,	0644, pfq_proc_dir, &pfq_proc_memory_fops);

	return 0;
}


int pfq_proc_destruct(void)
{
	remove_proc_entry(proc_computations,	pfq_proc_dir);
	remove_proc_entry(proc_groups,		pfq_proc_dir);
	remove_proc_entry(proc_stats,		pfq_proc_dir);
	remove_proc_entry(proc_memory,		pfq_proc_dir);
	remove_proc_entry("pfq", init_net.proc_net);

	return 0;
}


