/***************************************************************
 *
 * (C) 2014 Nicola Bonelli <nicola@pfq.io>
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

#include <pf_q-module.h>
#include <pf_q-global.h>
#include <pf_q-group.h>
#include <pf_q-bitops.h>
#include <pf_q-sparse.h>
#include <pf_q-macro.h>
#include <pf_q-proc.h>
#include <pf_q-memory.h>
#include <pf_q-printk.h>

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,10,0)
#define PDE_DATA(a) PDE(a)->data
#endif

struct proc_dir_entry *pfq_proc_dir = NULL;

static const char proc_computations[] = "computations";
static const char proc_groups[]       = "groups";
static const char proc_stats[]        = "stats";

#ifdef PFQ_USE_EXTENDED_PROC
static const char proc_memory[]       = "memory";
#endif


static void
seq_printf_functional_node(struct seq_file *m, struct pfq_functional_node const *node, size_t index)
{
	char buffer[256];

	snprintf_functional_node(buffer, sizeof(buffer), node, index);

	seq_printf(m, "%s\n", buffer);
}


static void
seq_printf_computation_tree(struct seq_file *m, struct pfq_computation_tree const *tree)
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
	struct pfq_computation_tree *comp;
	size_t n;

	down(&group_sem);

	for(n = 0; n < Q_MAX_GID; n++)
	{
		pfq_gid_t gid = (__force pfq_gid_t)n;

		struct pfq_group *this_group = pfq_get_group(gid);

		if (!this_group->policy)
			continue;

		comp = (struct pfq_computation_tree *)atomic_long_read(&this_group->comp);

		seq_printf(m, "group=%zu ", n);
		seq_printf_computation_tree(m, comp);
	}

	up(&group_sem);
	return 0;
}

static int pfq_proc_groups(struct seq_file *m, void *v)
{
	size_t n;

	seq_printf(m, "group: recv      drop      forward   kernel    disc      aborted   pol pid   def.    uplane   cplane    ctrl\n");

	down(&group_sem);

	for(n = 0; n < Q_MAX_GID; n++)
	{
		pfq_gid_t gid = (__force pfq_gid_t)n;

		struct pfq_group *this_group = pfq_get_group(gid);
		if (!this_group->policy)
			continue;

		seq_printf(m, "%5zu: %-9lu %-9lu %-9lu %-9lu %-9lu %-9lu", n,
			   sparse_read(&this_group->stats.recv),
			   sparse_read(&this_group->stats.drop),
			   sparse_read(&this_group->stats.frwd),
			   sparse_read(&this_group->stats.kern),
			   sparse_read(&this_group->stats.disc),
			   sparse_read(&this_group->stats.abrt));

		seq_printf(m, "%3d %3d ", this_group->policy, this_group->pid);

		seq_printf(m, "%08lx %08lx %08lx %08lx \n",
			   atomic_long_read(&this_group->sock_mask[pfq_ctz(Q_CLASS_DEFAULT)]),
			   atomic_long_read(&this_group->sock_mask[pfq_ctz(Q_CLASS_USER_PLANE)]),
			   atomic_long_read(&this_group->sock_mask[pfq_ctz(Q_CLASS_CONTROL_PLANE)]),
			   atomic_long_read(&this_group->sock_mask[63]));

	}

	up(&group_sem);
	return 0;
}

static int pfq_proc_stats(struct seq_file *m, void *v)
{
	seq_printf(m, "INPUT:\n");
	seq_printf(m, "  received  : %ld\n", sparse_read(&global_stats.recv));
	seq_printf(m, "  lost      : %ld\n", sparse_read(&global_stats.lost));
	seq_printf(m, "OUTPUT:\n");
	seq_printf(m, "  sent      : %ld\n", sparse_read(&global_stats.sent));
	seq_printf(m, "  kernel    : %ld\n", sparse_read(&global_stats.kern));
	seq_printf(m, "  forwarded : %ld\n", sparse_read(&global_stats.frwd));
	seq_printf(m, "  discarded : %ld\n", sparse_read(&global_stats.disc));
	seq_printf(m, "  aborted   : %ld\n", sparse_read(&global_stats.abrt));
#ifdef PFQ_USE_EXTENDED_PROC
	seq_printf(m, "SCHEDULE:\n");
	seq_printf(m, "  poll      : %ld\n", sparse_read(&global_stats.poll));
	seq_printf(m, "  wakeup    : %ld\n", sparse_read(&global_stats.wake));
#endif
	return 0;
}


#ifdef PFQ_USE_EXTENDED_PROC

static int pfq_proc_memory(struct seq_file *m, void *v)
{
	long int push = sparse_read(&memory_stats.pool_push);
	long int pop  = sparse_read(&memory_stats.pool_pop);

	seq_printf(m, "OS:\n");
	seq_printf(m, "  alloc          : %ld\n", sparse_read(&memory_stats.os_alloc));
	seq_printf(m, "  free           : %ld\n", sparse_read(&memory_stats.os_free));
	seq_printf(m, "POOL:\n");
	seq_printf(m, "  alloc          : %ld\n", sparse_read(&memory_stats.pool_alloc));
	seq_printf(m, "  free           : %ld\n", sparse_read(&memory_stats.pool_free));
	seq_printf(m, "  push           : %ld\n", push);
	seq_printf(m, "  pop            : %ld\n", pop);
	seq_printf(m, "  size           : %ld\n", push - pop);
	seq_printf(m, "ERROR:\n");
	seq_printf(m, "  error norecyl  : %ld\n", sparse_read(&memory_stats.err_norecyl));
	seq_printf(m, "  error pop      : %ld\n", sparse_read(&memory_stats.err_pop));
	seq_printf(m, "  error push     : %ld\n", sparse_read(&memory_stats.err_push));
	seq_printf(m, "  error intdisab : %ld\n", sparse_read(&memory_stats.err_intdis));
	seq_printf(m, "  error shared   : %ld\n", sparse_read(&memory_stats.err_shared));
	seq_printf(m, "  error cloned   : %ld\n", sparse_read(&memory_stats.err_cloned));
	seq_printf(m, "  error memory   : %ld\n", sparse_read(&memory_stats.err_memory));
	return 0;
}

static int pfq_proc_memory_open(struct inode *inode, struct file *file)
{
	return single_open(file, pfq_proc_memory, PDE_DATA(inode));
}

static ssize_t
pfq_proc_memory_reset(struct file *file, const char __user *buf, size_t length, loff_t *ppos)
{
	pfq_memory_stats_reset(&memory_stats);
	pfq_skb_pool_flush_all();
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


#endif


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
	pfq_global_stats_reset(&global_stats);
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
		printk(KERN_INFO "[PFQ] could not create /proc/net/pfq");
		return -ENOMEM;
	}

	proc_create(proc_computations,	0644, pfq_proc_dir, &pfq_proc_comp_fops);
	proc_create(proc_groups,	0644, pfq_proc_dir, &pfq_proc_groups_fops);
	proc_create(proc_stats,		0644, pfq_proc_dir, &pfq_proc_stats_fops);
#ifdef PFQ_USE_EXTENDED_PROC
	proc_create(proc_memory,	0644, pfq_proc_dir, &pfq_proc_memory_fops);
#endif

	return 0;
}


int pfq_proc_fini(void)
{
	remove_proc_entry(proc_computations,	pfq_proc_dir);
	remove_proc_entry(proc_groups,		pfq_proc_dir);
	remove_proc_entry(proc_stats,		pfq_proc_dir);
#ifdef PFQ_USE_EXTENDED_PROC
	remove_proc_entry(proc_memory,		pfq_proc_dir);
#endif
	remove_proc_entry("pfq", init_net.proc_net);

	return 0;
}


