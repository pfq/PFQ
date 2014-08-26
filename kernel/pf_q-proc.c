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

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/proc_fs.h>
#include <linux/seq_file.h>
#include <linux/pf_q.h>

#include <pf_q-global.h>
#include <pf_q-group.h>
#include <pf_q-bitops.h>
#include <pf_q-sparse.h>
#include <pf_q-macro.h>
#include <pf_q-proc.h>

#include <net/net_namespace.h>

struct proc_dir_entry *pfq_proc_dir = NULL;

static void
seq_printf_functional_node(struct seq_file *m, struct pfq_functional_node const *node, size_t index)
{
	char buffer[256];
        size_t n, len = 0;

	len += sprintf(buffer + len, "%4zu@%p: %pF { ", index, node, node->fun.ptr);

	for(n = 0; n < sizeof(node->fun.arg)/sizeof(node->fun.arg[0]); n++)
	{
		if (node->fun.arg[n].nelem != -1) /* vector */
		{
			if (node->fun.arg[n].value)
				len += sprintf(buffer + len, "%p[%zu] ",(void *)node->fun.arg[n].value, node->fun.arg[n].nelem);
		}
		else
		{
			if ((node->fun.arg[n].value & 0xffffLLU) == (node->fun.arg[n].value))
				len += sprintf(buffer + len, "%lld ",(int64_t)node->fun.arg[n].value);
			else
				len += sprintf(buffer + len, "%p ",(void *)node->fun.arg[n].value);
		}
	}

	if (node->next)
		len += sprintf(buffer + len, "} -> next:%p", node->next);
	else
		len += sprintf(buffer + len, "}");

	seq_printf(m, "%s\n", buffer);
}


static void
seq_printf_computation_tree(struct seq_file *m, struct pfq_computation_tree const *tree)
{
        size_t n;
        seq_printf(m, "computation size:%zu entry_point:%p\n", tree->size, tree->entry_point);
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

	for(n = 0; n < Q_MAX_GROUP; n++)
	{
		if (!pfq_groups[n].policy)
			continue;

                comp = (struct pfq_computation_tree *)atomic_long_read(&pfq_groups[n].comp);

		seq_printf(m, "group:%zu ", n);
		seq_printf_computation_tree(m, comp);
	}

	up(&group_sem);
	return 0;
}

static int pfq_proc_groups(struct seq_file *m, void *v)
{
	size_t n;

	seq_printf(m, "group: recv      drop      forward   kernel    pol pid   def.    uplane   cplane    ctrl\n");

	down(&group_sem);

	for(n = 0; n < Q_MAX_GROUP; n++)
	{
		if (!pfq_groups[n].policy)
			continue;

        	seq_printf(m, "%5zu: %-9lu %-9lu %-9lu %-9lu ", n, sparse_read(&pfq_groups[n].recv),
				   	                           sparse_read(&pfq_groups[n].drop),
					                           sparse_read(&pfq_groups[n].frwd),
					                           sparse_read(&pfq_groups[n].kern));

        	seq_printf(m, "%3d %3d ", pfq_groups[n].policy, pfq_groups[n].pid);

        	seq_printf(m, "%08zx %08zx %08zx %08zx \n", atomic_long_read(&pfq_groups[n].sock_mask[pfq_ctz(Q_CLASS_DEFAULT)]),
        				                    atomic_long_read(&pfq_groups[n].sock_mask[pfq_ctz(Q_CLASS_USER_PLANE)]),
        				                    atomic_long_read(&pfq_groups[n].sock_mask[pfq_ctz(Q_CLASS_CONTROL_PLANE)]),
        				                    atomic_long_read(&pfq_groups[n].sock_mask[63]));

	}

	up(&group_sem);
	return 0;
}

static int pfq_proc_stats(struct seq_file *m, void *v)
{
	seq_printf(m, "received  : %zu\n", sparse_read(&global_stats.recv));
	seq_printf(m, "kernel    : %zu\n", sparse_read(&global_stats.kern));
	seq_printf(m, "forwarded : %zu\n", sparse_read(&global_stats.frwd));
	seq_printf(m, "lost      : %zu\n", sparse_read(&global_stats.lost));
	seq_printf(m, "sent      : %zu\n", sparse_read(&global_stats.sent));
	seq_printf(m, "discarded : %zu\n", sparse_read(&global_stats.disc));

	return 0;
}

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
 	pfq_global_stats_reset();
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

	proc_create("groups", 0644, pfq_proc_dir, &pfq_proc_groups_fops);
	proc_create("stats",  0644, pfq_proc_dir, &pfq_proc_stats_fops);
	proc_create("comp",   0644, pfq_proc_dir, &pfq_proc_comp_fops);

	return 0;
}


int pfq_proc_fini(void)
{
	remove_proc_entry("groups", pfq_proc_dir);
	remove_proc_entry("stats", pfq_proc_dir);
	remove_proc_entry("comp",   pfq_proc_dir);
	remove_proc_entry("pfq", init_net.proc_net);

	return 0;
}


