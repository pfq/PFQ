/***************************************************************
 *                                                
 * (C) 2011-12 Nicola Bonelli <nicola.bonelli@cnit.it>   
 *             Andrea Di Pietro <andrea.dipietro@for.unipi.it>
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
#include <linux/semaphore.h>

#include <pf_q-group.h>
#include <pf_q-devmap.h>

MODULE_LICENSE("GPL");


DEFINE_SEMAPHORE(group_sem);


struct pfq_group pfq_groups[Q_MAX_GROUP];


inline void __pfq_group_ctor(int gid)
{
	// printk(KERN_INFO "[PFQ] group id:%d constructor\n", id);
	// ...
        struct pfq_group * that = &pfq_groups[gid];
	
        that->private = false;

	sparse_set(&that->recv, 0);
	sparse_set(&that->lost, 0);
	sparse_set(&that->drop, 0);

	wmb();
}

inline void __pfq_group_dtor(int gid)
{
	wmb();
	// printk(KERN_INFO "[PFQ] group id:%d destructor\n", id);

        /* remove this gid from demux matrix */
        pfq_devmap_update(map_reset, Q_ANY_DEVICE, Q_ANY_QUEUE, gid);
}


int pfq_join_free_group(int id, bool priv)
{
        int n = 0;
        down(&group_sem);
        for(; n < Q_MAX_ID; n++)
        {            
                unsigned long tmp = atomic_long_read(&pfq_groups[n].ids);
                if(tmp == 0)
                {
                        __pfq_group_ctor(n);
                        atomic_long_set(&pfq_groups[n].ids, 1L<<id);
			pfq_groups[n].private = priv;
                        up(&group_sem);
                        return n;
                }
        }
        up(&group_sem);
        return -1;
}


int
pfq_join_group(int gid, int id)
{
        unsigned long tmp;

        if (gid < 0 || gid >= Q_MAX_GROUP)
                return -1;

        down(&group_sem);
        
	tmp = atomic_long_read(&pfq_groups[gid].ids);
        if (!tmp) {
         	__pfq_group_ctor(gid);
	}
	
	if (pfq_groups[gid].private) {
        	up(&group_sem);
		return -1;
	}

	tmp |= 1L << id;
        atomic_long_set(&pfq_groups[gid].ids, tmp);

        up(&group_sem);
        return 0;
}


int
pfq_leave_group(int gid, int id)
{
        unsigned long tmp;

        if (gid < 0 || gid >= Q_MAX_GROUP)
                return -1;

        down(&group_sem);
        
	tmp = atomic_long_read(&pfq_groups[gid].ids);
        tmp &= ~(1L << id);
        atomic_long_set(&pfq_groups[gid].ids, tmp);
        if (!tmp) {
        	__pfq_group_dtor(gid);
	}
        
	up(&group_sem);
        return 0;
}


void
pfq_leave_all_groups(int id)
{
        int n = 0;
        down(&group_sem);
        for(; n < Q_MAX_ID; n++)
        {            
                unsigned long tmp = atomic_long_read(&pfq_groups[n].ids);
                if(tmp & (1L << id))
                {
                        tmp &= ~(1L<<id);
                        atomic_long_set(&pfq_groups[n].ids, tmp);
                	if (!tmp) {
                        	__pfq_group_dtor(n);
			}
		}
        }
        up(&group_sem);
}


unsigned long
pfq_get_groups(int id)
{
        unsigned long ret = 0L;
        int n = 0;
        down(&group_sem);
        for(; n < Q_MAX_ID; n++)
        {            
                if(atomic_long_read(&pfq_groups[n].ids) & (1L << id))
                {
                        ret |= (1L << n);
                }
        }
        up(&group_sem);
        return ret;
}


