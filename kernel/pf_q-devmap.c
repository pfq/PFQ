/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#include <pf_q-devmap.h>


/* devmap */


DEFINE_SEMAPHORE(devmap_sem);

atomic_long_t   pfq_devmap [Q_MAX_DEVICE][Q_MAX_HW_QUEUE];
atomic_t        pfq_devmap_monitor [Q_MAX_DEVICE];


void pfq_devmap_monitor_update(void)
{
    int i,j;
    for(i=0; i < Q_MAX_DEVICE; ++i)
    {
        unsigned long val = 0;
        for(j=0; j < Q_MAX_HW_QUEUE; ++j)
        {
            val |= atomic_long_read(&pfq_devmap[i][j]);
        }

        atomic_set(&pfq_devmap_monitor[i], val ? 1 : 0);
    }
}


int pfq_devmap_update(int action, int index, int queue, int gid)
{
    int n = 0, i,q;

    if (unlikely(gid >= Q_MAX_GROUP || gid < 0))
    {
        pr_devel("[PF_Q] devmap_update: bad gid(%u)\n",gid);
        return 0;
    }

    down(&devmap_sem);

    for(i=0; i < Q_MAX_DEVICE; ++i)
    {
        for(q=0; q < Q_MAX_HW_QUEUE; ++q)
        {
            unsigned long tmp;

            if (!__pfq_devmap_equal(i, q, index, queue))
                continue;

            /* map_set... */
            if (action == map_set)
            {
                tmp = atomic_long_read(&pfq_devmap[i][q]);
                tmp |= 1L << gid;
                atomic_long_set(&pfq_devmap[i][q], tmp);
                n++;
                continue;
            }

            /* map_reset */
            tmp = atomic_long_read(&pfq_devmap[i][q]);
            if (tmp & (1L<<gid))
            {
                tmp &= ~(1L<<gid);
                atomic_long_set(&pfq_devmap[i][q], tmp);
                n++;
                continue;
            }
        }
    }

    /* update capture monitor filter... */

    pfq_devmap_monitor_update();

    up(&devmap_sem);

    return n;
}

