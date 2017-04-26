/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#include <pfq/devmap.h>
#include <pfq/group.h>
#include <pfq/kcompat.h>
#include <pfq/printk.h>
#include <pfq/thread.h>


void pfq_devmap_toggle_update(void)
{
    int i,j;
    for(i=0; i < Q_MAX_DEVICE; ++i)
    {
        unsigned long val = 0;
        for(j=0; j < Q_MAX_QUEUE; ++j)
        {
            val |= (unsigned long)atomic_long_read(&global->devmap[i][j]);
        }

        atomic_set(&global->devmap_toggle[i], val ? 1 : 0);
    }
}


int pfq_devmap_update(int action, int index, int queue, pfq_gid_t gid)
{
    int n = 0, i,q;

    if (unlikely((__force int)gid >= Q_MAX_GID ||
		 (__force int)gid < 0)) {
        pr_devel("[PF_Q] devmap_update: bad gid (%u)\n",gid);
        return 0;
    }

    mutex_lock(&global->devmap_lock);

    for(i=0; i < Q_MAX_DEVICE; ++i)
    {
        for(q=0; q < Q_MAX_QUEUE; ++q)
        {
            long tmp;

            if (!pfq_devmap_equal(i, q, index, queue))
                continue;

            /* map_set... */
            if (action == Q_DEVMAP_SET) {

                tmp = atomic_long_read(&global->devmap[i][q]);
                tmp |= 1L << (__force int)gid;
                atomic_long_set(&global->devmap[i][q], tmp);
                n++;
                continue;
            }

            /* map_reset */
            tmp = atomic_long_read(&global->devmap[i][q]);
            if (tmp & (1L << (__force int)gid)) {
                tmp &= ~(1L << (__force int)gid);
                atomic_long_set(&global->devmap[i][q], tmp);
                n++;
                continue;
            }
        }
    }

    /* update capture toggle filter... */

    pfq_devmap_toggle_update();

    mutex_unlock(&global->devmap_lock);
    return n;
}

