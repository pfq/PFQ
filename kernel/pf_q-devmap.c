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

#include <pf_q-devmap.h>
#include <pf_q-global.h>


MODULE_LICENSE("GPL");


void pfq_devmap_monitor_update()
{
    int i,j;
    for(i=0; i < Q_MAX_DEVICE; ++i)
    {   
        unsigned long val = 0;
        for(j=0; j < Q_MAX_HW_QUEUE; ++j)
        {
            val |= global.devmap[i][j];
        }

        global.devmap_monitor[i] = (val ? 1 : 0);
    }
}


int pfq_devmap_update(int action, int index, int queue, unsigned int id)
{
    int n = 0, i,q;
    
    if (unlikely(id >= 64))
    {
        printk(KERN_WARNING "[PF_Q] devmap_update: bad id(%u)\n",id);
        return 0; 
    }

    down(&global_sem);

    for(i=0; i < Q_MAX_DEVICE; ++i)
    {
        for(q=0; q < Q_MAX_HW_QUEUE; ++q)
        {
            if ( !pfq_devmap_equal(i,q,index,queue) )
                continue;

            /* map_set... */
            if (action == map_set) 
            {
                global.devmap[i][q] |= (1<<id), n++;
                continue;
            }

            /* map_reset */
            if ( global.devmap[i][q] & (1<<id) )
            {
                global.devmap[i][q] &= ~(1<<id), n++;
                continue;
            }
        }
    }
    
    /* update capture monitor filter... */
    
    pfq_devmap_monitor_update();

    up(&global_sem);
    return n;
}

