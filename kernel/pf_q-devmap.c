/***************************************************************
 *                                                
 * (C) 2011 - Nicola Bonelli <nicola.bonelli@cnit.it>   
 *            Andrea Di Pietro <andrea.dipietro@for.unipi.it>
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
        printk(KERN_WARNING "[PF_Q]: devmap_update: bad id(%u)\n",id);
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

