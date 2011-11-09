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

MODULE_LICENSE("GPL");

unsigned long pfq_devmap[Q_MAX_DEVICE][Q_MAX_HW_QUEUE];

static DEFINE_SEMAPHORE(devmap_sem);


int pfq_devmap_update(int action, int index, int queue, unsigned int id)
{
    int n = 0, i,q;
    
    if (unlikely(id >= 64))
    {
        printk(KERN_WARNING "[PF_Q]: devmap_update: bad id(%u)\n",id);
        return 0; 
    }

    down(&devmap_sem);

    for(i=0; i < Q_MAX_DEVICE; ++i)
    {
        for(q=0; q < Q_MAX_HW_QUEUE; ++q)
        {
            if ( !pfq_devmap_equal(i,q,index,queue) )
                continue;

            /* map_set... */
            if (action == map_set) 
            {
                pfq_devmap[i][q] |= (1<<id), n++;
                continue;
            }

            /* map_reset */
            if ( pfq_devmap[i][q] & (1<<id) )
            {
                pfq_devmap[i][q] &= ~(1<<id), n++;
                continue;
            }
        }
    }
    
    up(&devmap_sem);
    return n;
}

