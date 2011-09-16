/***************************************************************
 *                                                
 * (C) 2011 - Nicola Bonelli <nicola.bonelli@cnit.it>   
 *            Andrea Di Pietro <andrea.dipietro@for.unipi.it>
 *
 ****************************************************************/

#ifndef _PF_Q_DEVMAP_H_
#define _PF_Q_DEVMAP_H_ 

#define __PFQ_MODULE__

#include <linux/pf_q.h>
#include <pf_q-priv.h>

/* pfq devmap */

enum { map_reset, map_set };

extern unsigned long pfq_devmap[MAX_NUM_DEVICE][MAX_NUM_HW_QUEUE];

extern atomic_long_t pfq_vector[MAX_NUM_PFQ]; 


// called from uc.
//

extern
int pfq_devmap_update(int action, int index, int queue, unsigned int id);
  
static inline 
int pfq_devmap_get_free_id(struct pfq_opt * pq)
{
    int n = 0;
    for(; n < MAX_NUM_PFQ; n++)
    {            
        if (!atomic_long_cmpxchg(pfq_vector + n, 0, (long)pq))
            return n;         
    }
    return -1;
}

static inline 
struct pfq_opt * 
pfq_devmap_get_opt(unsigned int id)
{
    if (unlikely(id >= 64))
    {
        printk(KERN_WARNING "[PF_Q]: pfq_devmap_freeid: bad id(%u)\n", id);
        return 0;
    }
    return (struct pfq_opt *)pfq_vector[id].counter;  // atomic_read not required here.
}


static inline 
void pfq_devmap_release_id(unsigned int id)
{
    if (unlikely(id >= 64))
    {
        printk(KERN_WARNING "[PF_Q]: pfq_devmap_freeid: bad id(%u)\n", id);
        return;
    }
    atomic_long_set(pfq_vector + id, 0);
}


static inline 
int pfq_devmap_equal(int i1, int q1, int i2, int q2)
{
    return  (i1 == i2 && q1 == q2) ||
            (i1 == i2 && q2 == -1) ||
            (i2 == -1 && q1 == q2) ||
            (i2 == -1 && q2 == -1);
}


static inline 
unsigned long pfq_devmap_get(int d, int q)
{
    return pfq_devmap[d & MAX_NUM_DEVICE_MASK][q & MAX_NUM_HW_QUEUE_MASK];
}


#endif /* _PF_Q_DEVMAP_H_ */
