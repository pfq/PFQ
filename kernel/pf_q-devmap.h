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

extern unsigned long pfq_devmap[Q_MAX_DEVICE][Q_MAX_HW_QUEUE];

extern uint8_t  pfq_devmap_monitor[Q_MAX_DEVICE];


// called from u-context
//

extern
int pfq_devmap_update(int action, int index, int queue, unsigned int id);

extern
void pfq_devmap_monitor_update(void);
  

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
    return pfq_devmap[d & Q_MAX_DEVICE_MASK][q & Q_MAX_HW_QUEUE_MASK];
}


static inline 
uint8_t pfq_devmap_monitor_get(int index)
{
    return pfq_devmap_monitor[index & Q_MAX_DEVICE_MASK];
}


#endif /* _PF_Q_DEVMAP_H_ */
