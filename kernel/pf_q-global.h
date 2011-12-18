#ifndef _PF_Q_GLOBAL_H_
#define _PF_Q_GLOBAL_H_ 

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/semaphore.h>

#define __PFQ_MODULE__
#include <linux/pf_q.h>

struct pfq_global_t
{
    /* devmap */
    volatile unsigned long devmap  [Q_MAX_DEVICE][Q_MAX_HW_QUEUE];
    volatile uint8_t devmap_monitor[Q_MAX_DEVICE];

    /* timestamp */
    atomic_t   tstamp;

};

extern struct pfq_global_t global;
extern struct semaphore    global_sem;

#endif /* _PF_Q_GLOBAL_H_ */
