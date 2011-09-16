/***************************************************************
 *                                                
 * (C) 2011 - Nicola Bonelli <nicola.bonelli@cnit.it>   
 *            Andrea Di Pietro <andrea.dipietro@for.unipi.it>
 *
 ****************************************************************/

#ifndef _MPDB_QUEUE_H_
#define _MPDB_QUEUE_H_ 

#include <linux/skbuff.h>
#include <linux/pf_q.h>
#include <pf_q-priv.h>

extern bool 
mpdb_enqueue(struct pfq_opt *pq, struct sk_buff *skb);

static inline size_t
mpdb_queue_len(struct pfq_opt *p)
{
    struct pfq_queue_descr *qd = (struct pfq_queue_descr *)p->q_mem;
    return DBMP_QUEUE_LEN(qd->data);
}


static inline size_t
mpdb_queue_size(struct pfq_opt *p)
{
    struct pfq_queue_descr *qd = (struct pfq_queue_descr *)p->q_mem;
    return DBMP_QUEUE_SIZE(qd->data);
}

#endif /* _MPDB_QUEUE_H_ */
