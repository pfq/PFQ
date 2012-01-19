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

extern void *
mpdb_queue_alloc(struct pfq_opt *pq, int queue_mem, size_t * tot_mem);

extern void
mpdb_queue_free(struct pfq_opt *pq);


static inline size_t
mpdb_queue_len(struct pfq_opt *p)
{
    struct pfq_queue_descr *qd = (struct pfq_queue_descr *)p->q_addr;
    return DBMP_QUEUE_LEN(qd->data);
}


static inline int
mpdb_queue_index(struct pfq_opt *p)
{
    struct pfq_queue_descr *qd = (struct pfq_queue_descr *)p->q_addr;
    return DBMP_QUEUE_INDEX(qd->data) ? 1 : 0;
}


static inline
size_t
mpdb_queue_size(struct pfq_opt *pq)
{
        return sizeof(struct pfq_queue_descr) + pq->q_slot_size * pq->q_slots * 2; 
}

#endif /* _MPDB_QUEUE_H_ */
