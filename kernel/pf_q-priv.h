/***************************************************************
 *                                                
 * (C) 2011 - Nicola Bonelli <nicola.bonelli@cnit.it>   
 *            Andrea Di Pietro <andrea.dipietro@for.unipi.it>
 *
 ****************************************************************/

#ifndef _PF_Q_TYPES_H_
#define _PF_Q_TYPES_H_ 

#include <linux/kernel.h>
#include <linux/poll.h>

#include <net/sock.h>

#include <mpsc-skbuff.h>
#include <sparse-counter.h>

/* sparse_counter_t stats */

typedef struct pfq_kstats
{
    sparse_counter_t  recv;    // received by the queue    
    sparse_counter_t  lost;    // queue is full, packet is lost
    sparse_counter_t  drop;    // filter

} pfq_kstat_t;


struct pfq_opt
{
        bool            q_active;
        unsigned int    q_id;

        int             q_tstamp_type;
        void *          q_mem;
        
        size_t          q_tot_mem;    /* pfq_queue_descr + q_queue_mem * 2 */
        size_t          q_queue_mem; 
        size_t          q_cap_len;

        wait_queue_head_t q_waitqueue;

        pfq_kstat_t     q_stat;

} __attribute__((aligned(64)));


struct pfq_sock
{
        struct sock sk;
        struct pfq_opt *opt;    
};

#define PFQ_PIPELINE_MAX_LEN  1024

struct pfq_pipeline
{
    struct sk_buff *queue[PFQ_PIPELINE_MAX_LEN];  /* sk_buff */
    size_t counter;

} __attribute__((aligned(64)));

#endif /* _PF_Q_TYPES_H_ */
