/***************************************************************

 * (C) 2011 - Nicola Bonelli <bonelli@cnit.it>   
 *            Andrea Di Pietro <andrea.dipietro@for.unipi.it>

 ***************************************************************/

#ifndef _PF_Q_H_
#define _PF_Q_H_ 

#ifdef __KERNEL__

#ifdef __PFQ_MODULE__


#define Q_VERSION               "1.1"
#define Q_VERSION_NUM           0x010001

#define Q_MAX_CPU               16
#define Q_MAX_ID                64

#define Q_MAX_DEVICE            256
#define Q_MAX_DEVICE_MASK       (Q_MAX_DEVICE-1)
#define Q_MAX_HW_QUEUE          64
#define Q_MAX_HW_QUEUE_MASK     (Q_MAX_HW_QUEUE-1)


#else  /* __PFQ_MODULE__ */ 

#include <linux/skbuff.h>
#include <linux/netdevice.h>

extern const char * pfq_version(void);
extern int  pfq_direct_capture(const struct sk_buff *skb);
extern int  pfq_direct_receive(struct sk_buff *skb, int ifindex, int queue);
extern gro_result_t pfq_gro_receive(struct napi_struct *napi, struct sk_buff *skb);

#endif

#else  /* user space */

#include <sys/time.h>
#include <unistd.h>
#include <stdint.h>

#endif /* __KERNEL__ */

/* Common header */

#define PF_Q                 27     /* packet q domain: note it's the same as the old pf_ring */

struct pfq_hdr
{
    uint16_t    caplen;     /* length of frame captured */    
    uint16_t    len;        /* length of the packet (off wire) */

    uint16_t    mark:15,    /* for classification */
                commit:1;   /* packet filled */

    uint8_t     if_index;   /* 256 devices */    
    uint8_t     hw_queue;   /* 256 queues per device */
    union 
    {
        unsigned long long tv64;
        struct {
            uint32_t    sec;
            uint32_t    nsec;
        } tv;           /* note: struct timespec is badly defined for 64 bits arch. */
    } tstamp;

} __attribute__((packed));

/* 
    [pfq_queue_descr][ ... queue .... ][ ... queue ... ]
 */

#define DBMP_QUEUE_INDEX(x)  ((x)     & 0x8000000000000000ULL)
#define DBMP_QUEUE_SIZE(x)   ((x)     & 0x00000000ffffffffULL)
#define DBMP_QUEUE_LEN(x)    ((x>>32) & 0x000000007fffffffULL)

struct pfq_queue_descr
{
    volatile uint64_t data;             /* atomic_long_t data: index(1) | queue_len(31) | queue_size(32)  */
    volatile uint64_t valid_data;       /* atomic_long_t data: index(1) | queue_len(31) | queue_size(32)  */
    volatile int      disable;
    volatile int      poll_wait;
} __attribute__((aligned(8)));


/* set socket options */

#define SO_TOGGLE_QUEUE         100     /* enable = 1, disable = 0 */
#define SO_ADD_DEVICE           101
#define SO_ADD_WEAK_DEVICE      102
#define SO_REMOVE_DEVICE        103
#define SO_TSTAMP_TYPE          105
#define SO_LOAD_BALANCE         106

/* get socket options */
#define SO_GET_ID               120
#define SO_GET_OWNERS           121
#define SO_GET_STATUS           122     /* 1 = enabled, 0 = disabled */
#define SO_GET_BATCH            123
#define SO_GET_STATS            124
#define SO_GET_TSTAMP_TYPE      125
#define SO_GET_TOT_MEM          126
#define SO_GET_QUEUE_MEM        127


/* struct used for setsockopt */

#define Q_ANY_DEVICE         -1
#define Q_ANY_QUEUE          -1

#define Q_TSTAMP_OFF          0       /* default */
#define Q_TSTAMP_ON           1

struct pfq_dev_queue
{
    long int if_index;
    int hw_queue;
};

struct pfq_stats
{
    unsigned long int recv;   // received by the queue    
    unsigned long int lost;   // queue is full, packet lost...
    unsigned long int drop;   // by filter
};

#endif /* _PF_Q_H_ */
