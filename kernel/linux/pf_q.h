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

#ifndef _PF_Q_H_
#define _PF_Q_H_ 

#ifdef __KERNEL__

#include <linux/types.h>

#define Q_VERSION               "2.0"

#define Q_MAX_CPU               64
#define Q_MAX_ID                64
#define Q_MAX_GROUP             64

#define Q_MAX_DEVICE            256
#define Q_MAX_DEVICE_MASK       (Q_MAX_DEVICE-1)
#define Q_MAX_HW_QUEUE          256
#define Q_MAX_HW_QUEUE_MASK     (Q_MAX_HW_QUEUE-1)

#else  /* user space */

#include <sys/time.h>
#include <unistd.h>
#include <stdint.h>

#endif /* __KERNEL__ */

/* Common header */

#define PF_Q    27          /* packet q domain: note it's the same as the old pf_ring */


struct pfq_hdr
{
    union 
    {
        unsigned long long tv64;
        struct {
            uint32_t    sec;
            uint32_t    nsec;
        } tv;               /* note: struct timespec is badly defined for 64 bits arch. */
    } tstamp;
    
    uint16_t    len;        /* length of the packet (off wire) */
    uint16_t    caplen;     /* bytes captured */

    union
    {
        struct 
        {
            uint16_t vlan_vid:12,   /* 8021q vlan id */
                     reserved:1,    /* 8021q reserved bit */
                     vlan_prio:3;   /* 8021q vlan priority */   
        } vlan;

        uint16_t     vlan_tci;
    } un;

    int         gid;        /* gruop id */
    int         if_index;   /* interface index */    
    
    uint8_t     hw_queue;   /* 256 queues per device */
    uint8_t     ready;

} /* __attribute__((packed)) */;


/* 
    [pfq_queue_descr][ ... queue .... ][ ... queue ... ]
 */

struct pfq_queue_descr
{
    volatile unsigned int   data;
    volatile int   poll_wait;

} __attribute__((aligned(8)));


#define DBMP_QUEUE_SLOT_SIZE(x)    ALIGN(sizeof(struct pfq_hdr) + x, 8)
#define DBMP_QUEUE_INDEX(data)     (((data) & 0xff000000U) >> 24)
#define DBMP_QUEUE_LEN(data)       ((data) & 0x00ffffffU)


/* set socket options */

#define SO_TOGGLE_QUEUE         100     /* enable = 1, disable = 0 */
#define SO_ADD_BINDING          101
#define SO_REMOVE_BINDING       102
#define SO_TSTAMP_TOGGLE        103
#define SO_GROUP_STEER          104
#define SO_CAPLEN               105
#define SO_SLOTS                106
#define SO_OFFSET               107
#define SO_GROUP_LEAVE          108
#define SO_GROUP_STATE          109

/* get socket options */

#define SO_GET_ID               120
#define SO_GET_GROUPS           121
#define SO_GET_STATUS           122     /* 1 = enabled, 0 = disabled */
#define SO_GET_STATS            123
#define SO_GET_TSTAMP           124
#define SO_GET_QUEUE_MEM        125     /* size of the whole dbmp queue (bytes) */
#define SO_GET_CAPLEN           126
#define SO_GET_SLOTS            127
#define SO_GET_OFFSET           128
#define SO_GROUP_JOIN           129
#define SO_GROUP_STATS          130

/* general defines */

#define Q_ANY_DEVICE         -1
#define Q_ANY_QUEUE          -1
#define Q_ANY_GROUP          -1

/* timestamp */

#define Q_TSTAMP_OFF          0       /* default */
#define Q_TSTAMP_ON           1

/* vlan */

#define Q_VLAN_PRIO_MASK     0xe000
#define Q_VLAN_VID_MASK      0x0fff
#define Q_VLAN_TAG_PRESENT   0x1000

/* struct used for setsockopt */

struct pfq_binding
{
    int gid;
    int if_index;
    int hw_queue;
};


/* group policies */

#define Q_GROUP_RESTRICTED      0
#define Q_GROUP_SHARED          1
#define Q_GROUP_UNDEFINED       2

/* class type */

#define Q_CLASS_MAX             16 
#define Q_CLASS_DEFAULT         (1<<0)
#define Q_CLASS_ANY             Q_CLASS_MAX-1

struct pfq_group_join
{
    int gid;
    int policy;
    unsigned long class_mask;
};

/* steering functions */

#define Q_STEERING_NAME_LEN        64

struct pfq_steering
{
    const char *name;
    int gid;
};


struct pfq_group_state
{
    const void * state;
    size_t       size;      // sizeof(state)
    int gid;
};


/* pfq statistics for socket and groups */

struct pfq_stats
{
    unsigned long int recv;   /* received by the queue         */
    unsigned long int lost;   /* queue is full, packet lost... */
    unsigned long int drop;   /* by filter                     */
};


#endif /* _PF_Q_H_ */
