/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#ifndef PF_Q_LINUX_H
#define PF_Q_LINUX_H

#ifdef __KERNEL__

#include <pragma/diagnostic_push>

#include <linux/types.h>
#include <linux/filter.h>
#include <linux/skbuff.h>

#include <pragma/diagnostic_pop>

#else  /* user space */

#define __user

#include <linux/filter.h>
#include <linux/types.h>
#include <linux/ioctl.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdint.h>
#include <stddef.h>


#define likely(x)	__builtin_expect((x),1)
#define unlikely(x)     __builtin_expect((x),0)
#define barrier()	asm volatile("" ::: "memory")

#endif /* __KERNEL__ */

#define ____pfq_cacheline_aligned		__attribute__((aligned(128)))

/* PFQ version as in Linux kernel */

#define PF_Q					27   /* pfq socket family */


#define	QIOCTX					_IOR('Q', 0, int) /* flush Tx */


#define PFQ_VERSION(a,b,c)			(((a) << 16) + ((b) << 8) + (c))
#define PFQ_MAJOR(a)				((a >> 16) & 0xff)
#define PFQ_MINOR(a)				((a >> 8) & 0xff)
#define PFQ_PATCHLEVEL(a)			(a & 0xff)

#define PFQ_VERSION_CODE			PFQ_VERSION(6,0,0)
#define PFQ_VERSION_STRING			"6.0.0"

#ifdef __x86_64__
typedef uint32_t pfq_qver_t;
#else
typedef uint8_t  pfq_qver_t;
#endif

#define PFQ_SHARED_QUEUE_VER_SIZE		(sizeof(pfq_qver_t))
#define PFQ_SHARED_QUEUE_LEN_SIZE		(sizeof(unsigned long) - PFQ_SHARED_QUEUE_VER_SIZE)

#define PFQ_SHARED_QUEUE_VER_MASK		((1UL << (PFQ_SHARED_QUEUE_VER_SIZE<<3))-1)
#define PFQ_SHARED_QUEUE_LEN_MASK		((1UL << (PFQ_SHARED_QUEUE_LEN_SIZE<<3))-1)

#define PFQ_SHARED_QUEUE_VER(shinfo)		((shinfo) >> (PFQ_SHARED_QUEUE_LEN_SIZE<<3))
#define PFQ_SHARED_QUEUE_LEN(shinfo)		((shinfo) & PFQ_SHARED_QUEUE_LEN_MASK)

#define PFQ_SHARED_QUEUE_SLOT_SIZE(x)		ALIGN(sizeof(struct pfq_pkthdr) + x, 64)

#define PFQ_SHARED_QUEUE_NEXT_PKTHDR(hdr, fix)	((struct pfq_pkthdr *)\
						 (fix ? \
						  (char *)(hdr) + fix :\
						  (char *)(hdr) + ALIGN(sizeof(struct pfq_pkthdr) + hdr->caplen, 64)))


#define PFQ_SHARED_QUEUE_NEXT_FIX_PKTHDR(hdr, fix) ((struct pfq_pkthdr *)((char *)(hdr) + fix))
#define PFQ_SHARED_QUEUE_NEXT_VAR_PKTHDR(hdr)	   ((struct pfq_pkthdr *)((char *)(hdr) + ALIGN(sizeof(struct pfq_pkthdr) + hdr->caplen, 64)))



/* PFQ socket options */

#define Q_SO_DISABLE			0
#define Q_SO_ENABLE			1

#define Q_SO_SET_RX_TSTAMP		2
#define Q_SO_SET_RX_CAPLEN		3
#define Q_SO_SET_RX_SLOTS		4
#define Q_SO_SET_RX_OFFSET		5
#define Q_SO_SET_TX_SLOTS		7
#define Q_SO_SET_WEIGHT			8

#define Q_SO_GROUP_BIND			10
#define Q_SO_GROUP_UNBIND		11
#define Q_SO_GROUP_JOIN			12
#define Q_SO_GROUP_LEAVE		13
#define Q_SO_GROUP_FPROG		14      /* Berkeley packet filter */
#define Q_SO_GROUP_VLAN_FILT_TOGGLE	15      /* enable/disable VLAN filters */
#define Q_SO_GROUP_VLAN_FILT		16      /* enable/disable VLAN ID filters */
#define Q_SO_GROUP_FUNCTION		17

#define Q_SO_EGRESS_BIND		18
#define Q_SO_EGRESS_UNBIND		19

#define Q_SO_GET_ID			20
#define Q_SO_GET_STATUS			21      /* 1 = enabled, 0 = disabled */
#define Q_SO_GET_STATS			22
#define Q_SO_GET_SHMEM_SIZE		23      /* size of the shared memory in (bytes) */
#define Q_SO_GET_RX_TSTAMP		24
#define Q_SO_GET_RX_CAPLEN		25
#define Q_SO_GET_RX_SLOTS		26
#define Q_SO_GET_RX_OFFSET		27
#define Q_SO_GET_TX_MAXLEN		28
#define Q_SO_GET_TX_SLOTS		29
#define Q_SO_GET_GROUPS			30
#define Q_SO_GET_GROUP_STATS		31
#define Q_SO_GET_GROUP_COUNTERS		32
#define Q_SO_GET_WEIGHT			33

#define Q_SO_TX_BIND			40
#define Q_SO_TX_UNBIND			41
#define Q_SO_TX_QUEUE_XMIT	        42

/* general placeholders */

#define Q_ANY_DEVICE			-1
#define Q_ANY_QUEUE			-1
#define Q_ANY_GROUP			-1
#define Q_NO_KTHREAD			-1

/*timestamp*/

#define Q_TSTAMP_OFF			0	/*default*/
#define Q_TSTAMP_ON			1


/*vlan*/

#define Q_VLAN_PRIO_MASK		0xe000
#define Q_VLAN_VID_MASK			0x0fff
#define Q_VLAN_TAG_PRESENT		0x1000

#define Q_VLAN_UNTAG			0
#define Q_VLAN_ANYTAG			-1

/*group policies*/

#define Q_POLICY_GROUP_UNDEFINED	0
#define Q_POLICY_GROUP_PRIVATE		1
#define Q_POLICY_GROUP_RESTRICTED	2
#define Q_POLICY_GROUP_SHARED		3

/* group class type */

#define Q_CLASS(n)			(1UL<<(n))
#define Q_CLASS_MAX			(sizeof(unsigned long)<<3)

#define Q_CLASS_DEFAULT			Q_CLASS(0)
#define Q_CLASS_USER_PLANE		Q_CLASS(1)
#define Q_CLASS_CONTROL_PLANE		Q_CLASS(2)
#define Q_CLASS_CONTROL			Q_CLASS(Q_CLASS_MAX-1)			/*reserved for management*/
#define Q_CLASS_ANY			(((unsigned long)-1)^Q_CLASS_CONTROL)	/*any class except management*/


/*additional constants*/

#define Q_MAX_COUNTERS			64
#define Q_MAX_TX_QUEUES			4
#define Q_MAX_RX_NAPI			4


/* PFQ socket queue */


struct pfq_rx_queue
{
        unsigned long		shinfo;	    /* atomic */
        unsigned int            len;        /* queue length in slots */
        unsigned int            size;       /* queue size in bytes */
        unsigned int            slot_size;  /* sizeof(pfq_pkthdr) + caplen  */

} ____pfq_cacheline_aligned;



struct pfq_tx_queue
{
        size_t				size;	    /* queue size in bytes */

	struct
	{
		unsigned int		index;
		ptrdiff_t		off0;
		ptrdiff_t	        off1;

	} prod  ____pfq_cacheline_aligned;

	struct
	{
		unsigned int		index;
		ptrdiff_t		off;

	} cons ____pfq_cacheline_aligned;

} ____pfq_cacheline_aligned;


struct pfq_shared_queue
{
        struct pfq_rx_queue rx;
        struct pfq_tx_queue tx;
        struct pfq_tx_queue tx_async[Q_MAX_TX_QUEUES];
};


/* packet headers */


struct pfq_pkthdr_info
{
        int         ifindex;			/* interface index */

	union
	{
		struct
		{
			uint32_t mark;		/* packet mark */
			uint32_t state;         /* monad state */
		};

		struct
		{
			unsigned int copies;	/* for packet Tx */
			uint32_t reserved;	/* reserved */
		};
	} data;

        union
        {
                struct
                {
                        uint16_t vid:12,	/* 8021q vlan id */
                                 reserved:1,    /* 8021q reserved bit */
                                 prio:3;	/* 8021q vlan priority */
                };

                uint16_t     tci;
        } vlan;

        uint8_t      gid;			/* group id */
        uint8_t      queue;			/* hardware queue */
        uint32_t     commit;                    /* commit round */
};


struct pfq_pkthdr
{
        union
        {
                uint64_t	    tv64;
                struct {
                        uint32_t    sec;
                        uint32_t    nsec;
                } tv;				/* note: compact timespec */

        } tstamp;

        uint16_t    caplen;			/* bytes captured */
        uint16_t    len;			/* length of the packet (off wire) */

	struct pfq_pkthdr_info info;		/* additional information */
};



/*
   +------------------+---------------------+                  +---------------------+          +---------------------+
   | pfq_queue_hdr    | pfq_pkthdr | packet | ...              | pfq_pkthdr | packet |...       | pfq_pkthdr | packet | ...
   +------------------+---------------------+                  +---------------------+          +---------------------+
   +                             +                             +                            +
   | <------+ queue rx  +------> |  <----+ queue rx +------>   |  <----+ queue tx +------>  |  <----+ queue tx +------>
   +                             +                             +                            +
   */


struct pfq_pcap_pkthdr {

    struct timeval ts;			/* time stamp */
    uint32_t caplen;			/* length of portion present */
    uint32_t len;			/* length this packet (off wire) */

    struct pfq_pkthdr_info info;        /* extended pcap packet header */
};


/*
 * Functional argument:
 *
 * pod	  	-> (ptr/value, sizeof,  -1)
 * pod array    -> (ptr,       sizeof,  len)
 * string 	-> (ptr,       0     ,  -1)
 * string array -> (ptr,       sizeof,  len)
 * expression 	-> (0,         index ,  -1)
 *
 */

struct pfq_lang_functional_arg_descr
{
	const void __user *	addr;
	size_t			size;
	ptrdiff_t		nelem;
};


/*
 * Functional descriptor:
 */

struct pfq_lang_functional_descr
{
        const char __user *			symbol;
	struct pfq_lang_functional_arg_descr	arg[8];
        ptrdiff_t				next;
};


struct pfq_lang_computation_descr
{
        size_t					size;
        size_t					entry_point;
        struct pfq_lang_functional_descr	fun[];
};


/*
 *  PFQ sock options helper structures
 */


struct pfq_so_enable
{
	unsigned long	user_addr;
	size_t		user_size;
	size_t		hugepage_size;
};


struct pfq_so_vlan_toggle
{
        int gid;
        int vid;
        int toggle;
};

struct pfq_so_binding
{
        union
        {
		int gid;
		int tid;
	};

        int ifindex;
        int qindex;
};

struct pfq_so_group_join
{
        int gid;
        int policy;
        unsigned long class_mask;
};

struct pfq_so_group_computation
{
        int gid;
        struct pfq_lang_computation_descr const __user *prog;
};


struct pfq_so_group_context
{
        void __user *context;
        size_t       size;      /* sizeof(context) */
        int gid;
        int level;
};


/* pfq_fprog: per-group sock_fprog */

struct pfq_so_fprog
{
        int gid;
        struct sock_fprog fcode;
};


/* pfq statistics for socket and groups */

struct pfq_stats
{
        unsigned long int recv;		/* received by the group */
        unsigned long int lost;		/* queue is full, packet lost... */
        unsigned long int drop;		/* by filter                     */

        unsigned long int sent;		/* sent by the driver */
        unsigned long int disc;		/* discarded by the driver */
        unsigned long int fail;		/* tx fail due to driver congestion */

	unsigned long int frwd;		/* forwarded to devices */
	unsigned long int kern;		/* forwarded to kernel  */
};


/* pfq counters for groups */

struct pfq_counters
{
        unsigned long int counter[Q_MAX_COUNTERS];
};

#endif /* PF_Q_LINUX_H */
