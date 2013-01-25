/***************************************************************
   
   Copyright (c) 2011-2013, Nicola Bonelli 
   All rights reserved. 

   Redistribution and use in source and binary forms, with or without 
   modification, are permitted provided that the following conditions are met: 

   * Redistributions of source code must retain the above copyright notice, 
     this list of conditions and the following disclaimer. 
   * Redistributions in binary form must reproduce the above copyright 
     notice, this list of conditions and the following disclaimer in the 
     documentation and/or other materials provided with the distribution. 
   * Neither the name of University of Pisa nor the names of its contributors 
     may be used to endorse or promote products derived from this software 
     without specific prior written permission. 

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
   LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
   POSSIBILITY OF SUCH DAMAGE.
 
 ***************************************************************/

#ifndef _PFQ_H_
#define _PFQ_H_ 

#include <stddef.h>
#include <linux/pf_q.h>

#ifdef _REENTRANT
#include <pthread.h>
#else
#include <sched.h>
#endif

#ifndef PFQ_LIBRARY
typedef void *pfq_t;   /* pfq descritor */
#endif


/* memory barriers */

#if defined(__GNUC__)
        static inline void barrier() { asm volatile ("" ::: "memory"); }
#else
#error "Compiler not supported"
#endif

#if defined(__i386__) && !defined(__LP64__)
#error "32-bit architecture is not supported"
#endif

#if defined(__LP64__)
        static inline void mb()  { asm volatile ("mfence" ::: "memory"); }
        static inline void rmb() { asm volatile ("lfence" ::: "memory"); }
        static inline void wmb() { asm volatile ("sfence" ::: "memory"); }
#endif

#ifdef CONFIG_SMP
        static inline void smp_mb()  { mb(); }
        static inline void smp_rmb() { barrier(); }
        static inline void smp_wmb() { barrier(); }
#else
        static inline void smp_mb()  { barrier(); }
        static inline void smp_rmb() { barrier(); }
        static inline void smp_wmb() { barrier(); }
#endif


#ifndef PFQ_LIBRARY
/* pfq_net_queue */

typedef char * pfq_iterator_t;

struct pfq_net_queue
{	
	pfq_iterator_t queue; 	  		/* net queue */
	size_t         len;       		/* number of packets in the queue */
    size_t         slot_size;
	unsigned int   index; 	  		/* current queue index */ 
};
#endif


static inline 
pfq_iterator_t
pfq_net_queue_begin(struct pfq_net_queue const *nq)
{
    return nq->queue;
}


static inline 
pfq_iterator_t
pfq_net_queue_end(struct pfq_net_queue const *nq)
{
    return nq->queue + nq->len * nq->slot_size;
}


static inline 
pfq_iterator_t
pfq_net_queue_next(struct pfq_net_queue const *nq, pfq_iterator_t iter)
{
    return iter + nq->slot_size;
}


static inline 
pfq_iterator_t
pfq_net_queue_prev(struct pfq_net_queue const *nq, pfq_iterator_t iter)
{
    return iter - nq->slot_size;
}


static inline
const struct pfq_hdr *
pfq_iterator_header(pfq_iterator_t iter)
{
    return (const struct pfq_hdr *)iter;
}


static inline
const char *
pfq_iterator_data(pfq_iterator_t iter)
{
    return (const char *)(iter + sizeof(struct pfq_hdr));
}


static inline
int
pfq_iterator_ready(struct pfq_net_queue const *nq, pfq_iterator_t iter)
{
	if (pfq_iterator_header(iter)->commit != nq->index) {
        return 0;
    }
	smp_rmb();
	return 1;
}


static inline
int
pfq_yield()
{
    return 
#ifdef _REENTRANT
    pthread_yield();
#else
    sched_yield();
#endif
}

/* pfq handler */

typedef void (*pfq_handler_t)(char *user, const struct pfq_hdr *h, const char *data); 


/* ************************************** *
 *          library functions             *
 * ************************************** */


/*
 * group_policy: Q_GROUP_RESTRICTED, Q_GROUP_SHARED, Q_GROUP_UNDEFINED...
   class_mask  : Q_CLASS_DEFAULT| .... = Q_CLASS_ANY
 */


extern pfq_t* pfq_open(size_t calpen, size_t offset, size_t slots);

extern pfq_t * pfq_open_nogroup(size_t caplen, size_t offset, size_t slots);

extern pfq_t* pfq_open_group(unsigned int class_mask, 
							 int group_policy, 
							 size_t calpen, size_t offset, size_t slots);

extern int pfq_close(pfq_t *);

extern const char *pfq_error(pfq_t *);

extern int pfq_ifindex(pfq_t const *q, const char *dev);
extern int pfq_set_promisc(pfq_t const *q, const char *dev, int value);

extern int pfq_enable(pfq_t *q);
extern int pfq_disable(pfq_t *q);

extern int pfq_is_enabled(pfq_t const *q);

extern int pfq_timestamp_enabled(pfq_t *q, int value);

extern int pfq_is_timestamp_enabled(pfq_t const *q);

extern int pfq_ifindex(pfq_t const *q, const char *dev);

extern int pfq_set_caplen(pfq_t *q, size_t value);

extern ssize_t pfq_get_caplen(pfq_t const *q);

extern int pfq_set_offset(pfq_t *q, size_t value);

extern ssize_t pfq_get_offset(pfq_t const *q);

extern int pfq_set_slots(pfq_t *q, size_t value);

extern size_t pfq_get_slots(pfq_t const *q);

extern size_t pfq_get_slot_size(pfq_t const *q); 

extern int pfq_bind_group(pfq_t *q, int gid, const char *dev, int queue);

extern int pfq_bind(pfq_t *q, const char *dev, int queue);

extern int pfq_unbind_group(pfq_t *q, int gid, const char *dev, int queue); /* Q_ANY_QUEUE */

extern int pfq_unbind(pfq_t *q, const char *dev, int queue);

extern int pfq_groups_mask(pfq_t const *q, unsigned long *_mask);

extern int pfq_group_function(pfq_t *q, int gid, const char *fun_name);

extern int pfq_group_state(pfq_t *q, int gid, const void *state, size_t size);

extern int pfq_group_fprog(pfq_t *q, int gid, struct sock_fprog *);

extern int pfq_group_fprog_reset(pfq_t *q, int gid);

extern int pfq_vlan_filters_enabled(pfq_t *q, int gid, int toggle);

extern int pfq_vlan_set_filter(pfq_t *q, int gid, int vid);

extern int pfq_vlan_reset_filter(pfq_t *q, int gid, int vid);

extern int pfq_join_group(pfq_t *q, int gid, unsigned int class_mask, int group_policy);

extern int pfq_leave_group(pfq_t *q, int gid);
        
extern int pfq_poll(pfq_t *q, long int microseconds /* = -1 -> infinite */);

extern int pfq_read(pfq_t *q, struct pfq_net_queue *nq, long int microseconds); 

extern int pfq_recv(pfq_t *q, void *buf, size_t buflen, 
					struct pfq_net_queue *nq, long int microseconds);

extern int pfq_dispatch(pfq_t *q, pfq_handler_t cb, long int microseconds, char *user);

extern size_t pfq_mem_size(pfq_t const *q);

extern const void * pfq_mem_addr(pfq_t const *q); 

extern int pfq_id(pfq_t *q);

extern int pfq_group_id(pfq_t *q);
 
extern int pfq_get_fd(pfq_t *q);

extern int pfq_get_stats(pfq_t const *q, struct pfq_stats *stats); 

extern int pfq_get_group_stats(pfq_t const *q, int gid, struct pfq_stats *stats); 


#endif /* _PFQ_H_ */
