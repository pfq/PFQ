/***************************************************************

  Copyright (c) 2012, Nicola Bonelli 
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

#include <linux/if_ether.h>
#include <linux/pf_q.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mman.h>

#include <net/if.h>
#include <net/ethernet.h>
#include <arpa/inet.h>

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <poll.h>

/* pfq descriptor */

typedef struct 
{
	int fd;
	int id;
	int gid;

	void * queue_addr;

	size_t queue_tot_mem;
	size_t queue_slots; 
	size_t queue_caplen;
	size_t queue_offset;
	size_t slot_size;

	const char * error;

} pfq_t;


#define  ALIGN8(value) ((value + 7) & ~7)

#define max(a,b) \
	({ __typeof__ (a) _a = (a); \
	   __typeof__ (b) _b = (b); \
	  _a > _b ? _a : _b; })

#define min(a,b) \
	({ __typeof__ (a) _a = (a); \
	   __typeof__ (b) _b = (b); \
	  _a < _b ? _a : _b; })


#define PFQ_LIBRARY
#include <pfq.h>

/* return the string error */

static __thread const char * __error;

const char *pfq_error(pfq_t *q)
{
	return q == NULL ? __error : q->error;
}


/* costructor */


pfq_t *
pfq_open(size_t caplen, size_t offset, size_t slots)        
{
	return pfq_open_group(Q_GROUP_DATA, Q_GROUP_RESTRICTED, caplen, offset, slots); 
}

pfq_t *
pfq_open_group(int group_type, int group_policy, size_t caplen, size_t offset, size_t slots)
{
	int fd = socket(PF_Q, SOCK_RAW, htons(ETH_P_ALL));
	pfq_t * q;

	if (fd == -1) {
		return __error = "PFQ: module not loaded", NULL;
	}

	q = (pfq_t *) malloc(sizeof(pfq_t));
	if (q == NULL) {
		return __error = "PFQ: out of memory", NULL;
	}

	q->fd 	= fd;  
	q->id 	= -1;
	q->gid 	= -1;

	q->queue_addr 	 = NULL;
	q->queue_tot_mem = 0;
	q->queue_slots   = 0;
	q->queue_caplen  = 0;
	q->queue_offset  = offset;
	q->slot_size     = 0;
	q->error 	 = NULL;

	/* get id */
	socklen_t size = sizeof(q->id);
	if (getsockopt(fd, PF_Q, SO_GET_ID, &q->id, &size) == -1) {
		return __error = "PFQ: SO_GET_ID", free(q), NULL;
	}

	/* set queue slots */
	if (setsockopt(fd, PF_Q, SO_SLOTS, &slots, sizeof(slots)) == -1) {
		return __error = "PFQ: SO_SLOTS", free(q), NULL;
	}

	q->queue_slots = slots;

	/* set caplen */
	if (setsockopt(fd, PF_Q, SO_CAPLEN, &caplen, sizeof(caplen)) == -1) {
		return __error = "PFQ: SO_CAPLEN", free(q), NULL;
	}

	q->queue_caplen = caplen;

	/* set offset */
	if (setsockopt(fd, PF_Q, SO_OFFSET, &offset, sizeof(offset)) == -1) {
		return __error = "PFQ: SO_OFFSET", free(q), NULL;
	}

	q->slot_size = ALIGN8(sizeof(struct pfq_hdr) + q->queue_caplen);
	
	if (group_policy != Q_GROUP_UNDEFINED)
	{
		q->gid = pfq_join_group(q, Q_ANY_GROUP, group_type, group_policy);
		if (q->gid == -1) {
			return __error = q->error, free(q), NULL;
		}
	}
	
	return __error = NULL, q;
}


int pfq_close(pfq_t *q)
{
	if (q->fd != -1) {
		if (q->queue_addr)
			return pfq_disable(q);
		if (close(q->fd) < 0)
			return q->error = "PFQ: close error", -1;
	}
	return q->error = "PFQ: socket not open", -1;
}


int
pfq_enable(pfq_t *q)
{
	int one = 1;

	if(setsockopt(q->fd, PF_Q, SO_TOGGLE_QUEUE, &one, sizeof(one)) == -1) {
		return q->error = "PFQ: SO_TOGGLE_QUEUE", -1;
	}

	size_t tot_mem; socklen_t size = sizeof(tot_mem);

	if (getsockopt(q->fd, PF_Q, SO_GET_QUEUE_MEM, &tot_mem, &size) == -1) {
		return q->error = "PFQ: SO_GET_QUEUE_MEM", -1;
	}

	q->queue_tot_mem = tot_mem;

	if ((q->queue_addr = mmap(NULL, tot_mem, PROT_READ|PROT_WRITE, MAP_SHARED, q->fd, 0)) == MAP_FAILED) { 
		return q->error = "PFQ: mmap error", -1;
	}
        return q->error = NULL, 0;
}


int 
pfq_disable(pfq_t *q)
{
	if (munmap(q->queue_addr,q->queue_tot_mem) == -1) {
		return q->error = "PFQ: munmap", -1;
	}

	q->queue_addr = NULL;
	q->queue_tot_mem = 0;

	int one = 0;
	if(setsockopt(q->fd, PF_Q, SO_TOGGLE_QUEUE, &one, sizeof(one)) == -1) {
		return q->error = "PFQ: SO_TOGGLE_QUEUE", -1;
	}
	return q->error = NULL, 0;
}


int
pfq_is_enabled(pfq_t const *q)
{
	pfq_t * mutable = (pfq_t *)q;
	if (q->fd != -1)
	{
		int ret; socklen_t size = sizeof(ret);
		if (getsockopt(q->fd, PF_Q, SO_GET_STATUS, &ret, &size) == -1) {
			return mutable->error = "PFQ: SO_GET_STATUS", -1;
		}
		return mutable->error = NULL, ret;
	}
	return mutable->error = NULL, 0;
}


int
pfq_toggle_time_stamp(pfq_t *q, int value)
{
	int ts = value;
	if (setsockopt(q->fd, PF_Q, SO_TSTAMP_TYPE, &ts, sizeof(ts)) == -1) {
		return q->error = "PFQ: SO_TSTAMP_TYPE", -1;
	}
	return q->error = NULL, 0;
}


int
pfq_time_stamp(pfq_t const *q)
{
	pfq_t * mutable = (pfq_t *)q;
	int ret; socklen_t size = sizeof(int);
	
	if (getsockopt(q->fd, PF_Q, SO_GET_TSTAMP_TYPE, &ret, &size) == -1) {
	        return mutable->error = "PFQ: SO_GET_TSTAMP_TYPE", -1;
	}
	return mutable->error = NULL, ret;
}


int
pfq_ifindex(pfq_t const *q, const char *dev)
{
	struct ifreq ifreq_io;
	pfq_t * mutable = (pfq_t *)q;
	
	memset(&ifreq_io, 0, sizeof(struct ifreq));
	strncpy(ifreq_io.ifr_name, dev, IFNAMSIZ);
	if (ioctl(q->fd, SIOCGIFINDEX, &ifreq_io) == -1) {
		return mutable->error = "PFQ: SIOCGIFINDEX", -1;
	}
	return mutable->error = NULL, ifreq_io.ifr_ifindex;
}


int 
pfq_set_caplen(pfq_t *q, size_t value)
{
	int enabled = pfq_is_enabled(q);
	if (enabled == 1) {
		return q->error =  "PFQ: enabled (caplen could not be set)", -1;
	}

	if (setsockopt(q->fd, PF_Q, SO_CAPLEN, &value, sizeof(value)) == -1) {
		return q->error = "PFQ: SO_CAPLEN", -1;
	}

	q->slot_size = ALIGN8(sizeof(struct pfq_hdr)+ value);
	return q->error = NULL, 0;
}


int
pfq_get_caplen(pfq_t const *q)
{
	size_t ret; socklen_t size = sizeof(ret);
	pfq_t * mutable = (pfq_t *)q;
	
	if (getsockopt(q->fd, PF_Q, SO_GET_CAPLEN, &ret, &size) == -1) {
		return mutable->error = "PFQ: SO_GET_CAPLEN", -1;
	}
	return mutable->error = NULL, ret;
}


int
pfq_set_offset(pfq_t *q, size_t value)
{
	int enabled = pfq_is_enabled(q);
	if (enabled == 1) {
		return q->error =  "PFQ: enabled (offset could not be set)", -1;
	}

	if (setsockopt(q->fd, PF_Q, SO_OFFSET, &value, sizeof(value)) == -1) {
		return q->error = "PFQ: SO_OFFSET", -1;
	}
	return q->error = NULL, 0;
}


int
pfq_get_offset(pfq_t const *q)
{
	pfq_t * mutable = (pfq_t *)q;
	size_t ret; socklen_t size = sizeof(ret);
	
	if (getsockopt(q->fd, PF_Q, SO_GET_OFFSET, &ret, &size) == -1) {
		return mutable->error = "PFQ: SO_GET_OFFSET", -1;
	}
	return mutable->error = NULL, ret;
}


int
pfq_set_slots(pfq_t *q, size_t value) 
{             
	int enabled = pfq_is_enabled(q);
	if (enabled == 1) {
		return q->error =  "PFQ: enabled (slots could not be set)", -1;
	}
	if (setsockopt(q->fd, PF_Q, SO_SLOTS, &value, sizeof(value)) == -1) {
		return q->error = "PFQ: SO_SLOTS", -1;
	}

	q->queue_slots = value;
	return q->error = NULL, 0;
}

int
pfq_get_slots(pfq_t const *q) 
{   
	return q->queue_slots;
}


int
pfq_get_slot_size(pfq_t const *q) 
{   
	return q->slot_size;
}


int
pfq_bind_group(pfq_t *q, int gid, const char *dev, int queue)
{
	int index = pfq_ifindex(q, dev);
	if (index == -1) {
		return q->error = "PFQ: device not found", -1;
	}

	struct pfq_binding b = { gid, index, queue };
	if (setsockopt(q->fd, PF_Q, SO_ADD_BINDING, &b, sizeof(b)) == -1) {
		return q->error = "PFQ: SO_ADD_BINDING", -1;
	}
	return q->error = NULL, 0;
}

int 
pfq_bind(pfq_t *q, const char *dev, int queue)
{
	int gid = q->gid;
	if (gid < 0) {
		return q->error = "PFQ: default group undefined", -1;
	}
	return pfq_bind_group(q, q->gid, dev, queue);
}                              


int
pfq_unbind_group(pfq_t *q, int gid, const char *dev, int queue) /* Q_ANY_QUEUE */
{
	int index = pfq_ifindex(q, dev);
	if (index == -1) {
		return q->error = "PFQ: device not found", -1;
	}
	struct pfq_binding b = { gid, index, queue };
	if (setsockopt(q->fd, PF_Q, SO_REMOVE_BINDING, &b, sizeof(b)) == -1) {
		return q->error = "PFQ: SO_REMOVE_BINDING", -1;
	}
	return q->error = NULL, 0;
}


int
pfq_unbind(pfq_t *q, const char *dev, int queue)
{
	int gid = q->gid;
	if (gid < 0) {
		return q->error = "PFQ: default group undefined", -1;
	}
	return pfq_unbind_group(q, gid, dev, queue);
}  


int 
pfq_groups_mask(pfq_t const *q, unsigned long *_mask) 
{
	unsigned long mask; socklen_t size = sizeof(mask);
	pfq_t * mutable = (pfq_t *)q;
	
	if (getsockopt(q->fd, PF_Q, SO_GET_GROUPS, &mask, &size) == -1) {
		return mutable->error = "PFQ: SO_GET_GROUPS", -1;
	}
	*_mask = mask;
	return mutable->error = NULL, 0;
}


int
pfq_steer_function(pfq_t *q, int gid, const char *fun_name)
{
	struct pfq_steer s = { gid, fun_name };
	if (setsockopt(q->fd, PF_Q, SO_GROUP_STEER, &s, sizeof(s)) == -1) {
		return q->error = "PFQ: SO_GROUP_STEER", -1;
	}
	return q->error = NULL, 0;
}


int
pfq_join_group(pfq_t *q, int gid, short int group_type, short int group_policy)
{
	if (group_policy == Q_GROUP_UNDEFINED) {
         	return q->error = "PFQ: join with undefined policy!", -1;
	}

	struct pfq_group_join group = { gid, group_type, group_policy };

	socklen_t size = sizeof(group);
	if (getsockopt(q->fd, PF_Q, SO_GROUP_JOIN, &group, &size) == -1) {
	        return q->error = "PFQ: SO_GROUP_JOIN", -1;
	}
	return q->error = NULL, group.gid;
}


int
leave_group(pfq_t *q, int gid)
{
	if (setsockopt(q->fd, PF_Q, SO_GROUP_LEAVE, &gid, sizeof(gid)) == -1) {
	        return q->error = "PFQ: SO_GROUP_LEAVE", -1;
	}
	return q->error = NULL, 0;
}

        
int 
pfq_poll(pfq_t *q, long int microseconds /* = -1 -> infinite */)
{
	if (q->fd == -1) {
		return q->error = "PFQ: not open", -1;
	}

	struct pollfd fd = {q->fd, POLLIN, 0 };
	struct timespec timeout = { microseconds/1000000, (microseconds%1000000) * 1000};

	int ret = ppoll(&fd, 1, microseconds < 0 ? NULL : &timeout, NULL);
	if (ret < 0) {
		return q->error = "PFQ: ppoll", -1;
	}
	return q->error = NULL, ret; 
}


int
pfq_stats(pfq_t const *q, struct pfq_stats *stats) 
{
	pfq_t *mutable = (pfq_t *)q;
	socklen_t size = sizeof(struct pfq_stats);
	if (getsockopt(q->fd, PF_Q, SO_GET_STATS, stats, &size) == -1) {
		return mutable->error = "PFQ: SO_GET_STATS", -1;
	}
	return mutable->error = NULL, 0;
}


int 
group_stats(pfq_t const *q, int gid, struct pfq_stats *stats) 
{
	pfq_t *mutable = (pfq_t *)q;
	socklen_t size = sizeof(struct pfq_stats);
	
	stats->recv = gid;
	if (getsockopt(q->fd, PF_Q, SO_GROUP_STATS, stats, &size) == -1) {
		return mutable->error = "PFQ: SO_GET_STATS", -1;
	}
	return mutable->error = NULL, 0;
}


/////


int 
pfq_read(pfq_t *q, struct pfq_net_queue *nq, long int microseconds) 
{
	struct pfq_queue_descr * qd = (struct pfq_queue_descr *)(q->queue_addr);
	int data   = qd->data;
	int index  = DBMP_QUEUE_INDEX(data);

	// size_t q_size = q->queue_slots * q->slot_size;

	//  watermark for polling...

	if( DBMP_QUEUE_LEN(data) < (q->queue_slots >> 1) ) {
		if (pfq_poll(q, microseconds) < 0)
		{
			return -1;
		}
	}

	// reset the next buffer...
	//

	data = __sync_lock_test_and_set(&qd->data, ((index+1) << 24));

	size_t queue_len = min(DBMP_QUEUE_LEN(data), q->queue_slots);

	nq->queue = (char *)(q->queue_addr) + sizeof(struct pfq_queue_descr) + (index & 1) * q->slot_size;
	nq->index = index;
	nq->len   = queue_len;
        nq->slot_size = q->slot_size; 
	return q->error = NULL, queue_len;
}


int 
pfq_recv(pfq_t *q, void *buf, size_t buflen, struct pfq_net_queue *nq, long int microseconds)
{
       	if (pfq_read(q, nq, microseconds) < 0)
		return -1;

	if (buflen < (q->queue_slots * q->slot_size)) {
		return q->error = "PFQ: buffer too small", -1;
	}

	memcpy(buf, nq->queue, q->slot_size * nq->len);
	return q->error = NULL, 0;
}


size_t
pfq_mem_size(pfq_t const *q) 
{
	return q->queue_tot_mem;
}


const void *
pfq_mem_addr(pfq_t const *q) 
{
	return q->queue_addr;
}



int
pfq_id(pfq_t *q)
{
 	return q->id;
}


int
pfq_group_id(pfq_t *q)
{
 	return q->gid;
}

