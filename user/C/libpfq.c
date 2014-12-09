/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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

#include <linux/if_ether.h>
#include <linux/pf_q.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mman.h>

#include <net/if.h>
#include <net/ethernet.h>
#include <arpa/inet.h>

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <ctype.h>
#include <sched.h>

#include <poll.h>

#include <pfq.h>


/* useful macros */

#define  ALIGN8(value) ((value + 7) & ~(__typeof__(value))7)

#define Q_VALUE(q,value)   __builtin_choose_expr(__builtin_types_compatible_p(typeof(q), pfq_t *), (((pfq_t *)q)->error = NULL, (value)), \
	     	         ( __builtin_choose_expr(__builtin_types_compatible_p(typeof(q), pfq_t const *), (((pfq_t *)q)->error = NULL, (value)), (void)0)))

#define Q_ERROR(q,msg)	  __builtin_choose_expr(__builtin_types_compatible_p(typeof(q), pfq_t *), (((pfq_t *)q)->error = (msg), -1), \
	     	        ( __builtin_choose_expr(__builtin_types_compatible_p(typeof(q), pfq_t const *), (((pfq_t *)q)->error = (msg), -1), (void)0)))

#define Q_OK(q) Q_VALUE(q,0)


#define max(a,b) \
	({ __typeof__ (a) _a = (a); \
	   __typeof__ (b) _b = (b); \
	  _a > _b ? _a : _b; })

#define min(a,b) \
	({ __typeof__ (a) _a = (a); \
	   __typeof__ (b) _b = (b); \
	  _a < _b ? _a : _b; })


/* string utils */

static char *trim_string(char *str)
{
	int i = 0, j = strlen (str) - 1;

	while ( isspace ( str[i] ) && str[i] != '\0' )
		i++;
	while ( isspace ( str[j] ) && j >= 0 )
		j--;

	str[j+1] = '\0';
	return str+i;
}


static void split_on(const char *str, const char *sep, void (*cb)(char *))
{
	const char * p = str, *q;
	char *x;

	while((q = strstr(p, sep)) != NULL) {

		x = strndup(p, q-p);
		cb(x);
		p = q + strlen(sep);
	}

	x = strdup(p);
	cb(x);
}


static int with_tokens(const char *str, const char *sep, int (*cb)(char **, int n))
{
	char *tokens[64] = { NULL };
	int n = 0, i, ret;

	void push_back_ptr(char *ptr)
	{
		if (n < 64)
			tokens[n++] = ptr;
	}

	split_on(str, sep, push_back_ptr);

	ret = cb(tokens, n);

	for(i = 0; i < n; ++i)
	{
        	free(tokens[i]);
	}

	return ret;
}


/* pfq descriptor */

typedef struct pfq_data
{
	void * shm_addr;
	size_t shm_size;

	void * tx_queue_addr;
	size_t tx_queue_size;

	void * rx_queue_addr;
	size_t rx_queue_size;

	size_t rx_slots;
	size_t rx_slot_size;

        size_t tx_slots;
	size_t tx_slot_size;
	size_t tx_batch_count;
	size_t tx_num_bind;

	int    tx_last_inject;

	const char * error;

	int fd;
	int id;
	int gid;

	struct pfq_net_queue netq;
} pfq_t;

/* return the string error */

static __thread const char * __error;

const char *pfq_error(pfq_t *q)
{
        const char * p = q == NULL ? __error : q->error;
	return p == NULL ? "NULL" : p;
}


/* costructor */

pfq_t *
pfq_open_default()
{
	return pfq_open_group(Q_CLASS_DEFAULT, Q_POLICY_GROUP_UNDEFINED, 64, 1, 64, 1);
}


pfq_t *
pfq_open(size_t length, size_t slots)
{
	return pfq_open_group(Q_CLASS_DEFAULT, Q_POLICY_GROUP_PRIVATE, length, slots, length, slots);
}


pfq_t *
pfq_open_(size_t caplen, size_t rx_slots, size_t maxlen, size_t tx_slots)
{
	return pfq_open_group(Q_CLASS_DEFAULT, Q_POLICY_GROUP_PRIVATE, caplen, rx_slots, maxlen, tx_slots);
}


pfq_t *
pfq_open_nogroup(size_t caplen, size_t slots)
{
	return pfq_open_group(Q_CLASS_DEFAULT, Q_POLICY_GROUP_UNDEFINED, caplen, slots, caplen, slots);
}


pfq_t *
pfq_open_nogroup_(size_t caplen, size_t rx_slots, size_t maxlen, size_t tx_slots)
{
	return pfq_open_group(Q_CLASS_DEFAULT, Q_POLICY_GROUP_UNDEFINED, caplen, rx_slots, maxlen, tx_slots);
}


pfq_t *
pfq_open_group(unsigned long class_mask, int group_policy, size_t caplen, size_t rx_slots, size_t maxlen, size_t tx_slots)
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

	memset(q, 0, sizeof(pfq_t));

	q->fd 	= fd;
	q->id 	= -1;
	q->gid 	= -1;

        memset(&q->netq, 0, sizeof(q->netq));

	/* get id */
	socklen_t size = sizeof(q->id);
	if (getsockopt(fd, PF_Q, Q_SO_GET_ID, &q->id, &size) == -1) {
		return __error = "PFQ: get id error", free(q), NULL;
	}

	/* set rx queue slots */
	if (setsockopt(fd, PF_Q, Q_SO_SET_RX_SLOTS, &rx_slots, sizeof(rx_slots)) == -1) {
		return __error = "PFQ: set Rx slots error", free(q), NULL;
	}

	q->rx_slots = rx_slots;

	/* set caplen */
	if (setsockopt(fd, PF_Q, Q_SO_SET_RX_CAPLEN, &caplen, sizeof(caplen)) == -1) {
		return __error = "PFQ: set Rx caplen error", free(q), NULL;
	}

	q->rx_slot_size = ALIGN8(sizeof(struct pfq_pkt_hdr) + caplen);

	/* set Tx queue slots */
	if (setsockopt(fd, PF_Q, Q_SO_SET_TX_SLOTS, &tx_slots, sizeof(tx_slots)) == -1) {
		return __error = "PFQ: set Tx slots error", free(q), NULL;
	}

	q->tx_slots = tx_slots;
	q->tx_slot_size = ALIGN8(sizeof(struct pfq_pkt_hdr) + maxlen);

        /* set maxlen */
        if (setsockopt(fd, PF_Q, Q_SO_SET_TX_MAXLEN, &maxlen, sizeof(maxlen)) == -1)
        {
		return __error = "PFQ: set Tx maxlen error", free(q), NULL;
        }

	if (group_policy != Q_POLICY_GROUP_UNDEFINED)
	{
		q->gid = pfq_join_group(q, Q_ANY_GROUP, class_mask, group_policy);
		if (q->gid == -1) {
			return __error = q->error, free(q), NULL;
		}
	}

	return __error = NULL, q;
}


int pfq_close(pfq_t *q)
{
	if (q->fd != -1)
	{
		if (q->shm_addr)
			pfq_disable(q);

		if (close(q->fd) < 0)
			return Q_ERROR(q, "PFQ: close error");

		free(q);

                return Q_OK(q);
	}
	else
	{
		free(q);
		return __error = "PFQ: socket not open", -1;
	}
}


int
pfq_enable(pfq_t *q)
{
	if(setsockopt(q->fd, PF_Q, Q_SO_ENABLE, NULL, 0) == -1) {
		return Q_ERROR(q, "PFQ: socket enable");
	}

	size_t tot_mem; socklen_t size = sizeof(tot_mem);

	if (getsockopt(q->fd, PF_Q, Q_SO_GET_SHARED_MEM, &tot_mem, &size) == -1) {
		return Q_ERROR(q, "PFQ: queue memory error");
	}

	q->shm_size = tot_mem;

	if (q->shm_addr) {
		return Q_ERROR(q, "PFQ: queue already enabled");
	}

	if ((q->shm_addr = mmap(NULL, tot_mem, PROT_READ|PROT_WRITE, MAP_SHARED, q->fd, 0)) == MAP_FAILED) {
		return Q_ERROR(q, "PFQ: queue mmap error");
	}

       	q->rx_queue_addr = (char *)(q->shm_addr) + sizeof(struct pfq_queue_hdr);
        q->rx_queue_size = q->rx_slots * q->rx_slot_size;

        q->tx_queue_addr = (char *)(q->shm_addr) + sizeof(struct pfq_queue_hdr) + q->rx_queue_size * 2;
        q->tx_queue_size = q->tx_slots * q->tx_slot_size;

        return Q_OK(q);
}


int
pfq_disable(pfq_t *q)
{
	if (q->fd == -1)
		return Q_ERROR(q, "PFQ: socket not open");

	if (munmap(q->shm_addr,q->shm_size) == -1) {
		return Q_ERROR(q, "PFQ: munmap error");
	}

	q->shm_addr = NULL;
	q->shm_size = 0;

	if(setsockopt(q->fd, PF_Q, Q_SO_DISABLE, NULL, 0) == -1) {
		return Q_ERROR(q, "PFQ: socket disable");
	}
	return Q_OK(q);
}


int
pfq_is_enabled(pfq_t const *q)
{
	if (q->fd != -1)
	{
		int ret; socklen_t size = sizeof(ret);
		if (getsockopt(q->fd, PF_Q, Q_SO_GET_STATUS, &ret, &size) == -1) {
			return Q_ERROR(q, "PFQ: get status error");
		}
		return Q_VALUE(q, ret);
	}
	return Q_OK(q);
}


int
pfq_timestamp_enable(pfq_t *q, int value)
{
	int ts = value;
	if (setsockopt(q->fd, PF_Q, Q_SO_SET_RX_TSTAMP, &ts, sizeof(ts)) == -1) {
		return Q_ERROR(q, "PFQ: set timestamp mode");
	}
	return Q_OK(q);
}


int
pfq_is_timestamp_enabled(pfq_t const *q)
{
	int ret; socklen_t size = sizeof(int);

	if (getsockopt(q->fd, PF_Q, Q_SO_GET_RX_TSTAMP, &ret, &size) == -1) {
	        return Q_ERROR(q, "PFQ: get timestamp mode");
	}
	return Q_VALUE(q, ret);
}


int
pfq_ifindex(pfq_t const *q, const char *dev)
{
	struct ifreq ifreq_io;

	memset(&ifreq_io, 0, sizeof(struct ifreq));
	strncpy(ifreq_io.ifr_name, dev, IFNAMSIZ);
	if (ioctl(q->fd, SIOCGIFINDEX, &ifreq_io) == -1) {
		return Q_ERROR(q, "PFQ: ioctl get ifindex error");
	}
	return Q_VALUE(q, ifreq_io.ifr_ifindex);
}


int
pfq_set_promisc(pfq_t const *q, const char *dev, int value)
{
	struct ifreq ifreq_io;

	memset(&ifreq_io, 0, sizeof(struct ifreq));
	strncpy(ifreq_io.ifr_name, dev, IFNAMSIZ);

	if(ioctl(q->fd, SIOCGIFFLAGS, &ifreq_io) == -1) {
		return Q_ERROR(q, "PFQ: ioctl getflags error");
	}

	if (value)
		ifreq_io.ifr_flags |= IFF_PROMISC;
	else
		ifreq_io.ifr_flags &= ~IFF_PROMISC;

	if(ioctl(q->fd, SIOCSIFFLAGS, &ifreq_io) == -1) {
		return Q_ERROR(q, "PFQ: ioctl setflags error");
	}
	return Q_OK(q);
}


int
pfq_set_caplen(pfq_t *q, size_t value)
{
	int enabled = pfq_is_enabled(q);
	if (enabled == 1) {
		return Q_ERROR(q, "PFQ: enabled (caplen could not be set)");
	}

	if (setsockopt(q->fd, PF_Q, Q_SO_SET_RX_CAPLEN, &value, sizeof(value)) == -1) {
		return Q_ERROR(q, "PFQ: set caplen error");
	}

	q->rx_slot_size = ALIGN8(sizeof(struct pfq_pkt_hdr)+ value);
	return Q_OK(q);
}


ssize_t
pfq_get_caplen(pfq_t const *q)
{
	size_t ret; socklen_t size = sizeof(ret);

	if (getsockopt(q->fd, PF_Q, Q_SO_GET_RX_CAPLEN, &ret, &size) == -1) {
		return Q_ERROR(q, "PFQ: get caplen error");
	}
	return Q_VALUE(q, (ssize_t)ret);
}


int
pfq_set_maxlen(pfq_t *q, size_t value)
{
	int enabled = pfq_is_enabled(q);
	if (enabled == 1) {
		return Q_ERROR(q, "PFQ: enabled (maxlen could not be set)");
	}

	if (setsockopt(q->fd, PF_Q, Q_SO_SET_TX_MAXLEN, &value, sizeof(value)) == -1) {
		return Q_ERROR(q, "PFQ: set maxlen error");
	}

	q->rx_slot_size = ALIGN8(sizeof(struct pfq_pkt_hdr) + value);
	return Q_OK(q);
}


ssize_t
pfq_get_maxlen(pfq_t const *q)
{
	size_t ret; socklen_t size = sizeof(ret);

	if (getsockopt(q->fd, PF_Q, Q_SO_GET_TX_MAXLEN, &ret, &size) == -1) {
		return Q_ERROR(q, "PFQ: get maxlen error");
	}
	return Q_VALUE(q, (ssize_t)ret);
}


int
pfq_set_rx_slots(pfq_t *q, size_t value)
{
	int enabled = pfq_is_enabled(q);
	if (enabled == 1) {
		return Q_ERROR(q, "PFQ: enabled (slots could not be set)");
	}
	if (setsockopt(q->fd, PF_Q, Q_SO_SET_RX_SLOTS, &value, sizeof(value)) == -1) {
		return Q_ERROR(q, "PFQ: set Rx slots error");
	}

	q->rx_slots = value;
	return Q_OK(q);
}


size_t
pfq_get_rx_slots(pfq_t const *q)
{
	return q->rx_slots;
}


int
pfq_set_tx_slots(pfq_t *q, size_t value)
{
	int enabled = pfq_is_enabled(q);
	if (enabled == 1) {
		return Q_ERROR(q, "PFQ: enabled (Tx slots could not be set)");
	}
	if (setsockopt(q->fd, PF_Q, Q_SO_SET_TX_SLOTS, &value, sizeof(value)) == -1) {
		return Q_ERROR(q, "PFQ: set Tx slots error");
	}

	q->tx_slots = value;
	return Q_OK(q);
}


size_t
pfq_get_tx_slots(pfq_t const *q)
{
	return q->tx_slots;
}

size_t
pfq_get_rx_slot_size(pfq_t const *q)
{
	return q->rx_slot_size;
}


int
pfq_bind_group(pfq_t *q, int gid, const char *dev, int queue)
{
	struct pfq_binding b;
	int index;

	if (strcmp(dev, "any")==0) {
        	index = Q_ANY_DEVICE;
	}
	else {
		index = pfq_ifindex(q, dev);
		if (index == -1) {
			return Q_ERROR(q, "PFQ: bind_group: device not found");
		}
	}

	b.gid      = gid;
	b.if_index = index;
	b.hw_queue = queue;

	if (setsockopt(q->fd, PF_Q, Q_SO_GROUP_BIND, &b, sizeof(b)) == -1) {
		return Q_ERROR(q, "PFQ: bind error");
	}
	return Q_OK(q);
}


int
pfq_bind(pfq_t *q, const char *dev, int queue)
{
	int gid = q->gid;
	if (gid < 0) {
		return Q_ERROR(q, "PFQ: default group undefined");
	}
	return pfq_bind_group(q, gid, dev, queue);
}


int
pfq_egress_bind(pfq_t *q, const char *dev, int queue)
{
	struct pfq_binding b;

	int index;
	if (strcmp(dev, "any")==0) {
        	index = Q_ANY_DEVICE;
	}
	else {
		index = pfq_ifindex(q, dev);
		if (index == -1) {
			return Q_ERROR(q, "PFQ: egress_bind: device not found");
		}
	}

	b.gid = 0;
	b.if_index = index;
	b.hw_queue = queue;

        if (setsockopt(q->fd, PF_Q, Q_SO_EGRESS_BIND, &b, sizeof(b)) == -1)
		return Q_ERROR(q, "PFQ: egress bind error");

	return Q_OK(q);
}

int
pfq_egress_unbind(pfq_t *q)
{
        if (setsockopt(q->fd, PF_Q, Q_SO_EGRESS_UNBIND, 0, 0) == -1)
		return Q_ERROR(q, "PFQ: egress unbind error");

	return Q_OK(q);
}


int
pfq_unbind_group(pfq_t *q, int gid, const char *dev, int queue) /* Q_ANY_QUEUE */
{
	struct pfq_binding b;

	int index;
	if (strcmp(dev, "any")==0) {
        	index = Q_ANY_DEVICE;
	}
	else {
		index = pfq_ifindex(q, dev);
		if (index == -1) {
			return Q_ERROR(q, "PFQ: unbind_group: device not found");
		}
	}

	b.gid = gid;
	b.if_index = index;
	b.hw_queue = queue;

	if (setsockopt(q->fd, PF_Q, Q_SO_GROUP_UNBIND, &b, sizeof(b)) == -1) {
		return Q_ERROR(q, "PFQ: unbind error");
	}
	return Q_OK(q);
}


int
pfq_unbind(pfq_t *q, const char *dev, int queue)
{
	int gid = q->gid;
	if (gid < 0) {
		return Q_ERROR(q, "PFQ: default group undefined");
	}
	return pfq_unbind_group(q, gid, dev, queue);
}


int
pfq_groups_mask(pfq_t const *q, unsigned long *_mask)
{
	unsigned long mask; socklen_t size = sizeof(mask);

	if (getsockopt(q->fd, PF_Q, Q_SO_GET_GROUPS, &mask, &size) == -1) {
		return Q_ERROR(q, "PFQ: get groups error");
	}
	*_mask = mask;
	return Q_OK(q);
}

int
pfq_set_group_computation(pfq_t *q, int gid, struct pfq_computation_descr *comp)
{
        struct pfq_group_computation p = { gid, comp };

        if (setsockopt(q->fd, PF_Q, Q_SO_GROUP_FUNCTION, &p, sizeof(p)) == -1) {
		return Q_ERROR(q, "PFQ: group computation error");
        }

	return Q_OK(q);
}


int
pfq_set_group_computation_from_string(pfq_t *q, int gid, const char *comp)
{
	int do_set_group_computation(char **fun, int n)
	{
		int i = 0, ret;

                struct pfq_computation_descr * prog = malloc(sizeof(size_t) * 2 + sizeof(struct pfq_functional_descr) * n);
		if (!prog)
			return Q_ERROR(q, "PFQ: group computation error (no memory)");

		prog->entry_point = 0;
		prog->size = n;

		for(i = 0; i < n; i++)
		{
			prog->fun[i].symbol = trim_string(fun[i]);
			prog->fun[i].arg[0].ptr   = NULL;
			prog->fun[i].arg[0].size  = 0;
			prog->fun[i].arg[0].nelem = 0;
			prog->fun[i].arg[1].ptr   = NULL;
			prog->fun[i].arg[1].size  = 0;
			prog->fun[i].arg[1].nelem = 0;
			prog->fun[i].arg[2].ptr   = NULL;
			prog->fun[i].arg[2].size  = 0;
			prog->fun[i].arg[2].nelem = 0;
			prog->fun[i].arg[3].ptr   = NULL;
			prog->fun[i].arg[3].size  = 0;
			prog->fun[i].arg[3].nelem = 0;

			prog->fun[i].next = i+1;
		}

		ret = pfq_set_group_computation(q, gid, prog);

		free(prog);

		return ret;
	}

	return with_tokens(comp, ">->", &do_set_group_computation);
}


int
pfq_group_fprog(pfq_t *q, int gid, struct sock_fprog *f)
{
	struct pfq_fprog fprog;

	fprog.gid = gid;
	if (f != NULL)
	{
        	fprog.fcode.len = f->len;
        	fprog.fcode.filter = f->filter;
	}
	else
	{
        	fprog.fcode.len = 0;
        	fprog.fcode.filter = NULL;
	}

        if (setsockopt(q->fd, PF_Q, Q_SO_GROUP_FPROG, &fprog, sizeof(fprog)) == -1) {
		return Q_ERROR(q, "PFQ: set group fprog error");
	}

	return Q_OK(q);
}


int
pfq_group_fprog_reset(pfq_t *q, int gid)
{
	struct sock_fprog null = { 0, NULL };

	return pfq_group_fprog(q, gid, &null);
}


int
pfq_join_group(pfq_t *q, int gid, unsigned long class_mask, int group_policy)
{
	if (group_policy == Q_POLICY_GROUP_UNDEFINED) {
         	return Q_ERROR(q, "PFQ: join with undefined policy!");
	}

	struct pfq_group_join group = { gid, group_policy, class_mask };

	socklen_t size = sizeof(group);
	if (getsockopt(q->fd, PF_Q, Q_SO_GROUP_JOIN, &group, &size) == -1) {
	        return Q_ERROR(q, "PFQ: join group error");
	}

        if (q->gid == -1)
                q->gid = group.gid;

	return Q_VALUE(q, group.gid);
}


int
pfq_leave_group(pfq_t *q, int gid)
{
	if (setsockopt(q->fd, PF_Q, Q_SO_GROUP_LEAVE, &gid, sizeof(gid)) == -1) {
	        return Q_ERROR(q, "PFQ: leave group error");
	}
	if (q->gid == gid)
	        q->gid = -1;

	return Q_OK(q);
}


int
pfq_poll(pfq_t *q, long int microseconds /* = -1 -> infinite */)
{
	struct timespec timeout;
	struct pollfd fd = {q->fd, POLLIN, 0 };

	if (q->fd == -1) {
		return Q_ERROR(q, "PFQ: socket not open");
	}

	if (microseconds >= 0) {
		timeout.tv_sec  = microseconds/1000000;
		timeout.tv_nsec = (microseconds%1000000) * 1000;
	}

	int ret = ppoll(&fd, 1, microseconds < 0 ? NULL : &timeout, NULL);
	if (ret < 0 && errno != EINTR) {
	    return Q_ERROR(q, "PFQ: ppoll error");
	}
	return Q_OK(q);
}


int
pfq_get_stats(pfq_t const *q, struct pfq_stats *stats)
{
	socklen_t size = sizeof(struct pfq_stats);
	if (getsockopt(q->fd, PF_Q, Q_SO_GET_STATS, stats, &size) == -1) {
		return Q_ERROR(q, "PFQ: get stats error");
	}
	return Q_OK(q);
}


int
pfq_get_group_stats(pfq_t const *q, int gid, struct pfq_stats *stats)
{
	socklen_t size = sizeof(struct pfq_stats);

	stats->recv = (unsigned int)gid;
	if (getsockopt(q->fd, PF_Q, Q_SO_GET_GROUP_STATS, stats, &size) == -1) {
		return Q_ERROR(q, "PFQ: get group stats error");
	}
	return Q_OK(q);
}


int
pfq_get_group_counters(pfq_t const *q, int gid, struct pfq_counters *cs)
{
	socklen_t size = sizeof(struct pfq_counters);

	cs->counter[0] = (unsigned int)gid;

	if (getsockopt(q->fd, PF_Q, Q_SO_GET_GROUP_COUNTERS, cs, &size) == -1) {
		return Q_ERROR(q, "PFQ: get group counters error");
	}
	return Q_OK(q);
}


int
pfq_vlan_filters_enable(pfq_t *q, int gid, int toggle)
{
        struct pfq_vlan_toggle value = { gid, 0, toggle };

        if (setsockopt(q->fd, PF_Q, Q_SO_GROUP_VLAN_FILT_TOGGLE, &value, sizeof(value)) == -1) {
	        return Q_ERROR(q, "PFQ: vlan filters");
        }

        return Q_OK(q);
}

int
pfq_vlan_set_filter(pfq_t *q, int gid, int vid)
{
        struct pfq_vlan_toggle value = { gid, vid, 1 };

        if (setsockopt(q->fd, PF_Q, Q_SO_GROUP_VLAN_FILT, &value, sizeof(value)) == -1) {
	        return Q_ERROR(q, "PFQ: vlan set filter");
        }

        return Q_OK(q);
}

int pfq_vlan_reset_filter(pfq_t *q, int gid, int vid)
{
        struct pfq_vlan_toggle value = { gid, vid, 0 };

        if (setsockopt(q->fd, PF_Q, Q_SO_GROUP_VLAN_FILT, &value, sizeof(value)) == -1) {
	        return Q_ERROR(q, "PFQ: vlan reset filter");
        }

        return Q_OK(q);
}


int
pfq_read(pfq_t *q, struct pfq_net_queue *nq, long int microseconds)
{
	struct pfq_queue_hdr * qd;
	unsigned int index, data;

        if (q->shm_addr == NULL) {
         	return Q_ERROR(q, "PFQ: read: socket not enabled");
	}

	qd    = (struct pfq_queue_hdr *)(q->shm_addr);
	data  = qd->rx.data;
	index = MPDB_QUEUE_INDEX(data);

	if( MPDB_QUEUE_LEN(data) == 0 ) {
#ifdef PFQ_USE_POLL
		if (pfq_poll(q, microseconds) < 0) {
        		return Q_ERROR(q, "PFQ: poll error");
		}
#else
		(void)microseconds;
#endif
	}

	/* reset the next buffer... */

	data = __sync_lock_test_and_set(&qd->rx.data, ((index+1) << 24));

	size_t queue_len = min(MPDB_QUEUE_LEN(data), q->rx_slots);

	nq->queue = (char *)(q->rx_queue_addr) + (index & 1) * q->rx_queue_size;
	nq->index = index;
	nq->len   = queue_len;
        nq->slot_size = q->rx_slot_size;

	return Q_VALUE(q, (int)queue_len);
}


int
pfq_recv(pfq_t *q, void *buf, size_t buflen, struct pfq_net_queue *nq, long int microseconds)
{
       	if (pfq_read(q, nq, microseconds) < 0)
		return -1;

	if (buflen < (q->rx_slots * q->rx_slot_size)) {
		return Q_ERROR(q, "PFQ: buffer too small");
	}

	memcpy(buf, nq->queue, q->rx_slot_size * nq->len);
	return Q_OK(q);
}


int
pfq_dispatch(pfq_t *q, pfq_handler_t cb, long int microseconds, char *user)
{
	pfq_iterator_t it, it_end;
	int n = 0;

	if (pfq_read(q, &q->netq, microseconds) < 0)
		return -1;

	it = pfq_net_queue_begin(&q->netq);
	it_end = pfq_net_queue_end(&q->netq);

	for(; it != it_end; it = pfq_net_queue_next(&q->netq, it))
	{
		while (!pfq_iterator_ready(&q->netq, it))
			pfq_yield();

		cb(user, pfq_iterator_header(it), pfq_iterator_data(it));
		n++;
	}
        return Q_VALUE(q, n);
}

/* Tx APIs */

int
pfq_bind_tx(pfq_t *q, const char *dev, int queue, int core)
{
	struct pfq_binding b;

        int index = pfq_ifindex(q, dev);
        if (index == -1)
		return Q_ERROR(q, "PFQ: device not found");

	b.cpu = core;
	b.if_index = index;
	b.hw_queue = queue;

        if (setsockopt(q->fd, PF_Q, Q_SO_TX_BIND, &b, sizeof(b)) == -1)
		return Q_ERROR(q, "PFQ: Tx bind error");

	q->tx_num_bind++;

	return Q_OK(q);
}


int
pfq_unbind_tx(pfq_t *q)
{
        if (setsockopt(q->fd, PF_Q, Q_SO_TX_UNBIND, NULL, 0) == -1)
		return Q_ERROR(q, "PFQ: Tx unbind error");

	return Q_OK(q);
}

int
pfq_inject(pfq_t *q, const void *buf, size_t len, int queue)
{
        struct pfq_queue_hdr *qh = (struct pfq_queue_hdr *)(q->shm_addr);
        struct pfq_tx_queue_hdr *tx;
       	struct pfq_pkt_hdr *hdr;
       	char *pkt;
        int index;
	int tss;

	if (q->shm_addr == NULL)
         	return Q_ERROR(q, "PFQ: inject: socket not enabled");

	if (queue == Q_ANY_QUEUE) {
		tss = pfq_fold(pfq_symmetric_hash(buf), q->tx_num_bind);
	}
	else {
        	tss = pfq_fold(queue,q->tx_num_bind);
	}

        tx = (struct pfq_tx_queue_hdr *)&qh->tx[tss];

        index = pfq_spsc_write_index(tx);
        if (index == -1)
	        return Q_VALUE(q,-1);

        hdr = (struct pfq_pkt_hdr *)(
        		(char *)(q->tx_queue_addr) +
        		(q->tx_slots * tss + index) * tx->slot_size);

        pkt = (char *)(hdr + 1);

	hdr->len = min((uint16_t)len, (uint16_t)(tx->max_len));

        memcpy(pkt, buf, hdr->len);

        pfq_spsc_write_commit(tx);

        return Q_VALUE(q, len);
}


int
pfq_tx_queue_flush(pfq_t *q, int queue)
{
        if (setsockopt(q->fd, PF_Q, Q_SO_TX_FLUSH, &queue, sizeof(queue)) == -1)
		return Q_ERROR(q, "PFQ: Tx queue flush");

        return Q_OK(q);
}


int
pfq_send(pfq_t *q, const void *ptr, size_t len)
{
        int rc = pfq_inject(q, ptr, len, Q_ANY_QUEUE);

        if (rc > 0)
                pfq_tx_queue_flush(q, Q_ANY_QUEUE);

        return rc;
}


int
pfq_send_async(pfq_t *q, const void *ptr, size_t len, size_t batch_len)
{
        int rc = pfq_inject(q, ptr, len, Q_ANY_QUEUE);
        int do_flush = 0;

	q->tx_batch_count++;

	if (rc > 0) {
		q->tx_last_inject = 1;

        	if (q->tx_batch_count == batch_len) {
                	q->tx_batch_count = 0;
                	do_flush = 1;
		}
	}
	else {
       		if (q->tx_last_inject || q->tx_batch_count == batch_len) {
       			q->tx_batch_count = 0;
       			do_flush = 1;
		}
		q->tx_last_inject = 0;
	}

	if (batch_len && do_flush)
		pfq_tx_queue_flush(q, Q_ANY_QUEUE);

        return rc;
}


size_t
pfq_mem_size(pfq_t const *q)
{
	return q->shm_size;
}


const void *
pfq_mem_addr(pfq_t const *q)
{
	return q->shm_addr;
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


int pfq_get_fd(pfq_t const *q)
{
	return q->fd;
}
