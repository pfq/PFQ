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

#ifndef PFQ_H
#define PFQ_H

#include <stddef.h>

#include <linux/pf_q.h>
#include <linux/if_ether.h>
#include <linux/ip.h>
#include <linux/udp.h>
#include <arpa/inet.h>

#ifdef _REENTRANT
#include <pthread.h>
#else
#include <sched.h>
#endif

/*! PFQ descriptor. */

typedef struct pfq_data pfq_t;

/*! PFQ iterator type. */

typedef char * pfq_iterator_t;

/*! pfq_net_queue is a struct which represents a net queue. */

struct pfq_net_queue
{
	pfq_iterator_t queue;		/* net queue */
	size_t         len;		/* number of packets in the queue */
	size_t         slot_size;
	unsigned int   index;		/* current queue index */
};

/*! Initialize the net queue... */

static inline void
pfq_net_queue_init(struct pfq_net_queue *nq)
{
	nq->queue     = NULL;
	nq->len	      = 0;
	nq->slot_size = 0;
	nq->index     = 0;
}

/*! Return an iterator to the first slot of a non-empty queue. */

static inline
pfq_iterator_t
pfq_net_queue_begin(struct pfq_net_queue const *nq)
{
        return nq->queue;
}

/*! Return an iterator past to the end of the queue. */

static inline
pfq_iterator_t
pfq_net_queue_end(struct pfq_net_queue const *nq)
{
        return nq->queue + nq->len * nq->slot_size;
}

/*! Return an iterator to the next slot. */

static inline
pfq_iterator_t
pfq_net_queue_next(struct pfq_net_queue const *nq, pfq_iterator_t iter)
{
        return iter + nq->slot_size;
}

/*! Return an iterator to the previous slot. */

static inline
pfq_iterator_t
pfq_net_queue_prev(struct pfq_net_queue const *nq, pfq_iterator_t iter)
{
        return iter - nq->slot_size;
}

/*! Given an iterator, return a pointer to the packet header. */

static inline
const struct pfq_pkthdr *
pfq_pkt_header(pfq_iterator_t iter)
{
        return (const struct pfq_pkthdr *)iter;
}

/*! Given an iterator, return a pointer to the packet data. */

static inline
const char *
pfq_pkt_data(pfq_iterator_t iter)
{
        return (const char *)(iter + sizeof(struct pfq_pkthdr));
}

/*! Given an iterator, return 1 if the packet is available. */

static inline
int
pfq_pkt_ready(struct pfq_net_queue const *nq, pfq_iterator_t iter)
{
        if (__atomic_load_n(&pfq_pkt_header(iter)->commit,
			    __ATOMIC_ACQUIRE) != nq->index) {
                return 0;
        }
        return 1;
}

/*! Cause the calling thread to relinquish the CPU. */

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

/*! pfq handler: function prototype. */

typedef void (*pfq_handler_t)(char *user, const struct pfq_pkthdr *h, const char *data);


/*! Symmetric hashx */

static inline
unsigned int pfq_symmetric_hash(const char *buf)
{
        const char *ptr = buf;

        struct ethhdr const *eh = (struct ethhdr const *)(ptr);
        if (eh->h_proto != htons(0x800))
            return 0;

        ptr += sizeof(struct ethhdr);

        struct iphdr const *ih = (struct iphdr const *)(ptr);
        if (ih->protocol != IPPROTO_TCP &&
            ih->protocol != IPPROTO_UDP)
            return (ih->saddr ^ ih->daddr);

        ptr += sizeof(ih->ihl << 2);

        struct udphdr const *uh = (struct udphdr const *)(ptr);
        return (ih->saddr ^ ih->daddr ^ uh->source ^ uh->dest);

}


/*! Fold operation */

static inline
unsigned int pfq_fold(unsigned int hash, unsigned int n)
{
        hash = hash ^ (hash >> 8) ^ (hash >> 16) ^ (hash >> 24);
	return hash % n;

	/* switch(n) {
	 *     case 1: return 0;
	 *     case 2: return hash & 1;
	 *     case 3: {
	 *         return (hash & 3) != 3 ? (hash & 3) : 0;
	 *     }
	 *     case 4: return hash & 3;
	 * }

	 * return hash % n;
	 */
}


/* ************************************** *
 *          library functions             *
 * ************************************** */

/*! Return the string error. */
/*!
 * Return a string of the most recent error.
 */

extern const char *pfq_error(pfq_t *);


/*! Given a device name, return the interface index. */

extern int pfq_ifindex(pfq_t const *q, const char *dev);


/*! Set the promiscuous mode for the given device. */

extern int pfq_set_promisc(pfq_t const *q, const char *dev, int value);


/*!
 * group_policy: Q_POLICY_GROUP_PRIVATE, Q_POLICY_GROUP_RESTRICTED, Q_POLICY_GROUP_SHARED, Q_POLICY_GROUP_UNDEFINED...
 *
 * class_mask  : Q_CLASS_DEFAULT| .... = Q_CLASS_ANY
 */


/*! Open the socket and create a new private group. */
/*!
 * The default values for class mask and group policy are Q_CLASS_DEFAULT and
 * Q_POLICY_GROUP_PRIVATE, respectively.
 */

extern pfq_t *pfq_open(size_t caplen, size_t rx_slots, size_t tx_slots);


/*! Open the socket. No group is joined or created. */
/*!
 * Groups can later be joined by means of 'pfq_join_group' function.
 */

extern pfq_t* pfq_open_nogroup(size_t caplen, size_t rx_slots, size_t tx_slots);


/*! Open the socket and create a new group with the specified parameters. */
/*!
 * If group_policy is Q_POLICY_GROUP_UNDEFINED no gorup is joined or created.
 */

extern pfq_t* pfq_open_group(unsigned long class_mask, int group_policy,
                size_t caplen, size_t rx_slots, size_t tx_slots);


/*! Close the socket. */
/*!
 * Release the shared memory, stop kernel threads.
 */

extern int pfq_close(pfq_t *);


/*! Return the id of the socket. */

extern int pfq_id(pfq_t *q);


/*! Return the group-id of the socket. */

extern int pfq_group_id(pfq_t *q);


/*! Enable the socket for packets capture and transmission. */
/*!
 * Allocate the shared memory for socket queues possibly using
 * the Linux HugePages support.
 * If the enviroment variable PFQ_HUGEPAGES is set to 0 (or
 * PFQ_NO_HUGEPAGES is defined) standard 4K pages are used.
 */

extern int pfq_enable(pfq_t *q);


/*! Disable the socket. */
/*!
 * Release the shared memory, stop kernel threads.
 */

extern int pfq_disable(pfq_t *q);


/*! Check whether the socket is enabled. */

extern int pfq_is_enabled(pfq_t const *q);


/*! Enable/disable timestamping for packets. */

extern int pfq_timestamping_enable(pfq_t *q, int value);


/*! Check whether timestamping for packets is enabled. */

extern int pfq_is_timestamping_enabled(pfq_t const *q);


/*! Set the weight of the socket for the steering phase. */

extern int pfq_set_weight(pfq_t *q, int value);

/*! Return the weight of the socket. */

extern int pfq_get_weight(pfq_t const *q);


/*! Specify the capture length of packets, in bytes. */
/*!
 * Capture length must be set before the socket is enabled.
 */

extern int pfq_set_caplen(pfq_t *q, size_t value);


/*! Return the capture length of packets, in bytes. */

extern ssize_t pfq_get_caplen(pfq_t const *q);


/*! Return the max transmission length of packets, in bytes. */

extern ssize_t pfq_get_maxlen(pfq_t const *q);


/*! Specify the length of the Rx queue, in number of packets. */

extern int pfq_set_rx_slots(pfq_t *q, size_t value);


/*! Return the length of the Rx queue, in number of packets. */

extern size_t pfq_get_rx_slots(pfq_t const *q);


/*! Return the length of a Rx slot, in bytes. */

extern size_t pfq_get_rx_slot_size(pfq_t const *q);


/*! Specify the length of the Tx queue, in number of packets. */

extern int pfq_set_tx_slots(pfq_t *q, size_t value);


/*! Return the length of the Tx queue, in number of packets. */

extern size_t pfq_get_tx_slots(pfq_t const *q);


/*! Bind the main group of the socket to the given device/queue. */
/*!
 * The first argument is the name of the device;
 * the second argument is the queue number or Q_ANY_QUEUE.
 */

extern int pfq_bind(pfq_t *q, const char *dev, int queue);


/*! Unbind the main group of the socket from the given device/queue. */

extern int pfq_unbind(pfq_t *q, const char *dev, int queue);


/*! Bind the given group to the given device/queue. */
/*!
 * The first argument is the group id.
 * The second argument is the name of the device;
 * the third argument is the queue number or Q_ANY_QUEUE.
 */

extern int pfq_bind_group(pfq_t *q, int gid, const char *dev, int queue);


/*! Unbind the group from the given device/queue. */

extern int pfq_unbind_group(pfq_t *q, int gid, const char *dev, int queue);


/*! Set the socket as egress and bind it to the given device/queue. */
/*!
 * The egress socket is used by groups as network forwarder.
 */

extern int pfq_egress_bind(pfq_t *q, const char *dev, int queue);


/*! Unset the socket as egress. */

extern int pfq_egress_unbind(pfq_t *q);


/*! Bind the socket for transmission to the given device name and queue. */
/*!
 *  The core parameter specifies the CPU index where to run a
 *  kernel thread (unless Q_NO_KTHREAD is specified).
 */

extern int pfq_bind_tx(pfq_t *q, const char *dev, int queue, int core);


/*! Unbind the socket for transmission. */
/*!
 * Unbind the socket for transmission from any device/queue.
 */

extern int pfq_unbind_tx(pfq_t *q);


/*! Join the group with the given class mask and group policy */

extern int pfq_join_group(pfq_t *q, int gid, unsigned long class_mask, int group_policy);


/*! Leave the group specified by the group id. */

extern int pfq_leave_group(pfq_t *q, int gid);


/*! Return the mask of the joined groups. */
/*!
 * Each socket can bind to multiple groups. Each bit of the mask represents
 * a joined group.
 */

extern int pfq_groups_mask(pfq_t const *q, unsigned long *_mask);


/*! Specify a functional computation for the given group. */
/*!
 * The functional computation is specified by a pfq_computation_descriptor.
 */

extern int pfq_set_group_computation(pfq_t *q, int gid, struct pfq_computation_descr *prg);


/*! Specify a functional computation for the given group, from string. */
/*!
 * This ability is limited to simple PFQ/lang functional computations.
 * Only the composition of monadic functions without arguments are currently supported.
 */

extern int pfq_set_group_computation_from_string(pfq_t *q, int gid, const char *prg);


/*! Specify a BPF program for the given group. */
/*!
 * This function can be used to set a specific BPF filter for the group.
 * It is used by PFQ/pcap library.
 */

extern int pfq_group_fprog(pfq_t *q, int gid, struct sock_fprog *);


/*! Reset the BPF program fro the given group. */

extern int pfq_group_fprog_reset(pfq_t *q, int gid);


/*! Enable/disable vlan filtering for the given group. */

extern int pfq_vlan_filters_enable(pfq_t *q, int gid, int toggle);


/*! Specify a capture vlan filter for the given group. */
/*!
 *  In addition to standard vlan ids, valid ids are also Q_VLAN_UNTAG and Q_VLAN_ANYTAG.
 */

extern int pfq_vlan_set_filter(pfq_t *q, int gid, int vid);


/*! Reset the vlan filter for the given group. */

extern int pfq_vlan_reset_filter(pfq_t *q, int gid, int vid);


/*! Wait for packets. */
/*!
 * Wait for packets available for reading. A timeout in microseconds can be specified.
 */

extern int pfq_poll(pfq_t *q, long int microseconds /* = -1 -> infinite */);


/*! Read packets in place. */
/*!
 * Wait for packets and return the number of packets available.
 * References to packets are stored into the 'pfq_net_queue' data structure.
 *
 * The memory of the socket queue is reset at the next read.
 * A timeout is specified in microseconds.
 */

extern int pfq_read(pfq_t *q, struct pfq_net_queue *nq, long int microseconds);


/*! Receive packets in the given buffer. */
/*!
 * Wait for packets and return the number of packets available.
 * Packets are stored in the given buffer.
 * It is possible to specify a timeout in microseconds.
 */

extern int pfq_recv(pfq_t *q, void *buf, size_t buflen, struct pfq_net_queue *nq, long int microseconds);


/*! Collect and process packets. */
/*! The function takes a function pointer as callback.
 *  The callback must have the following signature:
 *
 * typedef void (*pfq_handler)(char *user, const struct pfq_pkthdr *h, const char *data);
 */

extern int pfq_dispatch(pfq_t *q, pfq_handler_t cb, long int microseconds, char *user);


/*! Return the memory size of the Rx queue. */

extern size_t pfq_mem_size(pfq_t const *q);


/*! Return the address of the Rx queue. */

extern const void * pfq_mem_addr(pfq_t const *q);


/*! Return the underlying file descriptor. */

extern int pfq_get_fd(pfq_t const *q);


/*! Return the socket statistics. */

extern int pfq_get_stats(pfq_t const *q, struct pfq_stats *stats);


/*! Return the statistics of the given group. */

extern int pfq_get_group_stats(pfq_t const *q, int gid, struct pfq_stats *stats);


/*! Return the set of counters of the given group. */

extern int pfq_get_group_counters(pfq_t const *q, int gid, struct pfq_counters *cs);


/*! Flush the Tx queue(s). */
/*!
 * Transmit the packets in the Tx queues of the socket.
 */

extern int pfq_tx_queue_flush(pfq_t *q, int queue);


/*! Start Tx kernel threads. */
/*!
 * Start kernel threads associated with the Tx queues of the socket.
 */

extern int pfq_tx_async_start(pfq_t *q);


/*! Stop Tx kernel threads. */
/*!
 * Stop kernel threads associated with the Tx queues of the socket.
 */

extern int pfq_tx_async_stop(pfq_t *q);


/*! Schedule the packet for transmission. */
/*!
 * The packet is copied into a Tx queue (using a TSS symmetric hash if any_queue is specified)
 * and transmitted at the given timestamp by a kernel thread or when tx_queue_flush is called.
 * A timestamp of 0 nanoseconds means 'immediate transmission'.
 */

extern int pfq_inject(pfq_t *q, const void *ptr, size_t len, uint64_t nsec, int copies, int queue);


/*! Store the packet and transmit the packets in the queue. */
/*!
 * The queue is flushed (if required) and the transmission takes place.
 */

extern int pfq_send(pfq_t *q, const void *ptr, size_t len, int copies);


/*! Store the packet and transmit the packets in the queue, asynchronously. */
/*!
 * The transmission is invoked every @flush_hint packets.
 * When TX kernel threads are in use, @flush_hint is ignored.
 */

extern int pfq_send_async(pfq_t *q, const void *ptr, size_t len, size_t flush_hint, int copies);


/*! Store the packet and transmit it. */
/*!
 * The transmission takes place asynchronously at the given timespec time.
 */

extern int pfq_send_at(pfq_t *q, const void *ptr, size_t len, struct timespec *ts, int copies);


#endif /* PFQ_H */
