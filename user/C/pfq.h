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

#ifndef _PFQ_H_
#define _PFQ_H_

#include <stddef.h>
#include <linux/pf_q.h>

#ifdef _REENTRANT
#include <pthread.h>
#else
#include <sched.h>
#endif

/*! PFQ descriptor. */

typedef struct pfq pfq_t;

/*! PFQ iterator type. */

typedef char * pfq_iterator_t;

/*! pfq_net_queue is a struct which represents a net queue. */

struct pfq_net_queue
{
        pfq_iterator_t queue; 	  		/* net queue */
        size_t         len;       		/* number of packets in the queue */
        size_t         slot_size;
        unsigned int   index; 	  		/* current queue index */
};


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
const struct pfq_pkt_hdr *
pfq_iterator_header(pfq_iterator_t iter)
{
        return (const struct pfq_pkt_hdr *)iter;
}

/*! Given an iterator, return a pointer to the packet data. */

static inline
const char *
pfq_iterator_data(pfq_iterator_t iter)
{
        return (const char *)(iter + sizeof(struct pfq_pkt_hdr));
}

/*! Given an iterator, return 1 if the packet is available. */

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

typedef void (*pfq_handler_t)(char *user, const struct pfq_pkt_hdr *h, const char *data);


/* ************************************** *
 *          library functions             *
 * ************************************** */

/*!
 * group_policy: Q_POLICY_GROUP_PRIVATE, Q_POLICY_GROUP_RESTRICTED, Q_POLICY_GROUP_SHARED, Q_POLICY_GROUP_UNDEFINED...
 *
 * class_mask  : Q_CLASS_DEFAULT| .... = Q_CLASS_ANY
 */

/*! Open the socket. */
/*!
 * The default values are used; no group is joined or created.
 * The socket open is suitable for egress sockets.
 */

extern pfq_t* pfq_open_default();


/*! Open the socket and create a new private group. */
/*!
 * The default values for class mask and group policy are Q_CLASS_DEFAULT and
 * Q_POLICY_GROUP_PRIVATE, respectively.
 */

extern pfq_t* pfq_open(size_t calpen, size_t slots);


/*! Open the socket and create a new private group. */
/*!
 * The default values for class mask and group policy are Q_CLASS_DEFAULT and
 * Q_POLICY_GROUP_PRIVATE, respectively.
 */

extern pfq_t *pfq_open_(size_t caplen, size_t rx_slots, size_t maxlen, size_t tx_slots);


/*! Open the socket; no group is joined or created. */
/*!
 * Groups can be later joined by means of the join function.
 */

extern pfq_t* pfq_open_nogroup(size_t caplen, size_t slots);


/*! Open the socket and create a new group with the specified class and policy. */
/*!
 * All the possible parameters are specifiable.
 */

extern pfq_t* pfq_open_group(unsigned long class_mask, int group_policy,
                size_t calpen, size_t rx_slots,
                size_t maxlen, size_t tx_slots);

/*! Close the socket. */

extern int pfq_close(pfq_t *);

/*! Return the string error. */
/*!
 * Return a string of the most recent error.
 */

extern const char *pfq_error(pfq_t *);

/*! Given a device name, return the interface index. */

extern int pfq_ifindex(pfq_t const *q, const char *dev);

/*! Set the promiscuous mode for the given device. */

extern int pfq_set_promisc(pfq_t const *q, const char *dev, int value);

/*! Enable the socket for packet capture. */

extern int pfq_enable(pfq_t *q);

/*! Disable the socket for packet capture. */

extern int pfq_disable(pfq_t *q);

/*! Check whether the packet capture is enabled. */

extern int pfq_is_enabled(pfq_t const *q);

/*! Set the timestamping for packets. */

extern int pfq_timestamp_enable(pfq_t *q, int value);

/*! Check whether the timestamping is enabled for packets. */

extern int pfq_is_timestamp_enabled(pfq_t const *q);

/*! Specify the capture length of packets, in bytes. */
/*!
 * Capture length must be set before the socket is enabled for capture.
 */

extern int pfq_set_caplen(pfq_t *q, size_t value);

/*! Return the capture length of packets, in bytes. */

extern ssize_t pfq_get_caplen(pfq_t const *q);

/*! Specify the max transmission length of packets, in bytes. */

extern int pfq_set_maxlen(pfq_t *q, size_t value);

/*! Return the max transmission length of packets, in bytes. */

extern ssize_t pfq_get_maxlen(pfq_t const *q);

/*! Specify the length of the RX queue, in number of packets. */
/*!
 * The number of RX slots can't exceed the max value specified by
 * the rx_queue_slot kernel module parameter.
 */

extern int pfq_set_rx_slots(pfq_t *q, size_t value);

/*! Return the length of the RX queue, in number of packets. */

extern size_t pfq_get_rx_slots(pfq_t const *q);

/*! Specify the length of the TX queue, in number of packets. */
/*!
 * The number of TX slots can't exceed the max value specified by
 * the tx_queue_slot kernel module parameter.
 */

extern int pfq_set_tx_slots(pfq_t *q, size_t value);

/*! Return the length of the TX queue, in number of packets. */

extern size_t pfq_get_tx_slots(pfq_t const *q);

/*! Return the length of a RX slot, in bytes. */

extern size_t pfq_get_rx_slot_size(pfq_t const *q);

/*! Bind the main group of the socket to the given device/queue. */
/*!
 * The first argument is the name of the device;
 * the second argument is the queue number or any_queue.
 */

extern int pfq_bind(pfq_t *q, const char *dev, int queue);

/*! Bind the given group to the given device/queue. */
/*!
 * The first argument is the name of the device;
 * the second argument is the queue number or any_queue.
 */

extern int pfq_bind_group(pfq_t *q, int gid, const char *dev, int queue);

/*! Unbind the given group from the given device/queue. */

extern int pfq_unbind_group(pfq_t *q, int gid, const char *dev, int queue); /* Q_ANY_QUEUE */

/*! Unbind the main group of the socket from the given device/queue. */

extern int pfq_unbind(pfq_t *q, const char *dev, int queue);

/*! Mark the socket as egress and bind it to the given device/queue. */
/*!
 * The egress socket will be used within the capture groups as forwarder.
 */

extern int pfq_egress_bind(pfq_t *q, const char *dev, int queue);

/*! Unmark the socket as egress. */

extern int pfq_egress_unbind(pfq_t *q);


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
 * This function is experimental and is limited to simple functional computations.
 * Only the composition of monadic functions without binding arguments are supported.
 */

extern int pfq_set_group_computation_from_string(pfq_t *q, int gid, const char *prg);


/*! Specify a BPF program for the given group. */
/*!
 * This function can be used to set a specific BPF filter for the group.
 * It is used by the pfq pcap library.
 */

extern int pfq_group_fprog(pfq_t *q, int gid, struct sock_fprog *);

/*! Reset the BPF program fro the given group. */

extern int pfq_group_fprog_reset(pfq_t *q, int gid);

/*! Set vlan filtering for the given group. */

extern int pfq_vlan_filters_enable(pfq_t *q, int gid, int toggle);

/*! Specify a capture filter for the given group and vlan id. */
/*!
 *  In addition to standard vlan ids, valid ids are also Q_VLAN_UNTAG and Q_VLAN_ANYTAG.
 */

extern int pfq_vlan_set_filter(pfq_t *q, int gid, int vid);

/*! Reset vlan filter for the given group. */

extern int pfq_vlan_reset_filter(pfq_t *q, int gid, int vid);

/*! Join the group with the given class mask and group policy */

extern int pfq_join_group(pfq_t *q, int gid, unsigned long class_mask, int group_policy);

/*! Leave the given group. */

extern int pfq_leave_group(pfq_t *q, int gid);

/*! Wait for packets. */
/*!
 * Wait for packets available to read. A timeout in microseconds can be specified.
 */

extern int pfq_poll(pfq_t *q, long int microseconds /* = -1 -> infinite */);

/*! Read packets in place. */
/*!
 * Wait for packets and return the number of packets available in the queue.
 * Packets are stored in the memory mapped queue of the socket.
 * The timeout is specified in microseconds.
 */

extern int pfq_read(pfq_t *q, struct pfq_net_queue *nq, long int microseconds);

/*! Receive packets in the given mutable buffer. */
/*!
 * Wait for packets and return the number of packets available. Packets are stored in the given buffer.
 * It is possible to specify a timeout in microseconds.
 */

extern int pfq_recv(pfq_t *q, void *buf, size_t buflen, struct pfq_net_queue *nq, long int microseconds);

/*! This function takes a function pointer as callback, which is invoked on each packet captured. */
/*!
 * The callback is to be called with the following signature:
 *
 * typedef void (*pfq_handler)(char *user, const struct pfq_pkt_hdr *h, const char *data);
 */

extern int pfq_dispatch(pfq_t *q, pfq_handler_t cb, long int microseconds, char *user);

/*! Return the memory size of the RX queue. */

extern size_t pfq_mem_size(pfq_t const *q);

/*! Return the address of the RX queue. */

extern const void * pfq_mem_addr(pfq_t const *q);

/*! Return the id of the socket. */

extern int pfq_id(pfq_t *q);

/*! Return the group-id of the socket. */

extern int pfq_group_id(pfq_t *q);

/*! Return the underlying file descriptor. */

extern int pfq_get_fd(pfq_t const *q);

/*! Return the socket stats. */

extern int pfq_get_stats(pfq_t const *q, struct pfq_stats *stats);

/*! Return the stats of the given group. */

extern int pfq_get_group_stats(pfq_t const *q, int gid, struct pfq_stats *stats);

/*! Return the counters of the given group. */

extern int pfq_get_group_counters(pfq_t const *q, int gid, struct pfq_counters *cs);

/*! Bind the socket for transmission to the given device name and queue. */
/*!
 * A socket for transmission can be bound to a given device/queue at time.
 */

extern int pfq_bind_tx(pfq_t *q, const char *dev, int queue);

/*! Start the TX kernel thread. */

extern int pfq_start_tx_thread(pfq_t *q, int node);

/*! Stop the TX kernel thread. */

extern int pfq_stop_tx_thread(pfq_t *q);

/*! Wakeup the TX kernel thread. */
/*!
 * Wake up the TX kernel thread which transmits the packets in the Tx queue.
 * The kernel thread must be aldready started.
 */

extern int pfq_wakeup_tx_thread(pfq_t *q);

/*! Flush the TX queue, in the context of the calling thread. */
/*!
 * To invoke this function, no TX kernel thread is required.
 */

extern int pfq_tx_queue_flush(pfq_t *q);

/*! Schedule the packet for transmission. */
/*!
 * The packet is copied to the TX queue and later sent when
 * the tx_queue_flush or wakeup_tx_thread function are invoked.
 */

extern int pfq_inject(pfq_t *q, const void *ptr, size_t len);

/*! Transmit the packet stored in the given buffer. */

extern int pfq_send(pfq_t *q, const void *ptr, size_t len);

/*! Store the packet and possibly transmit the packets in the queue, synchronously. */
/*!
 * The transmission is invoked every n packets enqueued.
 */
extern int pfq_send_sync (pfq_t *q, const void *ptr, size_t len, size_t batch_len);

/*! Store the packet and possibly transmit the packets in the queue, asynchronously. */
/*!
 * The transmission is invoked in the kernel thread, every n packets enqueued.
 */

extern int pfq_send_async(pfq_t *q, const void *ptr, size_t len, size_t batch_len);


#endif /* _PFQ_H_ */
