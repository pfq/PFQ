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

#include <pfq/pfq-int.h>


#ifndef __cplusplus

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

static const int pfq_version_code	= PFQ_VERSION_CODE;
static const int pfq_major_version	= PFQ_MAJOR(PFQ_VERSION_CODE);
static const int pfq_minor_version	= PFQ_MINOR(PFQ_VERSION_CODE);
static const int pfq_patchlevel_version = PFQ_PATCHLEVEL(PFQ_VERSION_CODE);

extern const char *pfq_string_version;


#endif /*__cplusplus */

#ifdef __cplusplus
extern "C" {
#endif

/*! pfq handler: function prototype. */

typedef void (*pfq_handler_t)(char *user, const struct pfq_pkthdr *h, const char *data);


/*! Return the string error. */
/*!
 * Return a string of the most recent error.
 */

extern const char *pfq_error(pfq_t const *);


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
 *  The tid parameter specifies the index (id) of the transmitter
 *  thread. If 'Q_NO_KTHREAD' specified, bind refers to synchronous
 *  transmissions.
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
 * The functional computation is specified by a pfq_lang_computation_descriptor.
 */

extern int pfq_set_group_computation(pfq_t *q, int gid, struct pfq_lang_computation_descr const *prg);


/*! Specify a functional computation for the given group, from JSON description. */
/*!
 */
extern int pfq_set_group_computation_from_json(pfq_t *q, int gid, const char *prg);


/*! Specify a functional computation for the given group, from string as pfq-lang program. */
/*!
 */
extern int pfq_set_group_computation_from_string(pfq_t *q, int gid, const char *prg);


/*! Specify a BPF program for the given group. */
/*!
 * This function can be used to set a specific BPF filter for the group.
 * It is used by PFQ/pcap library.
 */

extern int pfq_group_fprog(pfq_t *q, int gid, struct sock_fprog const *);


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


/*! Transmit the packets in the queue. */
/*!
 * Transmit the packets in the queue of the socket. 'queue = 0' is the
 * queue enabled for synchronous transmission.
 */

extern int pfq_transmit_queue(pfq_t *q, int queue);


/*! Schedule packet transmission. */
/*!
 * The packet is copied into a Tx queue. If 'async' is 1 and 'queue' is set to any_queue, a TSS symmetric hash
 * function is used to select the Tx queue. The packet is transmitted at the given timestamp by a PFQ kernel thread.
 * Otherwise the queue is flushed every 'fhint' packets.
 * A timestamp of 0 nanoseconds means immediate transmission.
 */

extern int pfq_send_raw(pfq_t *q, const void *ptr, size_t len, int ifindex, int qindex, uint64_t nsec, unsigned int copies, int async, int queue);


/*! Store the packet and transmit the packets in the queue. */
/*!
 * The queue is flushed every fhint packets.
 * Requires the socket is bound for transmission to a net device and queue.
 * See 'pfq_bind_tx'.
 */

extern
int pfq_send(pfq_t *q, const void *ptr, size_t len, size_t fhint, unsigned int copies);


/*! Store the packet and transmit the packets in the queue. */
/*!
 * The queue is flushed every fhint packets.
 */

extern
int pfq_send_to(pfq_t *q, const void *ptr, size_t len, int ifindex, int qindex, size_t fhint, unsigned int copies);


/*! Transmit the packet asynchronously. */
/*!
 * The transmission is handled by PFQ kernel threads.
 * Requires the socket is bound for transmission to one (or multiple) PFQ kernel threads.
 * See 'pfq_bind_tx'.
 */

static inline
int pfq_send_async(pfq_t *q, const void *ptr, size_t len, unsigned int copies)
{
	return pfq_send_raw(q, ptr, len, 0, 0, 0, copies, 1, Q_ANY_QUEUE);
}


/*! Transmit the packet asynchronously. */
/*!
 * The transmission takes place asynchronously at the given timespec time.
 * Requires the socket is bound for transmission to one (or multiple) PFQ kernel threads.
 * See 'pfq_bind_tx'.
 */

static inline
int pfq_send_at(pfq_t *q, const void *ptr, size_t len, struct timespec *ts, unsigned int copies)
{
	uint64_t ns = (uint64_t)(ts->tv_sec)*1000000000ull + (uint64_t)ts->tv_nsec;
        return pfq_send_raw(q, ptr, len, 0, 0, ns, copies, 1, Q_ANY_QUEUE);
}


#ifdef __cplusplus
}
#endif

#endif /* PFQ_H */
