/***************************************************************
 *
 * (C) 2011-14 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#pragma once

#include <linux/if_ether.h>
#include <linux/pf_q.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <sys/mman.h>
#include <poll.h>

#include <tuple>
#include <memory>
#include <vector>
#include <type_traits>
#include <algorithm>
#include <thread>

#include <pfq-util.hpp>
#include <pfq-queue.hpp>

#include <pfq-lang/lang.hpp>

namespace net {

    //! group policies.
    /*!
     * Each group can be specified with the following policies:
     * undefined (not specified), priv (private group), restricted
     * (group shared among threads), shared (shared among threads/processes).
     */

    enum class group_policy : int16_t
    {
        undefined  = Q_POLICY_GROUP_UNDEFINED,
        priv       = Q_POLICY_GROUP_PRIVATE,
        restricted = Q_POLICY_GROUP_RESTRICTED,
        shared     = Q_POLICY_GROUP_SHARED
    };

    //! class mask.
    /*!
     * The default classes are class::default_ and class::any.
     */

    enum class class_mask : unsigned long
    {
        default_ = Q_CLASS_DEFAULT,
        any      = Q_CLASS_ANY
    };

    //! vlan options.
    /*!
     * Special vlan ids are untag (matches with untagged vlans) and anytag.
     */

    struct vlan_id
    {
        static constexpr int untag  = Q_VLAN_UNTAG;
        static constexpr int anytag = Q_VLAN_ANYTAG;
    };

    //////////////////////////////////////////////////////////////////////

    //! open parameters.

    namespace param
    {
        namespace
        {
            struct list_t {} constexpr list = list_t {};
        }

        struct class_   { class_mask value;   };
        struct policy   { group_policy value; };

        struct caplen   { size_t value; };
        struct rx_slots { size_t value; };
        struct maxlen   { size_t value; };
        struct tx_slots { size_t value; };

        using types = std::tuple<class_, policy, caplen, rx_slots, maxlen, tx_slots>;

        inline
        types make_default()
        {
            return std::make_tuple(param::class_   {class_mask::default_},
                                   param::policy   {group_policy::priv},
                                   param::caplen   {64},
                                   param::rx_slots {1024},
                                   param::maxlen   {64},
                                   param::tx_slots {1024});
        }
    }

    //////////////////////////////////////////////////////////////////////

    //! PFQ: the class
    /*!
     * This class is the main interface to the PFQ kernel module.
     * Each instance handles a socket that can be used to receive from and transmit
     * packets to the network.
     */

    class pfq
    {
        struct pfq_data
        {
            int id;
            int gid;

            void * queue_addr;
            size_t queue_tot_mem;
            size_t rx_caplen;
            size_t rx_slots;
            size_t rx_slot_size;

            size_t   tx_slots;
            uint64_t tx_counter;
        };

        int fd_;

        std::unique_ptr<pfq_data> pdata_;

    public:

        static constexpr int any_device  = Q_ANY_DEVICE;
        static constexpr int any_queue   = Q_ANY_QUEUE;
        static constexpr int any_group   = Q_ANY_GROUP;

        //! Default constructor

        pfq()
        : fd_(-1)
        , pdata_()
        {}

        //! Constructor with named-parameter idiom (param::get is the C++14 std::get)

        template <typename ...Ts>
        pfq(param::list_t, Ts&& ...args)
        : fd_(-1)
        , pdata_()
        {
            auto def = param::make_default();

            param::load(def, std::forward<Ts>(args)...);

            this->open(param::get<param::class_>(def).value,
                       param::get<param::policy>(def).value,
                       param::get<param::caplen>(def).value,
                       param::get<param::rx_slots>(def).value,
                       param::get<param::maxlen>(def).value,
                       param::get<param::tx_slots>(def).value);
        }

        //! Constructor
        /*!
         * Create a PFQ socket and join a new group.
         */

        pfq(size_t caplen, size_t rx_slots = 65536, size_t maxlen = 64, size_t tx_slots = 4096)
        : fd_(-1)
        , pdata_()
        {
            this->open(class_mask::default_, group_policy::priv, caplen, rx_slots, maxlen, tx_slots);
        }

        //! Constructor
        /*!
         * Create a PFQ socket with the given group policy (default class).
         */

        pfq(group_policy policy, size_t caplen, size_t rx_slots = 65536, size_t maxlen = 64, size_t tx_slots = 4096)
        : fd_(-1)
        , pdata_()
        {
            this->open(class_mask::default_, policy, caplen, rx_slots, maxlen, tx_slots);
        }

        //! Constructor
        /*!
         * Create a PFQ socket with the given class mask and group policy.
         */

        pfq(class_mask mask, group_policy policy, size_t caplen, size_t rx_slots = 65536, size_t maxlen = 64, size_t tx_slots = 4096)
        : fd_(-1)
        , pdata_()
        {
            this->open(mask, policy, caplen, rx_slots, maxlen, tx_slots);
        }

        //! Destructor: close the socket

        ~pfq()
        {
            this->close();
        }

        //! PFQ socket is a non-copyable resource.

        pfq(const pfq&) = delete;

        //! PFQ socket is a non-copy assignable resource.

        pfq& operator=(const pfq&) = delete;


        //! Move constructor.

        pfq(pfq &&other) noexcept
        : fd_(other.fd_)
        , pdata_(std::move(other.pdata_))
        {
            other.fd_ = -1;
        }

        //! Move assignment operator.

        pfq&
        operator=(pfq &&other) noexcept
        {
            if (this != &other)
            {
                fd_    = other.fd_;
                pdata_ = std::move(other.pdata_);
                other.fd_ = -1;
            }
            return *this;
        }

        //! Swap two PFQ sockets.

        void
        swap(pfq &other)
        {
            std::swap(fd_,    other.fd_);
            std::swap(pdata_, other.pdata_);
        }

        //! Open the PFQ socket with the given group policy.
        /*!
         * If the policy is not group_policy::undefined, also join a
         * new group with class_mask::default_ and the given policy.
         */

        void
        open(group_policy policy, size_t caplen, size_t rx_slots = 65536, size_t maxlen = 64, size_t tx_slots = 4096)
        {
            this->open(caplen, rx_slots, maxlen, tx_slots);

            if (policy != group_policy::undefined)
            {
                pdata_->gid = this->join_group(any_group, policy, class_mask::default_);
            }
        }

        //! Open the PFQ socket with the given class mask and group policy.
        /*!
         * If the policy is not group_policy::undefined, also join a
         * new group with the specified class mask and group policy.
         */

        void
        open(class_mask mask, group_policy policy, size_t caplen, size_t rx_slots = 65536, size_t maxlen = 64, size_t tx_slots = 4096)
        {
            this->open(caplen, rx_slots, maxlen, tx_slots);

            if (policy != group_policy::undefined)
            {
                pdata_->gid = this->join_group(any_group, policy, mask);
            }
        }

        //! Open the socket with named-parameter idiom.

        template <typename ...Ts>
        void open(param::list_t, Ts&& ...args)
        {
            auto def = param::make_default();

            param::load(def, std::forward<Ts>(args)...);

            this->open(param::get<param::class_>(def).value,
                       param::get<param::policy>(def).value,
                       param::get<param::caplen>(def).value,
                       param::get<param::rx_slots>(def).value,
                       param::get<param::maxlen>(def).value,
                       param::get<param::tx_slots>(def).value);
        }

        //! Return the id for the socket.

        int
        id() const
        {
            if (pdata_)
                return pdata_->id;
            return -1;
        }

        //! Return the group-id for the socket.

        int
        group_id() const
        {
            if (pdata_)
                return pdata_->gid;
            return -1;
        }

        //! Return the underlying file descriptor.

        int
        fd() const
        {
            return fd_;
        }


    private:

        void
        open(size_t caplen, size_t rx_slots, size_t maxlen, size_t tx_slots)
        {
            if (fd_ != -1)
                throw pfq_error("PFQ: socket already open");

            fd_ = ::socket(PF_Q, SOCK_RAW, htons(ETH_P_ALL));
            if (fd_ == -1)
                throw pfq_error("PFQ: module not loaded");

            // allocate pdata

            pdata_.reset(new pfq_data { -1,
                                        -1,
                                        nullptr,
                                        0,
                                        0,
                                        0,
                                        0,
                                        0,
                                        0
                                       });

            // get id

            socklen_t size = sizeof(pdata_->id);
            if (::getsockopt(fd_, PF_Q, Q_SO_GET_ID, &pdata_->id, &size) == -1)
                throw pfq_error(errno, "PFQ: get id error");

            // set RX queue slots

            if (::setsockopt(fd_, PF_Q, Q_SO_SET_RX_SLOTS, &rx_slots, sizeof(rx_slots)) == -1)
                throw pfq_error(errno, "PFQ: set RX slots error");

            pdata_->rx_slots = rx_slots;

            // set caplen

            if (::setsockopt(fd_, PF_Q, Q_SO_SET_RX_CAPLEN, &caplen, sizeof(caplen)) == -1)
                throw pfq_error(errno, "PFQ: set caplen error");

            pdata_->rx_caplen = caplen;

            pdata_->rx_slot_size = align<8>(sizeof(pfq_pkt_hdr) + pdata_->rx_caplen);

            // set TX queue slots

            if (::setsockopt(fd_, PF_Q, Q_SO_SET_TX_SLOTS, &tx_slots, sizeof(tx_slots)) == -1)
                throw pfq_error(errno, "PFQ: set TX slots error");

            pdata_->tx_slots = tx_slots;

            // set maxlen

            if (::setsockopt(fd_, PF_Q, Q_SO_SET_TX_MAXLEN, &maxlen, sizeof(maxlen)) == -1)
                throw pfq_error(errno, "PFQ: set maxlen error");
        }

    public:

        //! Close the socket.

        void
        close()
        {
            if (fd_ != -1)
            {
                if (pdata_ && pdata_->queue_addr)
                    this->disable();

                pdata_.reset(nullptr);

                if (::close(fd_) < 0)
                    throw pfq_error("FPQ: close");
                fd_ = -1;
            }
        }

        //! Enable the socket for packet capture.

        void
        enable()
        {
            int one = 1;

            if(::setsockopt(fd_, PF_Q, Q_SO_TOGGLE_QUEUE, &one, sizeof(one)) == -1) {
                throw pfq_error(errno, "PFQ: queue: out of memory");
            }

            size_t tot_mem; socklen_t size = sizeof(tot_mem);

            if (::getsockopt(fd_, PF_Q, Q_SO_GET_QUEUE_MEM, &tot_mem, &size) == -1)
                throw pfq_error(errno, "PFQ: queue memory error");

            pdata_->queue_tot_mem = tot_mem;

            if ((pdata_->queue_addr = mmap(nullptr, tot_mem, PROT_READ|PROT_WRITE, MAP_SHARED, fd_, 0)) == MAP_FAILED)
                throw pfq_error(errno, "PFQ: mmap error");

        }

        //! Disable the packet capture.

        void
        disable()
        {
            if (fd_ == -1)
                throw pfq_error("PFQ: socket not open");

            if (munmap(pdata_->queue_addr, pdata_->queue_tot_mem) == -1)
                throw pfq_error(errno, "PFQ: munmap error");

            pdata_->queue_addr = nullptr;
            pdata_->queue_tot_mem = 0;

            int zero = 0;
            if(::setsockopt(fd_, PF_Q, Q_SO_TOGGLE_QUEUE, &zero, sizeof(zero)) == -1)
                throw pfq_error(errno, "PFQ: queue cleanup error");
        }

        //! Check whether the packet capture is enabled.

        bool
        enabled() const
        {
            if (fd_ != -1)
            {
                int ret; socklen_t size = sizeof(ret);

                if (::getsockopt(fd_, PF_Q, Q_SO_GET_STATUS, &ret, &size) == -1)
                    throw pfq_error(errno, "PFQ: get status error");
                return ret;
            }
            return false;
        }

        //! Set the timestamping for packets.

        void
        timestamp_enable(bool value)
        {
            int ts = static_cast<int>(value);
            if (::setsockopt(fd_, PF_Q, Q_SO_SET_RX_TSTAMP, &ts, sizeof(ts)) == -1)
                throw pfq_error(errno, "PFQ: set timestamp mode");
        }

        //! Check whether the timestamping is enabled for packets.

        bool
        timestamp_enabled() const
        {
           int ret; socklen_t size = sizeof(int);
           if (::getsockopt(fd_, PF_Q, Q_SO_GET_RX_TSTAMP, &ret, &size) == -1)
                throw pfq_error(errno, "PFQ: get timestamp mode");
           return ret;
        }

        //! Specify the capture length of packets, in bytes.
        /*!
         * Capture length must be set before the socket is enabled to capture.
         */

        void
        caplen(size_t value)
        {
            if (enabled())
                throw pfq_error("PFQ: enabled (caplen could not be set)");

            if (::setsockopt(fd_, PF_Q, Q_SO_SET_RX_CAPLEN, &value, sizeof(value)) == -1) {
                throw pfq_error(errno, "PFQ: set caplen error");
            }

            pdata_->rx_slot_size = align<8>(sizeof(pfq_pkt_hdr)+ value);
        }

        //! Return the capture length of packets, in bytes.

        size_t
        caplen() const
        {
           size_t ret; socklen_t size = sizeof(ret);
           if (::getsockopt(fd_, PF_Q, Q_SO_GET_RX_CAPLEN, &ret, &size) == -1)
                throw pfq_error(errno, "PFQ: get caplen error");
           return ret;
        }

        //! Specify the max transmission length of packets, in bytes.

        void
        maxlen(size_t value)
        {
            if (enabled())
                throw pfq_error("PFQ: enabled (maxlen could not be set)");

            if (::setsockopt(fd_, PF_Q, Q_SO_SET_TX_MAXLEN, &value, sizeof(value)) == -1) {
                throw pfq_error(errno, "PFQ: set maxlen error");
            }
        }

        //! Return the max transmission length of packets, in bytes.

        size_t
        maxlen() const
        {
           size_t ret; socklen_t size = sizeof(ret);
           if (::getsockopt(fd_, PF_Q, Q_SO_GET_TX_MAXLEN, &ret, &size) == -1)
                throw pfq_error(errno, "PFQ: get maxlen error");
           return ret;
        }

        //! Specify the length of the RX queue, in number of packets.
        /*!
         * The number of RX slots can't exceed the max value specified by
         * the rx_queue_slot kernel module parameter.
         */

        void
        rx_slots(size_t value)
        {
            if (enabled())
                throw pfq_error("PFQ: enabled (RX slots could not be set)");

            if (::setsockopt(fd_, PF_Q, Q_SO_SET_RX_SLOTS, &value, sizeof(value)) == -1) {
                throw pfq_error(errno, "PFQ: set RX slots error");
            }

            pdata_->rx_slots = value;
        }

        //! Return the length of the RX queue, in number of packets.

        size_t
        rx_slots() const
        {
            if (!pdata_)
                throw pfq_error("PFQ: socket not open");

            return pdata_->rx_slots;
        }

        //! Specify the length of the TX queue, in number of packets.
        /*!
         * The number of TX slots can't exceed the max value specified by
         * the tx_queue_slot kernel module parameter.
         */

        void
        tx_slots(size_t value)
        {
            if (enabled())
                throw pfq_error("PFQ: enabled (TX slots could not be set)");

            if (::setsockopt(fd_, PF_Q, Q_SO_SET_TX_SLOTS, &value, sizeof(value)) == -1) {
                throw pfq_error(errno, "PFQ: set TX slots error");
            }

            pdata_->tx_slots = value;
        }

        //! Return the length of the TX queue, in number of packets.

        size_t
        tx_slots() const
        {
           if (!pdata_)
                throw pfq_error("PFQ: socket not open");

           return pdata_->tx_slots;
        }

        //! Return the length of a RX slot, in bytes.

        size_t
        rx_slot_size() const
        {
            if (!pdata_)
                throw pfq_error("PFQ: socket not open");

            return pdata_->rx_slot_size;
        }

        //! Bind the main group of the socket to the given device/queue.
        /*!
         * The first argument is the name of the device;
         * the second argument is the queue number or any_queue.
         */

        void
        bind(const char *dev, int queue = any_queue)
        {
            auto gid = group_id();
            if (gid < 0)
                throw pfq_error("PFQ: default group undefined");

            bind_group(gid, dev, queue);
        }

        //! Bind the given group to the given device/queue.
        /*!
         * The first argument is the name of the device;
         * the second argument is the queue number or any_queue.
         */

        void
        bind_group(int gid, const char *dev, int queue = any_queue)
        {
            auto index = ifindex(this->fd(), dev);
            if (index == -1)
                throw pfq_error("PFQ: device not found");

            struct pfq_binding b = { gid, index, queue };
            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_BIND, &b, sizeof(b)) == -1)
                throw pfq_error(errno, "PFQ: add binding error");
        }

        //! Unbind the main group of the socket from the given device/queue.

        void
        unbind(const char *dev, int queue = any_queue)
        {
            auto gid = group_id();
            if (gid < 0)
                throw pfq_error("PFQ: default group undefined");

            unbind_group(gid, dev, queue);
        }

        //! Unbind the given group from the given device/queue.

        void
        unbind_group(int gid, const char *dev, int queue = any_queue)
        {
            auto index = ifindex(this->fd(), dev);
            if (index == -1)
                throw pfq_error("PFQ: device not found");

            struct pfq_binding b = { gid, index, queue };
            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_UNBIND, &b, sizeof(b)) == -1)
                throw pfq_error(errno, "PFQ: remove binding error");
        }

        //! Mark the socket as egress and bind it to the given device/queue.
        /*!
         * The egress socket will be used within the capture groups as forwarder.
         */

        void
        egress_bind(const char *dev, int queue = any_queue)
        {
            auto index = ifindex(this->fd(), dev);
            if (index == -1)
                throw pfq_error("PFQ: egress: device not found");

            struct pfq_binding b = { 0, index, queue };

            if (::setsockopt(fd_, PF_Q, Q_SO_EGRESS_BIND, &b, sizeof(b)) == -1)
                throw pfq_error(errno, "PFQ: egress bind error");

        }

        //! Unmark the socket as egress.

        void
        egress_unbind()
        {
            if (::setsockopt(fd_, PF_Q, Q_SO_EGRESS_UNBIND, 0, 0) == -1)
                throw pfq_error(errno, "PFQ: egress unbind error");
        }

        //! Return the mask of the joined groups.
        /*!
         * Each socket can bind to multiple groups. Each bit of the mask represents
         * a joined group.
         */

        unsigned long
        groups_mask() const
        {
            unsigned long mask; socklen_t size = sizeof(mask);
            if (::getsockopt(fd_, PF_Q, Q_SO_GET_GROUPS, &mask, &size) == -1)
                throw pfq_error(errno, "PFQ: get groups error");
            return mask;
        }

        //! Obtain the list of the joined groups.

        std::vector<int>
        groups() const
        {
            std::vector<int> vec;
            auto grps = this->groups_mask();
            for(int n = 0; grps != 0; n++)
            {
                if (grps & (1L << n)) {
                    vec.push_back(n);
                    grps &= ~(1L << n);
                }
            }

            return vec;
        }

        //! Specify a functional computation for the given group.
        /*!
         * The functional computation is specified by the eDSL pfq-lang.
         */

        template <typename Comp>
        void set_group_computation(int gid, Comp const &comp)
        {
            auto ser = pfq_lang::serialize(comp, 0).first;

            std::unique_ptr<pfq_computation_descr> prg (
                reinterpret_cast<pfq_computation_descr *>(malloc(sizeof(size_t) * 2 + sizeof(pfq_functional_descr) * ser.size())));

            prg->size = ser.size();
            prg->entry_point = 0;

            int n = 0;

            for(auto & descr : ser)
            {
                prg->fun[n].symbol = descr.symbol.c_str();

                for(size_t i = 0; i < sizeof(prg->fun[0].arg)/sizeof(prg->fun[0].arg[0]); i++)
                {
                    prg->fun[n].arg[i].ptr  = descr.arg[i].ptr ? descr.arg[i].ptr->forall_addr() : nullptr;
                    prg->fun[n].arg[i].size = descr.arg[i].size;
                }

                prg->fun[n].next  = descr.next;

                n++;
            }

            set_group_computation(gid, prg.get());
        }

        //! Specify a functional computation for the given group.
        /*!
         * The functional computation is specified by a pfq_computation_descriptor.
         * This function should not be used; use the pfq-lang eDSL instead.
         */

        void
        set_group_computation(int gid, pfq_computation_descr *prog)
        {
            struct pfq_group_computation p { gid, prog };
            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_FUNCTION, &p, sizeof(p)) == -1)
                throw pfq_error(errno, "PFQ: group computation error");
        }


        //! Specify a functional computation for the given group, from string.
        /*!
         * This function is experimental and is limited to simple functional computations.
         * Only the composition of monadic functions without binding arguments are supported.
         */

        void
        set_group_computation(int gid, std::string prog)
        {
            std::vector<pfq_lang::MFunction> comp;
            auto fs = split(prog, ">->");

            for (auto & f : fs)
            {
                comp.push_back(pfq_lang::mfunction(trim(f)));
            }

            set_group_computation(gid, comp);
        }


        //! Specify a BPF program for the given group.
        /*!
         * This function can be used to set a specific BPF filter for the group.
         * It is used by the pfq pcap library.
         */

        void
        set_group_fprog(int gid, const sock_fprog &f)
        {
            struct pfq_fprog fprog = { gid, f };

            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_FPROG, &fprog, sizeof(fprog)) == -1)
                throw pfq_error(errno, "PFQ: set group fprog error");
        }

        //! Reset the BPF program fro the given group.

        void
        reset_group_fprog(int gid)
        {
            struct pfq_fprog fprog = { gid, {0, 0} };

            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_FPROG, &fprog, sizeof(fprog)) == -1)
                throw pfq_error(errno, "PFQ: reset group fprog error");
        }

        //! Join the given group.
        /*!
         * If the policy is not specified, use group_policy::shared by default.
         * If the class mask is not specified, use the class_mask::default_.
         */

        int
        join_group(int gid, group_policy pol = group_policy::shared, class_mask mask = class_mask::default_)
        {
            if (pol == group_policy::undefined)
                throw pfq_error("PFQ: join with undefined policy!");

            struct pfq_group_join group { gid, static_cast<int16_t>(pol), static_cast<unsigned int>(mask) };

            socklen_t size = sizeof(group);

            if (::getsockopt(fd_, PF_Q, Q_SO_GROUP_JOIN, &group, &size) == -1)
                throw pfq_error(errno, "PFQ: join group error");

            if (pdata_->gid == -1)
                pdata_->gid = group.gid;

            return group.gid;
        }

        //! Leave the given group.

        void
        leave_group(int gid)
        {
            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_LEAVE, &gid, sizeof(gid)) == -1)
                throw pfq_error(errno, "PFQ: leave group error");

            if (pdata_->gid == gid)
                pdata_->gid = -1;
        }

        //! Wait for packets.
        /*!
         * Wait for packets available to read. A timeout in microseconds can be specified.
         */

        int
        poll(long int microseconds = -1 /* infinite */)
        {
            if (fd_ == -1)
                throw pfq_error("PFQ: socket not open");

            struct pollfd fd = {fd_, POLLIN, 0 };
            struct timespec timeout = { microseconds/1000000, (microseconds%1000000) * 1000};

            int ret = ::ppoll(&fd, 1, microseconds < 0 ? nullptr : &timeout, nullptr);
            if (ret < 0 &&
            	errno != EINTR)
               throw pfq_error(errno, "PFQ: ppoll");
            return 0;
        }

        //! Read packets in place.
        /*!
         * Wait for packets to read and return a queue descriptor.
         * Packets are stored in the memory mapped queue of the socket.
         * The timeout is specified in microseconds.
         */

        queue
        read(long int microseconds = -1)
        {
            if (!pdata_ || !pdata_->queue_addr)
                throw pfq_error("PFQ: not enabled");

            auto q = static_cast<struct pfq_queue_hdr *>(pdata_->queue_addr);

            size_t data = q->rx.data;
            size_t index = MPDB_QUEUE_INDEX(data);
            size_t q_size = pdata_->rx_slots * pdata_->rx_slot_size;

            //  watermark for polling...

            if( MPDB_QUEUE_LEN(data) < (pdata_->rx_slots >> 1) ) {
                this->poll(microseconds);
            }

            // reset the next buffer...

            data = __sync_lock_test_and_set(&q->rx.data, (unsigned int)((index+1) << 24));

            auto queue_len =  std::min(static_cast<size_t>(MPDB_QUEUE_LEN(data)), pdata_->rx_slots);

            return queue(static_cast<char *>(pdata_->queue_addr) +
						    sizeof(pfq_queue_hdr) +
						    (index & 1) * q_size,
                            pdata_->rx_slot_size, queue_len, index);
        }

        //! Return the current commit version (used internally by the memory mapped queue).

        uint8_t
        current_commit() const
        {
            auto q = static_cast<struct pfq_queue_hdr *>(pdata_->queue_addr);
            return MPDB_QUEUE_INDEX(q->rx.data);
        }

        //! Receive packets in the given mutable buffer.
        /*!
         * Wait for packets and return a queue descriptor. Packets are stored in the given mutable buffer.
         * It is possible to specify a timeout in microseconds.
         */

        queue
        recv(const mutable_buffer &buff, long int microseconds = -1)
        {
            if (fd_ == -1)
                throw pfq_error("PFQ: socket not open");

            auto this_queue = this->read(microseconds);

            if (buff.second < pdata_->rx_slots * pdata_->rx_slot_size)
                throw pfq_error("PFQ: buffer too small");

            memcpy(buff.first, this_queue.data(), this_queue.slot_size() * this_queue.size());
            return queue(buff.first, this_queue.slot_size(), this_queue.size(), this_queue.index());
        }

        //! This function takes an instance of a callable type which is invoked on each packet captured.
        /*!
         * The object must provide the following callable signature:
         *
         * typedef void (*pfq_handler)(char *user, const struct pfq_pkt_hdr *h, const char *data);
         */

        template <typename Fun>
        size_t dispatch(Fun callback, long int microseconds = -1, char *user = nullptr)
        {
            auto many = this->read(microseconds);

            auto it = std::begin(many),
                 it_e = std::end(many);
            size_t n = 0;
            for(; it != it_e; ++it)
            {
                while (!it.ready())
                    std::this_thread::yield();

                callback(user, &(*it), reinterpret_cast<const char *>(it.data()));
                n++;
            }
            return n;
        }

        //! Set vlan filtering for the given group.

        void vlan_filters_enable(int gid, bool toggle)
        {
            pfq_vlan_toggle value { gid, 0, toggle};

            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_VLAN_FILT_TOGGLE, &value, sizeof(value)) == -1)
                throw pfq_error(errno, "PFQ: vlan filters");
        }

        //! Specify a capture filter for the given group and vlan id.
        /*!
         *  In addition to standard vlan ids, valid ids are also vlan_id::untag and vlan_id::anytag.
         */

        void vlan_set_filter(int gid, int vid)
        {
            pfq_vlan_toggle value { gid, vid, true};

            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_VLAN_FILT, &value, sizeof(value)) == -1)
                throw pfq_error(errno, "PFQ: vlan set filter");
        }

        //! Specify the vlan capture filters in the given range.

        template <typename Iter>
        void vlan_set_filter(int gid, Iter beg, Iter end)
        {
            std::for_each(beg, end, [&](int vid) {
                vlan_set_filter(gid, vid);
            });
        }

        //! Reset all vlan filters.

        void vlan_reset_filter(int gid, int vid)
        {
            pfq_vlan_toggle value { gid, vid, false};

            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_VLAN_FILT, &value, sizeof(value)) == -1)
                throw pfq_error(errno, "PFQ: vlan reset filter");
        }

        //! Reset the vlan id filters specified in the given range.

        template <typename Iter>
        void vlan_reset_filter(int gid, Iter beg, Iter end)
        {
            std::for_each(beg, end, [&](int vid) {
                vlan_reset_filter(gid, vid);
            });
        }

        //! Return the socket stats.

        pfq_stats
        stats() const
        {
            pfq_stats stat;
            socklen_t size = sizeof(struct pfq_stats);
            if (::getsockopt(fd_, PF_Q, Q_SO_GET_STATS, &stat, &size) == -1)
                throw pfq_error(errno, "PFQ: get stats error");
            return stat;
        }

        //! Return the stats of the given group.

        pfq_stats
        group_stats(int gid) const
        {
            pfq_stats stat;
            stat.recv = static_cast<unsigned long>(gid);
            socklen_t size = sizeof(struct pfq_stats);
            if (::getsockopt(fd_, PF_Q, Q_SO_GET_GROUP_STATS, &stat, &size) == -1)
                throw pfq_error(errno, "PFQ: get group stats error");
            return stat;
        }

        //! Return the counters of the given group.

        std::vector<unsigned long>
        group_counters(int gid) const
        {
            pfq_counters cs;
            cs.counter[0] = static_cast<unsigned long>(gid);
            socklen_t size = sizeof(struct pfq_counters);
            if (::getsockopt(fd_, PF_Q, Q_SO_GET_GROUP_COUNTERS, &cs, &size) == -1)
                throw pfq_error(errno, "PFQ: get group counters error");

            return std::vector<unsigned long>(std::begin(cs.counter), std::end(cs.counter));
        }

        //! Return the memory size of the RX queue.

        size_t
        mem_size() const
        {
            if (pdata_)
                return pdata_->queue_tot_mem;
            return 0;
        }

        //! Return the address of the RX queue.

        const void *
        mem_addr() const
        {
            if (pdata_)
                return pdata_->queue_addr;
            return nullptr;
        }

        //! Bind the socket for transmission to the given device name and queue.
        /*!
         * A socket for transmission can be bound to a given device/queue at time.
         */

        void
        bind_tx(const char *dev, int queue = any_queue)
        {
            auto index = ifindex(this->fd(), dev);
            if (index == -1)
                throw pfq_error("PFQ: device not found");

            struct pfq_binding b = { 0, index, queue };

            if (::setsockopt(fd_, PF_Q, Q_SO_TX_THREAD_BIND, &b, sizeof(b)) == -1)
                throw pfq_error(errno, "PFQ: TX bind error");
        }

        //! Transmit the packet stored in the given buffer.

        bool
        send(const_buffer pkt)
        {
            auto ret = inject(pkt);

            if (ret)
                tx_queue_flush();

            return ret;
        }

        //! Store the packet and possibly transmit the packets in the queue, synchronously.
        /*!
         * The transmission is invoked every n packets enqueued.
         */

        bool
        send_sync(const_buffer pkt, size_t n = 128)
        {
            auto ret = inject(pkt);

            if ((pdata_->tx_counter ++ % n) == 0)
                tx_queue_flush();

            return ret;
        }

        //! Store the packet and possibly transmit the packets in the queue, asynchronously.
        /*!
         * The transmission is invoked in the kernel thread, every n packets enqueued.
         */

        bool
        send_async(const_buffer pkt, size_t n = 128)
        {
            auto ret = inject(pkt);

            if ((pdata_->tx_counter ++ % n) == 0)
                wakeup_tx_thread();

            return ret;
        }

        //! Schedule the packet for transmission.
        /*!
         * The packet is copied to the TX queue and later sent when
         * the tx_queue_flush or wakeup_tx_thread function are invoked.
         */

        bool
        inject(const_buffer pkt)
        {
            auto q  = static_cast<struct pfq_queue_hdr *>(pdata_->queue_addr);
            auto tx = &q->tx;

            int index = pfq_spsc_write_index(tx);
            if (index == -1)
                return false;

            auto h = reinterpret_cast<pfq_pkt_hdr *>(reinterpret_cast<char *>(q + 1) +
                        pdata_->rx_slots * pdata_->rx_slot_size * 2  + index * tx->slot_size);

            auto addr = reinterpret_cast<char *>(h + 1);

            h->len = std::min(pkt.second, static_cast<size_t>(tx->max_len));

            memcpy(addr, pkt.first, h->len);

            pfq_spsc_write_commit(tx);
            return true;
        }

        //! Start the TX kernel thread.

        void start_tx_thread(int node)
        {
            if (::setsockopt(fd_, PF_Q, Q_SO_TX_THREAD_START, &node, sizeof(node)) == -1)
                throw pfq_error(errno, "PFQ: start TX thread");
        }

        //! Stop the TX kernel thread.

        void stop_tx_thread()
        {
            if (::setsockopt(fd_, PF_Q, Q_SO_TX_THREAD_STOP, nullptr, 0) == -1)
                throw pfq_error(errno, "PFQ: stop TX thread");
        }

        //! Wakeup the TX kernel thread.
        /*!
         * Wake up the TX kernel thread which transmit the packets in the Tx queue.
         * The kernel thread must be started.
         */

        void wakeup_tx_thread()
        {
            if (::setsockopt(fd_, PF_Q, Q_SO_TX_THREAD_WAKEUP, nullptr, 0) == -1)
                throw pfq_error(errno, "PFQ: wakeup TX thread");
        }

        //! Flush the TX queue, in the context of the calling thread.
        /*!
         * To invoke this function, no TX kernel thread is required.
         */

        void tx_queue_flush()
        {
            if (::setsockopt(fd_, PF_Q, Q_SO_TX_QUEUE_FLUSH, nullptr, 0) == -1)
                throw pfq_error(errno, "PFQ: TX queue flush");
        }
    };


    template <typename CharT, typename Traits>
    typename std::basic_ostream<CharT, Traits> &
    operator<<(std::basic_ostream<CharT,Traits> &out, const pfq_stats& rhs)
    {
        return out << rhs.recv << ' ' << rhs.lost << ' ' << rhs.drop << ' ' << rhs.sent << ' ' << rhs.disc;
    }

    inline pfq_stats&
    operator+=(pfq_stats &lhs, const pfq_stats &rhs)
    {
        lhs.recv += rhs.recv;
        lhs.lost += rhs.lost;
        lhs.drop += rhs.drop;

        lhs.sent += rhs.sent;
        lhs.disc += rhs.disc;

        return lhs;
    }

    inline pfq_stats&
    operator-=(pfq_stats &lhs, const pfq_stats &rhs)
    {
        lhs.recv -= rhs.recv;
        lhs.lost -= rhs.lost;
        lhs.drop -= rhs.drop;

        lhs.sent -= rhs.sent;
        lhs.disc -= rhs.disc;

        return lhs;
    }

    inline pfq_stats
    operator+(pfq_stats lhs, const pfq_stats &rhs)
    {
        lhs += rhs;
        return lhs;
    }

    inline pfq_stats
    operator-(pfq_stats lhs, const pfq_stats &rhs)
    {
        lhs -= rhs;
        return lhs;
    }

} // namespace net

