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

#pragma once

#include <linux/if_ether.h>
#include <linux/ip.h>
#include <linux/pf_q.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <poll.h>

#include <tuple>
#include <memory>
#include <vector>
#include <type_traits>
#include <algorithm>
#include <thread>
#include <chrono>

#include <pfq/util.hpp>
#include <pfq/queue.hpp>
#include <pfq/lang/lang.hpp>

namespace pfq {

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
        default_      = Q_CLASS_DEFAULT,
        user_plane    = Q_CLASS_USER_PLANE,
        control_plane = Q_CLASS_CONTROL_PLANE,
        control       = Q_CLASS_CONTROL,
        any           = Q_CLASS_ANY
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

    //! integer constants...
    //!

    static constexpr int any_device  = Q_ANY_DEVICE;
    static constexpr int any_queue   = Q_ANY_QUEUE;
    static constexpr int any_group   = Q_ANY_GROUP;
    static constexpr int no_kthread  = Q_NO_KTHREAD;

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
        struct tx_slots { size_t value; };

        using types = std::tuple<class_, policy, caplen, rx_slots, tx_slots>;

        inline
        types make_default()
        {
            return std::make_tuple(param::class_   {class_mask::default_},
                                   param::policy   {group_policy::priv},
                                   param::caplen   {64},
                                   param::rx_slots {1024},
                                   param::tx_slots {1024});
        }
    }

    //////////////////////////////////////////////////////////////////////

    //! PFQ: the socket
    /*!
     * This class is the main interface to the PFQ kernel module.
     * Each instance handles a socket that can be used to receive from and transmit
     * packets to the network.
     */

    class socket
    {
        struct pfq_data
        {
            int id;
            int gid;

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

            size_t tx_attempt;

            size_t tx_num_bind;
            size_t tx_num_async;
        };

        int fd_;
        int hd_;

        std::unique_ptr<pfq_data> data_;

    public:

        //! Default constructor

        socket()
        : fd_(-1)
        , hd_(-1)
        , data_()
        {}

        //! Constructor with named-parameter idiom (param::get is the C++14 std::get)

        template <typename ...Ts>
        socket(param::list_t, Ts&& ...args)
        : fd_(-1)
        , hd_(-1)
        , data_()
        {
            auto def = param::make_default();

            param::load(def, std::forward<Ts>(args)...);

            this->open(param::get<param::class_>(def).value,
                       param::get<param::policy>(def).value,
                       param::get<param::caplen>(def).value,
                       param::get<param::rx_slots>(def).value,
                       param::get<param::tx_slots>(def).value);
        }

        //! Constructor
        /*!
         * Create a socket and join a new private group.
         * The default values for class mask and group policy are class_mask::default_ and
         * group_policy::priv, respectively.
         */

        socket(size_t caplen, size_t rx_slots = 1024, size_t tx_slots = 1024)
        : fd_(-1)
        , hd_(-1)
        , data_()
        {
            this->open(class_mask::default_, group_policy::priv, caplen, rx_slots, tx_slots);
        }

        //! Constructor
        /*!
         * Create a socket with the given group policy.
         * The default class used is class_mask::default_.
         */

        socket(group_policy policy, size_t caplen, size_t rx_slots = 1024, size_t tx_slots = 1024)
        : fd_(-1)
        , hd_(-1)
        , data_()
        {
            this->open(class_mask::default_, policy, caplen, rx_slots, tx_slots);
        }

        //! Constructor
        /*!
         * Create a socket with the given class mask and group policy.
         * All the possible parameters are specifiable.
         */

        socket(class_mask mask, group_policy policy, size_t caplen, size_t rx_slots = 1024, size_t tx_slots = 1024)
        : fd_(-1)
        , hd_(-1)
        , data_()
        {
            this->open(mask, policy, caplen, rx_slots, tx_slots);
        }

        //! Destructor: close the socket

        ~socket()
        {
            try
            {
                this->close();
            }
            catch(std::exception &e)
            {
                std::cerr << "exception: " << e.what() << std::endl;
            }
        }

        //! PFQ socket is a non-copyable resource.

        socket(const socket&) = delete;

        //! PFQ socket is a non-copy assignable resource.

        socket& operator=(const socket&) = delete;


        //! Move constructor.

        socket(socket &&other) noexcept
        : fd_(other.fd_)
        , hd_(other.hd_)
        , data_(std::move(other.data_))
        {
            other.fd_ = -1;
            other.hd_ = -1;
        }

        //! Move assignment operator.

        socket &
        operator=(socket &&other) noexcept
        {
            if (this != &other)
            {
                data_     = std::move(other.data_);
                fd_       = other.fd_;
                hd_       = other.hd_;
                other.fd_ = -1;
                other.hd_ = -1;
            }
            return *this;
        }

        //! Swap two sockets.

        void
        swap(socket &other)
        {
            std::swap(data_, other.data_);
            std::swap(fd_,   other.fd_);
            std::swap(hd_,   other.hd_);
        }

        //! Open the socket with the given group policy.
        /*!
         * If the policy is not group_policy::undefined, also join a
         * new group with class_mask::default_ and the given policy.
         */

        void
        open(group_policy policy, size_t caplen, size_t rx_slots = 1024, size_t tx_slots = 1024)
        {
            this->open(caplen, rx_slots, tx_slots);

            if (policy != group_policy::undefined)
            {
                data()->gid = this->join_group(any_group, policy, class_mask::default_);
            }
        }

        //! Open the socket with the given class mask and group policy.
        /*!
         * If the policy is not group_policy::undefined, also join a
         * new group with the specified class mask and group policy.
         */

        void
        open(class_mask mask, group_policy policy, size_t caplen, size_t rx_slots = 1024, size_t tx_slots = 1024)
        {
            this->open(caplen, rx_slots, tx_slots);

            if (policy != group_policy::undefined)
            {
                data()->gid = this->join_group(any_group, policy, mask);
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
                       param::get<param::tx_slots>(def).value);
        }

        //! Return the id of the socket.

        int
        id() const
        {
            if (data_)
                return data_->id;
            return -1;
        }

        //! Return the group-id of the socket.

        int
        group_id() const
        {
            if (data_)
                return data_->gid;
            return -1;
        }

        //! Return the underlying file descriptor.

        int
        fd() const
        {
            return fd_;
        }


    private:

        pfq_data * data()
        {
            if (data_)
                return data_.get();
            throw pfq_error("PFQ: socket not open");
        }

        pfq_data const * data() const
        {
            if (data_)
                return data_.get();
            throw pfq_error("PFQ: socket not open");
        }

        void
        open(size_t caplen, size_t rx_slots, size_t tx_slots)
        {
            if (fd_ != -1)
                throw pfq_error("PFQ: socket already open");

            fd_ = ::socket(PF_Q, SOCK_RAW, htons(ETH_P_ALL));
            if (fd_ == -1)
                throw pfq_error("PFQ: module not loaded");

            // allocate pdata

            data_.reset(new pfq_data {  -1,
                                        -1,
                                        nullptr,
                                        0,
                                        nullptr,
                                        0,
                                        nullptr,
                                        0,
                                        0,
                                        0,
                                        0,
                                        0,
                                        0,
                                        0,
                                        0
                                     });

            // get id

            data_->id = PFQ_VERSION_CODE;
            socklen_t size = sizeof(data_->id);

            if (::getsockopt(fd_, PF_Q, Q_SO_GET_ID, &data_->id, &size) == -1)
                throw pfq_error(errno, "PFQ: get id error");

            // set Rx queue slots

            if (::setsockopt(fd_, PF_Q, Q_SO_SET_RX_SLOTS, &rx_slots, sizeof(rx_slots)) == -1)
                throw pfq_error(errno, "PFQ: set Rx slots error");

            data_->rx_slots = rx_slots;

            // set caplen
            //

            if (::setsockopt(fd_, PF_Q, Q_SO_SET_RX_CAPLEN, &caplen, sizeof(caplen)) == -1)
                throw pfq_error(errno, "PFQ: set Rx caplen error");

            data_->rx_slot_size = align<8>(sizeof(pfq_pkthdr) + caplen);

            // set Tx queue slots

            if (::setsockopt(fd_, PF_Q, Q_SO_SET_TX_SLOTS, &tx_slots, sizeof(tx_slots)) == -1)
                throw pfq_error(errno, "PFQ: set Tx slots error");


            // get maxlen

            int maxlen;
            size = sizeof(maxlen);

            if (::getsockopt(fd_, PF_Q, Q_SO_GET_TX_MAXLEN, &maxlen, &size) == -1)
                throw pfq_error(errno, "PFQ: get Tx maxlen error");

            data_->tx_slots = tx_slots;
            data_->tx_slot_size = align<8>(sizeof(pfq_pkthdr_tx) + maxlen);

        }

    public:

        //! Close the socket.

        void
        close()
        {
            if (fd_ != -1)
            {
                if (data_ && data_->shm_addr)
                    this->disable();

                data_.reset(nullptr);

                if (::close(fd_) < 0)
                    throw pfq_error("FPQ: close error");

                fd_ = -1;

                if (hd_ != -1)
                {
                    ::close(hd_);
                    hd_ = -1;
                }
            }
        }

        //! Enable the socket for packets capture and transmission.

        void
        enable()
        {
            size_t tot_mem; socklen_t size = sizeof(tot_mem);

            if (data()->shm_addr != MAP_FAILED &&
                data()->shm_addr != nullptr )
                throw pfq_error(errno, "PFQ: queue already enabled");

            if (::getsockopt(fd_, PF_Q, Q_SO_GET_SHMEM_SIZE, &tot_mem, &size) == -1)
                throw pfq_error(errno, "PFQ: queue memory error");

            auto env = getenv("PFQ_HUGEPAGES");
            auto hugepages = hugepages_mountpoint();

            if (!hugepages.empty() &&
                !getenv("PFQ_NO_HUGEPAGES") &&
                (env == nullptr || atoi(env) != 0))
            {
                // HugePages
                //
                std::clog << "[PFQ] using HugePages..." << std::endl;

                hd_ = ::open((hugepages + "/pfq." + std::to_string(data_->id)).c_str(),  O_CREAT | O_RDWR, 0755);
                if (hd_ == -1)
                    throw pfq_error(errno, "PFQ: couldn't open a HugePages descriptor");

                data()->shm_addr = ::mmap(nullptr, tot_mem, PROT_READ|PROT_WRITE, MAP_SHARED, hd_, 0);
                if (data()->shm_addr == MAP_FAILED)
                    throw pfq_error(errno, "PFQ: couldn't mmap HugePages");

                if(::setsockopt(fd_, PF_Q, Q_SO_ENABLE, &data()->shm_addr, sizeof(data()->shm_addr)) == -1)
                    throw pfq_error(errno, "PFQ: socket enable (HugePages)");
            }
            else
            {
                // standard pages (4K)
                //

                std::clog << "[PFQ] using 4k-Pages..." << std::endl;

                void * null = nullptr;
                if(::setsockopt(fd_, PF_Q, Q_SO_ENABLE, &null, sizeof(null)) == -1)
                    throw pfq_error(errno, "PFQ: socket enable");

                data()->shm_addr = ::mmap(nullptr, tot_mem, PROT_READ|PROT_WRITE, MAP_SHARED, fd_, 0);
                if (data()->shm_addr == MAP_FAILED)
                    throw pfq_error(errno, "PFQ: socket enable (memory map)");
            }

            data()->shm_size = tot_mem;

            data()->rx_queue_addr = static_cast<char *>(data()->shm_addr) + sizeof(pfq_shared_queue);
            data()->rx_queue_size = data()->rx_slots * data()->rx_slot_size;

            data()->tx_queue_addr = static_cast<char *>(data()->shm_addr) + sizeof(pfq_shared_queue) + data()->rx_queue_size * 2;
            data()->tx_queue_size = data()->tx_slots * data()->tx_slot_size;
        }

        //! Disable the socket.
        /*!
         * Release the shared memory, stop kernel threads.
         */

        void
        disable()
        {
            if (fd_ == -1)
                throw pfq_error("PFQ: socket not open");

            if (data()->shm_addr != MAP_FAILED)
            {
                if (::munmap(data()->shm_addr, data()->shm_size) == -1)
                    throw pfq_error(errno, "PFQ: munmap error");

                auto hugepages = hugepages_mountpoint();
                if (hd_ != -1) {
                    unlink((hugepages + "/pfq." + std::to_string(fd_)).c_str());
                }
            }

            data()->shm_addr = nullptr;
            data()->shm_size = 0;

            if(::setsockopt(fd_, PF_Q, Q_SO_DISABLE, nullptr, 0) == -1)
                throw pfq_error(errno, "PFQ: socket disable");
        }

        //! Check whether the socket capture is enabled.

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

        //! Check whether the timestamping for packets is enabled.

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

            data()->rx_slot_size = align<8>(sizeof(pfq_pkthdr) + value);
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

        //! Return the max transmission length of packets, in bytes.

        size_t
        maxlen() const
        {
           int ret; socklen_t size = sizeof(ret);
           if (::getsockopt(fd_, PF_Q, Q_SO_GET_TX_MAXLEN, &ret, &size) == -1)
                throw pfq_error(errno, "PFQ: get maxlen error");
           return ret;
        }

        //! Specify the length of the Rx queue, in number of packets.
        /*!
         * The number of Rx slots can't exceed the value specified by
         * the max_queue_slot kernel module parameter.
         */

        void
        rx_slots(size_t value)
        {
            if (enabled())
                throw pfq_error("PFQ: enabled (Rx slots could not be set)");

            if (::setsockopt(fd_, PF_Q, Q_SO_SET_RX_SLOTS, &value, sizeof(value)) == -1) {
                throw pfq_error(errno, "PFQ: set Rx slots error");
            }

            data()->rx_slots = value;
        }

        //! Return the length of the Rx queue, in number of packets.

        size_t
        rx_slots() const
        {
            return data()->rx_slots;
        }

        //! Return the length of a Rx slot, in bytes.

        size_t
        rx_slot_size() const
        {
            return data()->rx_slot_size;
        }

        //! Specify the length of the Tx queue, in number of packets.
        /*!
         * The number of Tx slots can't exceed the value specified by
         * the max_queue_slot kernel module parameter.
         */

        void
        tx_slots(size_t value)
        {
            if (enabled())
                throw pfq_error("PFQ: enabled (Tx slots could not be set)");

            if (::setsockopt(fd_, PF_Q, Q_SO_SET_TX_SLOTS, &value, sizeof(value)) == -1) {
                throw pfq_error(errno, "PFQ: set Tx slots error");
            }

            data()->tx_slots = value;
        }

        //! Return the length of the Tx queue, in number of packets.

        size_t
        tx_slots() const
        {
           return data()->tx_slots;
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
            auto index = [this, dev]() -> int {
                if (strcmp(dev, "any") == 0)
                    return any_device;
                auto n = ifindex(this->fd(), dev);
                if (n == -1)
                    throw pfq_error("PFQ: bind_group: device not found");
                return n;
            }();

            struct pfq_binding b = { {gid}, index, queue };

            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_BIND, &b, sizeof(b)) == -1)
                throw pfq_error(errno, "PFQ: bind error");
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
            auto index = [this, dev]() -> int {
                if (strcmp(dev, "any") == 0)
                    return any_device;
                auto n = ifindex(this->fd(), dev);
                if (n == -1)
                    throw pfq_error("PFQ: unbind_group: device not found");
                return n;
            }();

            struct pfq_binding b = { {gid}, index, queue };

            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_UNBIND, &b, sizeof(b)) == -1)
                throw pfq_error(errno, "PFQ: unbind error");
        }

        //! Set the socket as egress and bind it to the given device/queue.
        /*!
         * The egress socket will be used within the capture groups as forwarder.
         */

        void
        egress_bind(const char *dev, int queue = any_queue)
        {
            auto index = [this, dev]() -> int {
                if (strcmp(dev, "any") == 0)
                    return any_device;
                auto n = ifindex(this->fd(), dev);
                if (n == -1)
                    throw pfq_error("PFQ: egress_bind: device not found");
                return n;
            }();

            struct pfq_binding b = { {0}, index, queue };

            if (::setsockopt(fd_, PF_Q, Q_SO_EGRESS_BIND, &b, sizeof(b)) == -1)
                throw pfq_error(errno, "PFQ: egress bind error");

        }

        //! Unset the socket as egress.

        void
        egress_unbind()
        {
            if (::setsockopt(fd_, PF_Q, Q_SO_EGRESS_UNBIND, 0, 0) == -1)
                throw pfq_error(errno, "PFQ: egress unbind error");
        }


        //! Bind the socket for transmission to the given device name and queue.
        /*!
         *  A socket can be bound up to a maximum number of queues.
         *  The core parameter specifies the CPU index where to run a
         *  kernel thread (unless no_kthread id is specified).
         */

        void
        bind_tx(const char *dev, int queue = any_queue, int core = no_kthread)
        {
            auto index = ifindex(this->fd(), dev);
            if (index == -1)
                throw pfq_error("PFQ: device not found");

            struct pfq_binding b = { {core}, index, queue };

            if (::setsockopt(fd_, PF_Q, Q_SO_TX_BIND, &b, sizeof(b)) == -1)
                throw pfq_error(errno, "PFQ: Tx bind error");

            if (core != no_kthread)
                data()->tx_num_async++;

            data()->tx_num_bind++;
        }

        //! Unbind the socket transmission.
        /*!
         * Unbind the socket for transmission from any device/queue.
         */

        void
        unbind_tx()
        {
            if (::setsockopt(fd_, PF_Q, Q_SO_TX_UNBIND, nullptr, 0) == -1)
                throw pfq_error(errno, "PFQ: Tx unbind error");

            data()->tx_num_async = 0;
            data()->tx_num_bind = 0;
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
         * The functional computation is specified as a PFQ/lang expression.
         */

        template <typename Comp>
        void set_group_computation(int gid, Comp const &comp)
        {
            struct free_deleter
            {
                void operator()(void *a) const { ::free(a); }
            };

            auto ser = pfq::lang::serialize(comp, 0).first;

            std::unique_ptr<pfq_computation_descr, free_deleter> prg (
                reinterpret_cast<pfq_computation_descr *>(::malloc(sizeof(size_t) * 2 + sizeof(pfq_functional_descr) * ser.size())));

            prg->size = ser.size();
            prg->entry_point = 0;

            int n = 0;

            for(auto & descr : ser)
            {
                prg->fun[n].symbol = descr.symbol.c_str();

                for(size_t i = 0; i < sizeof(prg->fun[0].arg)/sizeof(prg->fun[0].arg[0]); i++)
                {
                    prg->fun[n].arg[i].addr  = descr.arg[i].ptr ? descr.arg[i].ptr->forall_addr() : nullptr;
                    prg->fun[n].arg[i].size  = descr.arg[i].size;
                    prg->fun[n].arg[i].nelem = descr.arg[i].nelem;
                }

                prg->fun[n].next  = descr.link;

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
         * This function is limited to simple PFQ/lang functional computations.
         * Only the composition of monadic functions without arguments are supported.
         */

        void
        set_group_computation(int gid, std::string prog)
        {
            std::vector<pfq::lang::MFunction<>> comp;
            auto fs = split(prog, ">->");

            for (auto & f : fs)
            {
                comp.push_back(pfq::lang::mfunction(trim(f)));
            }

            set_group_computation(gid, comp);
        }


        //! Specify a BPF program for the given group.
        /*!
         * This function can be used to set a specific BPF filter for the group.
         * It is used by the PFQ/pcap library.
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

            struct pfq_group_join group { gid, static_cast<int16_t>(pol), static_cast<unsigned long>(mask) };

            socklen_t size = sizeof(group);

            if (::getsockopt(fd_, PF_Q, Q_SO_GROUP_JOIN, &group, &size) == -1)
                throw pfq_error(errno, "PFQ: join group error");

            if (data()->gid == -1)
                data()->gid = group.gid;

            return group.gid;
        }

        //! Leave the group specified by the group id.

        void
        leave_group(int gid)
        {
            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_LEAVE, &gid, sizeof(gid)) == -1)
                throw pfq_error(errno, "PFQ: leave group error");

            if (data()->gid == gid)
                data()->gid = -1;
        }

        //! Wait for packets.
        /*!
         * Wait for packets available to read. A timeout in microseconds can be specified.
         */

        int
        poll(long int microseconds = -1 /* infinite */)
        {
            struct timespec timeout;
            struct pollfd fd = {fd_, POLLIN, 0 };

            if (fd_ == -1)
                throw pfq_error("PFQ: socket not open");

            if (microseconds >= 0) {
                timeout.tv_sec  = microseconds / 1000000;
                timeout.tv_nsec = (microseconds % 1000000) * 1000;
            }

            int ret = ::ppoll(&fd, 1, microseconds < 0 ? nullptr : &timeout, nullptr);
            if (ret < 0 && errno != EINTR)
               throw pfq_error(errno, "PFQ: ppoll error");

            return 0;
        }

        //! Read packets in place.
        /*!
         * Wait for packets and return a queue descriptor.
         * Packets are stored in the memory mapped queue of the socket.
         * The timeout is specified in microseconds.
         */

        queue
        read(long int microseconds = -1)
        {
            if (!data()->shm_addr)
                throw pfq_error("PFQ: read: socket not enabled");

            auto q = static_cast<struct pfq_shared_queue *>(data()->shm_addr);
            unsigned int data, index;

            data = __atomic_load_n(&q->rx.data, __ATOMIC_RELAXED);
            index = Q_SHARED_QUEUE_INDEX(data);

            // at wrap-around reset Rx slots...
            //

            if (((index+1) & 0xfe)== 0)
            {
                auto raw = static_cast<char *>(data_->rx_queue_addr) + ((index+1) & 1) * data_->rx_queue_size;
                auto end = raw + data_->rx_queue_size;
                const int rst = index & 1;
                for(; raw < end; raw += data_->rx_slot_size)
                    reinterpret_cast<pfq_pkthdr *>(raw)->commit = rst;
            }

            if (Q_SHARED_QUEUE_LEN(data) == 0)
            {
#ifdef PFQ_USE_POLL
                this->poll(microseconds);
#else
                (void)microseconds;
                return queue();
#endif
            }

            // swap the queue...
            //

            data = __atomic_exchange_n(&q->rx.data, (unsigned int)((index+1) << 24), __ATOMIC_RELAXED);

            auto queue_len = std::min(static_cast<size_t>(Q_SHARED_QUEUE_LEN(data)), data_->rx_slots);

            return queue(static_cast<char *>(data_->rx_queue_addr) + (index & 1) * data_->rx_queue_size,
                         data_->rx_slot_size, queue_len, index);
        }

        //! Return the current commit version (used internally by the memory mapped queue).

        uint8_t
        current_commit() const
        {
            auto q = static_cast<struct pfq_shared_queue *>(data_->shm_addr);
            return Q_SHARED_QUEUE_INDEX(q->rx.data);
        }

        //! Receive packets in the given mutable buffer.
        /*!
         * Wait for packets and return a queue descriptor.
         * Packets are stored in the given mutable buffer.
         * It is possible to specify a timeout in microseconds.
         */

        queue
        recv(const mutable_buffer &buff, long int microseconds = -1)
        {
            if (fd_ == -1)
                throw pfq_error("PFQ: socket not open");

            auto this_queue = this->read(microseconds);

            if (buff.second < data_->rx_slots * data_->rx_slot_size)
                throw pfq_error("PFQ: buffer too small");

            memcpy(buff.first, this_queue.data(), this_queue.slot_size() * this_queue.size());
            return queue(buff.first, this_queue.slot_size(), this_queue.size(), this_queue.index());
        }


        //! Collect and process packets.

        /*! The function takes an instance of a callable type.
         * The object must have the following callable signature:
         *
         * typedef void (*pfq_handler)(char *user, const struct pfq_pkthdr *h, const char *data);
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

        //! Return the socket statistics.

        pfq_stats
        stats() const
        {
            pfq_stats stat;
            socklen_t size = sizeof(struct pfq_stats);
            if (::getsockopt(fd_, PF_Q, Q_SO_GET_STATS, &stat, &size) == -1)
                throw pfq_error(errno, "PFQ: get stats error");
            return stat;
        }

        //! Return the statistics of the given group.

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

        //! Return the set of counters of the given group.

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

        //! Return the memory size of the Rx queue.

        size_t
        mem_size() const
        {
            return data()->shm_size;
        }

        //! Return the address of the Rx queue.

        const void *
        mem_addr() const
        {
            return data()->shm_addr;
        }


        //! Store the packet and transmit the packets in the queue.
        /*!
         * The queue is flushed (if required) and the transmission takes place.
         */

        bool
        send(const_buffer pkt)
        {
            auto rc = inject(pkt, 0);
            if (data_->tx_num_bind != data_->tx_num_async)
                tx_queue_flush();
            return rc;
        }


        //! Store the packet and transmit the packets in the queue, asynchronously.
        /*!
         * The transmission is invoked every @flush_hint packets.
         * When kernel threads are in use, @flush_hint is ignored.
         */

        bool
        send_async(const_buffer pkt, size_t flush_hint = 1)
        {
            auto rc = inject(pkt, 0);

            if (++data_->tx_attempt == flush_hint) {

                data_->tx_attempt = 0;

                if (data_->tx_num_bind != data_->tx_num_async)
                    tx_queue_flush(any_queue);
            }

            return rc;
        }

        /*! Store the packet and transmit it. */
        /*!
         * The transmission takes place at the given timespec time, specified
         * as a generic chrono::time_point.
         */

        template <typename Clock, typename Duration>
        bool
        send_at(const_buffer pkt, std::chrono::time_point<Clock, Duration> const &tp)
        {
            if (data_->tx_num_bind != data_->tx_num_async)
                throw std::runtime_error("PFQ: send_at not fully async!");

            auto ns = std::chrono::duration_cast<std::chrono::nanoseconds>(tp.time_since_epoch()).count();
            return inject(pkt, ns);
        }

        //! Schedule the packet for transmission.
        /*!
         * The packet is copied into a Tx queue (using a TSS symmetric hash if any_queue is specified)
         * and transmitted at the given timestamp by a kernel thread or when tx_queue_flush is called.
         * A timestamp of 0 nanoseconds means 'immediate transmission'.
         */

        bool
        inject(const_buffer buf, uint64_t nsec, int queue = any_queue)
        {
            if (!data_->shm_addr)
                throw pfq_error("PFQ: inject: socket not enabled");

            const int tss = fold(queue == any_queue ? symmetric_hash(buf.first) : queue,
                                 data_->tx_num_bind);

            auto tx = &static_cast<struct pfq_shared_queue *>(data_->shm_addr)->tx[tss];

            auto index = __atomic_load_n(&tx->cons, __ATOMIC_RELAXED);
            if (index != __atomic_load_n(&tx->prod, __ATOMIC_RELAXED))
            {
                __atomic_store_n(&tx->prod, index, __ATOMIC_RELAXED);
            }

            // get base address of the soft Tx queue:
            //
	        void * base_addr = static_cast<char *>(data_->tx_queue_addr)
	                            + data_->tx_queue_size * (2 * tss + (index & 1));

            if (index != tx->index)
            {
                    tx->index = index;
                    tx->ptr = base_addr;
            }

            // cut the packet to maxlen:
            //

            auto len = std::min(buf.second, data_->tx_slot_size - sizeof(struct pfq_pkthdr_tx));

            // compute the current slot_size:
            //
            auto slot_size = sizeof(struct pfq_pkthdr_tx) + align<8>(len);

            // ensure there's space enough for the current slot_size + the next header:
            //
            if ((static_cast<char *>(tx->ptr) - static_cast<char *>(base_addr) + slot_size + sizeof(struct pfq_pkthdr_tx))
                    < data_->tx_queue_size)
            {
                auto hdr = (struct pfq_pkthdr_tx *)tx->ptr;
                hdr->len = len;
                hdr->nsec = nsec;
                memcpy(hdr+1, buf.first, len);

                reinterpret_cast<char *&>(tx->ptr) += slot_size;
                static_cast<struct pfq_pkthdr_tx *>(tx->ptr)->len = 0;

                return true;
            }

            return false;
        }

        //! Flush the Tx queue(s).
        /*!
         * Transmit the packets in the Tx queues of the socket.
         */

        void
        tx_queue_flush(int queue = any_queue)
        {
            if (::setsockopt(fd_, PF_Q, Q_SO_TX_FLUSH, &queue, sizeof(queue)) == -1)
                throw pfq_error(errno, "PFQ: Tx queue flush");
        }

        //! Start kernel threads.
        /*!
         * Start kernel threads associated with Tx queues.
         */

        void
        tx_async_start()
        {
            if (::setsockopt(fd_, PF_Q, Q_SO_TX_ASYNC_START, nullptr, 0) == -1)
                throw pfq_error(errno, "PFQ: Tx async start");
        }

        //! Stop kernel threads.
        /*!
         * Stop kernel threads associated with Tx queues.
         */

        void
        tx_async_stop()
        {
            if (::setsockopt(fd_, PF_Q, Q_SO_TX_ASYNC_STOP, nullptr, 0) == -1)
                throw pfq_error(errno, "PFQ: Tx async stop");
        }

    };


    template <typename CharT, typename Traits>
    typename std::basic_ostream<CharT, Traits> &
    operator<<(std::basic_ostream<CharT,Traits> &out, const pfq_stats& rhs)
    {
        return out << rhs.recv << ' ' << rhs.lost << ' ' << rhs.drop << ' ' << rhs.sent << ' ' << rhs.disc << ' ' << rhs.frwd << ' ' << rhs.kern;
    }

    inline pfq_stats&
    operator+=(pfq_stats &lhs, const pfq_stats &rhs)
    {
        lhs.recv += rhs.recv;
        lhs.lost += rhs.lost;
        lhs.drop += rhs.drop;

        lhs.sent += rhs.sent;
        lhs.disc += rhs.disc;

        lhs.frwd += rhs.frwd;
        lhs.kern += rhs.kern;

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

        lhs.frwd -= rhs.frwd;
        lhs.kern -= rhs.kern;

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

} // namespace pfq

