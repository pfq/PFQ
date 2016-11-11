/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#include <cstddef>
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

#include <pfq/pfq-int.h>
#include <pfq/pfq.h>


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

        using types = std::tuple<caplen, rx_slots, tx_slots, policy, class_>;

        inline
        types make_default()
        {
            return std::make_tuple(param::caplen   {1514},
                                   param::rx_slots {1024},
                                   param::tx_slots {1024},
                                   param::policy   {group_policy::priv},
                                   param::class_   {class_mask::default_}
                                   );
        }
    }

    //////////////////////////////////////////////////////////////////////

    constexpr const int version_code  = PFQ_VERSION_CODE;
    constexpr const int major_version = PFQ_MAJOR(PFQ_VERSION_CODE);
    constexpr const int minor_version = PFQ_MINOR(PFQ_VERSION_CODE);
    constexpr const int patchlevel_version = PFQ_PATCHLEVEL(PFQ_VERSION_CODE);
    constexpr const char * string_version  = PFQ_VERSION_STRING;

    //! PFQ: the socket
    /*!
     * This class is the main interface to the PFQ kernel module.
     * Each instance handles a socket that can be used to receive from and transmit
     * packets to the network.
     */

    class socket
    {
        std::unique_ptr<pfq_data_int> data_;

    public:

        //! Default constructor

        socket()
        : data_()
        { }

        //! Constructor with named-parameter idiom (make use of C++14 std::get)

        template <typename ...Ts>
        socket(param::list_t, Ts&& ...args)
        : data_()
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
        : data_()
        {
            this->open(class_mask::default_, group_policy::priv, caplen, rx_slots, tx_slots);
        }

        //! Constructor
        /*!
         * Create a socket with the given group policy.
         * The default class used is class_mask::default_.
         */

        socket(group_policy policy, size_t caplen, size_t rx_slots = 1024, size_t tx_slots = 1024)
        : data_()
        {
            this->open(class_mask::default_, policy, caplen, rx_slots, tx_slots);
        }

        //! Constructor
        /*!
         * Create a socket with the given class mask and group policy.
         * All the possible parameters are specifiable.
         */

        socket(class_mask mask, group_policy policy, size_t caplen, size_t rx_slots = 1024, size_t tx_slots = 1024)
        : data_()
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

        //! PFQ socket is not copyable.

        socket(const socket&) = delete;

        //! PFQ socket is not copy-assignable.

        socket& operator=(const socket&) = delete;


        //! Move constructor.

        socket(socket &&other) noexcept
        : data_(std::move(other.data_))
        {
        }

        //! Move assignment operator.

        socket &
        operator=(socket &&other) noexcept
        {
            if (this != &other)
            {
                data_ = std::move(other.data_);
            }
            return *this;
        }

        //! Swap two sockets.

        void
        swap(socket &other)
        {
            std::swap(data_, other.data_);
        }

        //! Open the socket with the given group policy.
        /*!
         * If the policy is not group_policy::undefined, also join a
         * new group with class_mask::default_ and the given policy.
         */

        void
        open(group_policy policy, size_t caplen, size_t rx_slots = 1024, size_t tx_slots = 1024)
        {
            this->open(class_mask::default_, policy, caplen, rx_slots, tx_slots);
        }

        //! Open the socket with the given class mask and group policy.
        /*!
         * If the policy is not group_policy::undefined, also join a
         * new group with the specified class mask and group policy.
         */

        void
        open(class_mask mask, group_policy policy, size_t caplen, size_t rx_slots = 1024, size_t tx_slots = 1024)
        {
            if (data_)
                throw system_error("PFQ: socket already open");

            auto ptr = pfq_open_group(static_cast<unsigned long>(mask),
                                      static_cast<int>(policy),
                                      caplen,
                                      rx_slots,
                                      tx_slots);

            if (!ptr)
                throw system_error(errno, pfq_error(NULL));

            data_.reset(ptr);
        }


        //! Open the socket with the named-parameter idiom.
        /*!
         * Unspecified parameters are set to default values as
         * in param::make_default();
         */

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
            if (data_)
                return data_->fd;
            return -1;
        }


    private:

        template <typename T, typename Ret>
        static T as(pfq_data_int const *q, Ret value)
        {
            if (value < 0)
                throw system_error(errno, pfq_error(q));
            return static_cast<T>(value);
        }

        template <typename Ret>
        static void throw_if(pfq_data_int const *q, Ret value)
        {
            if (value < 0)
                throw system_error(errno, pfq_error(q));
        }

        pfq_data_int * data()
        {
            if (data_)
                return data_.get();
            throw system_error("PFQ: socket not open");
        }

        pfq_data_int const * data() const
        {
            if (data_)
                return data_.get();
            throw system_error("PFQ: socket not open");
        }

    public:

        //! Close the socket.
        /*!
         * Release the shared memory, stop kernel threads.
         */

        void
        close()
        {
            if (auto q = data_.release())
            {
                throw_if(q, pfq_close(q));
            }
        }

        //! Enable the socket for packets capture and transmission.
        /*!
         * Allocate the shared memory for socket queues possibly using
         * the Linux HugePages support.
         * If the enviroment variable PFQ_HUGEPAGES is set to 0 (or
         * PFQ_NO_HUGEPAGES is defined) standard 4K pages are used.
         */

        void
        enable()
        {
            auto q = this->data();
            throw_if(q, pfq_enable(q));
        }

        //! Disable the socket.
        /*!
         * Release the shared memory, stop kernel threads.
         */

        void
        disable()
        {
            auto q = this->data();
            throw_if(q, pfq_disable(q));
        }

        //! Check whether the socket capture is enabled.

        bool
        is_enabled() const
        {
            if (auto q = data_.get())
            {
                return as<bool>(q, pfq_is_enabled(q));
            }
            return false;
        }

        //! Enable/disable timestamping for packets.

        void
        timestamping_enable(bool value)
        {
            auto q = this->data();
            throw_if(q, pfq_timestamping_enable(q, value));
        }

        //! Check whether timestamping for packets is enabled.

        bool
        is_timestamping_enabled() const
        {
            auto q = this->data();
            return as<bool>(q, pfq_is_timestamping_enabled(q));
        }

        //! Set the weight of the socket for the steering phase.

        void
        weight(int w)
        {
            auto q = this->data();
            throw_if(q, pfq_set_weight(q, w));
        }


        //! Return the weight of the socket.

        int
        weight() const
        {
            auto q = this->data();
            return as<int>(q, pfq_get_weight(q));
        }


        //! Specify the capture length of packets, in bytes.
        /*!
         * Capture length must be set before the socket is enabled.
         */

        void
        caplen(size_t value)
        {
            auto q = this->data();
            throw_if(q, pfq_set_caplen(q, value));
        }

        //! Return the capture length of packets, in bytes.

        size_t
        caplen() const
        {
            auto q = this->data();
            return as<size_t>(q, pfq_get_caplen(q));
        }

        //! Return the max transmission length of packets, in bytes.

        size_t
        maxlen() const
        {
            auto q = this->data();
            return as<size_t>(q, pfq_get_maxlen(q));
        }

        //! Specify the length of the Rx queue, in number of packets.

        void
        rx_slots(size_t value)
        {
            auto q = this->data();
            throw_if(q, pfq_set_rx_slots(q, value));
        }

        //! Return the length of the Rx queue, in number of packets.

        size_t
        rx_slots() const
        {
            auto q = this->data();
            return as<size_t>(q, pfq_get_rx_slots(q));
        }

        //! Return the length of a Rx slot, in bytes.

        size_t
        rx_slot_size() const
        {
            return data()->rx_slot_size;
        }

        //! Specify the length of the Tx queue, in number of packets.

        void
        tx_slots(size_t value)
        {
            auto q = this->data();
            throw_if(q, pfq_set_tx_slots(q, value));
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
         * the second argument is the queue index or 'any_queue'.
         */

        void
        bind(const char *dev, int queue = any_queue)
        {
            auto q = this->data();
            throw_if(q, pfq_bind(q, dev, queue));
        }

        //! Unbind the main group of the socket from the given device/queue.

        void
        unbind(const char *dev, int queue = any_queue)
        {
            auto q = this->data();
            throw_if(q, pfq_unbind(q, dev, queue));
        }

        //! Bind the group to the given device/queue.
        /*!
         * The first argument is the group id.
         * The second argument is the name of the device;
         * the third argument is the queue number or 'any_queue'.
         */

        void
        bind_group(int gid, const char *dev, int queue = any_queue)
        {
            auto q = this->data();
            throw_if(q, pfq_bind_group(q, gid, dev, queue));
        }

        //! Unbind the group from the given device/queue.

        void
        unbind_group(int gid, const char *dev, int queue = any_queue)
        {
            auto q = this->data();
            throw_if(q, pfq_unbind_group(q, gid, dev, queue));
        }

        //! Set the socket as egress and bind it to the given device/queue.
        /*!
         * The egress socket is be used by groups as network forwarder.
         */

        void
        egress_bind(const char *dev, int queue = any_queue)
        {
            auto q = this->data();
            throw_if(q, pfq_egress_bind(q, dev, queue));
        }

        //! Unset the socket as egress.

        void
        egress_unbind()
        {
            auto q = this->data();
            throw_if(q, pfq_egress_unbind(q));
        }


        //! Bind the socket for transmission to the given device name and queue.
        /*!
         *  The tid parameter specifies the index (id) of the transmitter
         *  thread. If 'no_kthread' specified, bind refers to synchronous
         *  transmissions.
         */

        void
        bind_tx(const char *dev, int queue = any_queue, int tid = no_kthread)
        {
            auto q = this->data();
            throw_if(q, pfq_bind_tx(q, dev, queue, tid));
        }

        //! Unbind the socket transmission.
        /*!
         * Unbind the socket for transmission from any device/queue.
         */

        void
        unbind_tx()
        {
            auto q = this->data();
            throw_if(q, pfq_unbind_tx(q));
        }

        //! Join the group specified by the group id.
        /*!
         * If the policy is not specified, group_policy::shared is used by default.
         * If the class mask is not specified, class_mask::default_ is used by default.
         */

        int
        join_group(int gid,
                   group_policy policy = group_policy::shared,
                   class_mask mask = class_mask::default_)
        {
            auto q = this->data();
            return as<int>(q, pfq_join_group(q, gid,
                                             static_cast<unsigned long>(mask),
                                             static_cast<int>(policy)));
        }

        //! Leave the group specified by the group id.

        void
        leave_group(int gid)
        {
            auto q = this->data();
            throw_if(q, pfq_leave_group(q, gid));
        }


        //! Return the mask of the joined groups.
        /*!
         * Each socket can bind to multiple groups. Each bit set in the mask represents
         * a joined group.
         */

        unsigned long
        groups_mask() const
        {
            unsigned long mask;
            auto q = this->data();
            throw_if(q, pfq_groups_mask(q, &mask));
            return mask;
        }

        //! Obtain the list of gid of joined groups.

        std::vector<int>
        groups() const
        {
            std::vector<int> vec;
            auto grps = this->groups_mask();
            for(int n = 0; grps != 0; n++)
            {
                if (grps & (1UL << n)) {
                    vec.push_back(n);
                    grps &= ~(1UL << n);
                }
            }

            return vec;
        }

        //! Specify a functional computation for the given group.
        /*!
         * The functional computation is specified as a pfq-lang expression.
         */

        template <typename Comp>
        void set_group_computation(int gid, Comp const &comp)
        {
            auto ser = pfq::lang::serialize(comp, 0).first;

            std::unique_ptr<pfq_lang_computation_descr, decltype(free) *> prg (
                reinterpret_cast<pfq_lang_computation_descr *>(::malloc(sizeof(size_t) * 2 + sizeof(pfq_lang_functional_descr) * ser.size())),
                free);

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
         * The functional computation is specified by a pfq_lang_computation_descriptor.
         * This function should not be used; use the pfq-lang eDSL instead.
         */

        void
        set_group_computation(int gid, pfq_lang_computation_descr const *prog)
        {
            auto q = this->data();
            throw_if(q, pfq_set_group_computation(q, gid, prog));
        }

        //! Specify a functional computation for the given group, as pfq-lang program from string.

        void
        set_group_computation(int gid, std::string const &comp)
        {
            auto q = this->data();
            throw_if(q, pfq_set_group_computation_from_string(q, gid, comp.c_str()));
        }

        //! Specify a functional computation for the given group, as pfq-lang program from file.

        void
        set_group_computation(int gid, const char *filepath)
        {
            auto q = this->data();
            throw_if(q, pfq_set_group_computation_from_file(q, gid, filepath));
        }

        //! Specify a functional computation for the given group, from JSON description.

        void
        set_group_computation_json(int gid, std::string const &comp)
        {
            auto q = this->data();
            throw_if(q, pfq_set_group_computation_from_json(q, gid, comp.c_str()));
        }


        //! Specify a BPF program for the given group.
        /*!
         * This function can be used to set a specific BPF filter for the group.
         * It is used by the PFQ/pcap library.
         */

        void
        set_group_fprog(int gid, const sock_fprog &f)
        {
            auto q = this->data();
            throw_if(q, pfq_group_fprog(q, gid, &f));
        }

        //! Reset the BPF program fro the given group.

        void
        reset_group_fprog(int gid)
        {
            auto q = this->data();
            throw_if(q, pfq_group_fprog_reset(q, gid));
        }


        //! Wait for packets.
        /*!
         * Wait for packets available for reading. A timeout in microseconds can be specified.
         */

        int
        poll(long int microseconds = -1 /* infinite */)
        {
            struct timespec timeout;
            struct pollfd fd = {data()->fd, POLLIN, 0 };

            if (microseconds >= 0) {
                timeout.tv_sec  = microseconds / 1000000;
                timeout.tv_nsec = (microseconds % 1000000) * 1000;
            }

            int ret = ::ppoll(&fd, 1, microseconds < 0 ? nullptr : &timeout, nullptr);
            if (ret < 0 && errno != EINTR)
               throw system_error(errno, "PFQ: ppoll error");

            return 0;
        }

        //! Read packets in place.
        /*!
         * Wait for packets and return a 'queue' descriptor, which contains
         * the references to packets.
         *
         * The memory of the socket queue is reset at the next read.
         * A timeout is specified in microseconds.
         */

        net_queue
        read(long int microseconds = -1)
        {
            if (!data()->shm_addr)
                throw system_error("PFQ: read: socket not enabled");

            auto q = static_cast<struct pfq_shared_queue *>(data()->shm_addr);
            unsigned long int data, qver;

            data = __atomic_load_n(&q->rx.shinfo, __ATOMIC_RELAXED);
            qver = PFQ_SHARED_QUEUE_VER(data);

            // forward packets first... 

            if (data_->tx_forward)
            {
                data_->tx_forward = 0;
                this->transmit_queue(0);
            }

            // at wrap-around reset Rx slots...
            //

            if (((qver+1) & (PFQ_SHARED_QUEUE_VER_MASK^1))== 0)
            {
                auto raw = static_cast<char *>(data_->rx_queue_addr) + ((qver+1) & 1) * data_->rx_queue_size;
                auto end = raw + data_->rx_queue_size;
                const pfq_ver_t rst = qver & 1;
                for(; raw < end; raw += data_->rx_slot_size)
                    reinterpret_cast<pfq_pkthdr *>(raw)->info.commit = rst;
            }

            // swap the net_queue...
            //

            data = __atomic_exchange_n(&q->rx.shinfo, ((qver+1) << (PFQ_SHARED_QUEUE_LEN_SIZE<<3)), __ATOMIC_RELAXED);

            if (PFQ_SHARED_QUEUE_LEN(data) == 0)
            {
#ifdef PFQ_USE_POLL
                this->poll(microseconds);
#else
                (void)microseconds;
                return net_queue();
#endif
            }

            auto queue_len = std::min(static_cast<size_t>(PFQ_SHARED_QUEUE_LEN(data)), data_->rx_slots);

            return net_queue(static_cast<char *>(data_->rx_queue_addr) + (qver & 1) * data_->rx_queue_size,
                         data_->rx_slot_size, queue_len, qver);
        }

        //! Return the current commit version (used internally by the memory mapped queue).

        pfq_ver_t
        current_commit() const
        {
            auto q = static_cast<struct pfq_shared_queue *>(data_->shm_addr);
            auto data = __atomic_load_n(&q->rx.shinfo, __ATOMIC_RELAXED);
            return static_cast<pfq_ver_t>(PFQ_SHARED_QUEUE_VER(data));
        }

        //! Receive packets in the given buffer.
        /*!
         * Wait for packets and return a queue descriptor.
         *
         * Packets are stored in the given buffer.
         * It is possible to specify a timeout in microseconds.
         */

        net_queue
        recv(const mutable_buffer &buff, long int microseconds = -1)
        {
            if (data_->fd == -1)
                throw system_error("PFQ: socket not open");

            auto this_queue = this->read(microseconds);

            if (buff.second < data_->rx_slots * data_->rx_slot_size)
                throw system_error("PFQ: buffer too small");

            memcpy(buff.first, this_queue.data(), this_queue.slot_size() * this_queue.size());
            return net_queue(buff.first, this_queue.slot_size(), this_queue.size(), this_queue.index());
        }


        //! Collect and process packets.

        /*! The function takes an instance of a callable type.
         * Such a type must have the following callable signature:
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

        //! Enable/disable vlan filtering for the given group.

        void vlan_filters_enable(int gid, bool toggle)
        {
            auto q = this->data();
            throw_if(q, pfq_vlan_filters_enable(q, gid, toggle));
        }

        //! Specify a capture vlan filter for the given group.
        /*!
         *  In addition to standard vlan ids, valid ids are also vlan_id::untag and vlan_id::anytag.
         */

        void vlan_set_filter(int gid, int vid)
        {
            auto q = this->data();
            throw_if(q, pfq_vlan_set_filter(q, gid, vid));
        }

        //! Specify the vlan capture filters in the given range.

        template <typename Iter>
        void vlan_set_filter(int gid, Iter beg, Iter end)
        {
            std::for_each(beg, end, [&](int vid) {
                vlan_set_filter(gid, vid);
            });
        }

        //! Reset the vlan filter for the given group.

        void vlan_reset_filter(int gid, int vid)
        {
            auto q = this->data();
            throw_if(q, pfq_vlan_reset_filter(q, gid, vid));
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
            auto q = this->data();
            throw_if(q, pfq_get_stats(q, &stat));
            return stat;
        }

        //! Return the statistics of the given group.

        pfq_stats
        group_stats(int gid) const
        {
            pfq_stats stat;
            auto q = this->data();
            throw_if(q, pfq_get_group_stats(q, gid, &stat));
            return stat;
        }

        //! Return the set of counters of the given group.

        std::vector<unsigned long>
        group_counters(int gid) const
        {
            pfq_counters cs;
            auto q = this->data();
            throw_if(q, pfq_get_group_counters(q, gid, &cs));
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
         * The queue is flushed every fhint packets.
         * Requires the socket is bound for transmission to a net device and queue.
         * See 'bind_tx'.
         */

        bool
        send(const_buffer pkt, size_t fhint = 1, unsigned int copies = 1)
        {
            auto ret = send_raw(pkt, 0, 0, 0, copies, false);
            if (++data_->tx_attempt == fhint)
            {
                data_->tx_attempt = 0;
                this->transmit_queue(0);
            }
            return ret;
        }

        //! Store the packet and transmit the packets in the queue.
        /*!
         * The queue is flushed every fhint packets.
         */

        bool
        send_to(const_buffer pkt, int ifindex, int qindex, size_t fhint = 1, unsigned int copies = 1)
        {
            auto ret = send_raw(pkt, ifindex, qindex, 0, copies, false);
            if (++data_->tx_attempt == fhint)
            {
                data_->tx_attempt = 0;
                this->transmit_queue(0);
            }
            return ret;
        }

        //! Transmit the packet asynchronously.
        /*!
         * The transmission is handled by PFQ kernel threads.
         * Requires the socket is bound for transmission to one (or multiple) PFQ kernel threads.
         * See 'bind_tx'.
         */

        bool
        send_async(const_buffer pkt, unsigned int copies = 1)
        {
            return send_raw(pkt, 0, 0, 0, copies, true, any_queue);
        }

        /*! Transmit the packet asynchronously. */
        /*!
         * The transmission takes place asynchronously at the given timespec time, specified
         * as a generic chrono::time_point.
         * Requires the socket is bound for transmission to one (or multiple) PFQ kernel threads.
         * See 'bind_tx'.
         */

        template <typename Clock, typename Duration>
        bool
        send_at(const_buffer pkt, std::chrono::time_point<Clock, Duration> const &tp, unsigned int copies = 1)
        {
            auto ns = std::chrono::duration_cast<std::chrono::nanoseconds>(tp.time_since_epoch()).count();
            return send_raw(pkt, 0, 0, static_cast<uint64_t>(ns), copies, true, any_queue);
        }

        //! Schedule a packet transmission.
        /*!
         * The packet is copied into a Tx queue. If 'async' is true and 'queue' is set to any_queue, a TSS symmetric hash
         * function is used to select the Tx queue. The packet is transmitted at the given timestamp by a PFQ kernel thread.
         * Otherwise the queue is flushed every 'fhint' packets.
         * A timestamp of 0 nanoseconds means immediate transmission.
         */

        bool
        send_raw(const_buffer pkt, int ifindex, int qindex, uint64_t nsec, unsigned int copies, bool async = false, int queue = any_queue)
        {
            if (unlikely(!data_->shm_addr))
                throw system_error("PFQ: send: socket not enabled");

            ptrdiff_t *poff_addr;
            uint32_t rx_off = 0; int tss;
            uint16_t caplen;

            auto tx = [&] {

                if (async) {
                    if (unlikely(data_->tx_num_async == 0))
                        throw system_error("PFQ: send: socket not bound to async threads");
                    tss = static_cast<int>(fold(queue == any_queue ? symmetric_hash(pkt.first) : static_cast<uint32_t>(queue), static_cast<uint32_t>(data_->tx_num_async)));
                    return &static_cast<struct pfq_shared_queue *>(data_->shm_addr)->tx_async[tss];
                }

                tss = -1;
                return &static_cast<struct pfq_shared_queue *>(data_->shm_addr)->tx;
            }();

            // swap the queue...
            //

            auto index = __atomic_load_n(&tx->cons.index, __ATOMIC_RELAXED);
            if (index == __atomic_load_n(&tx->prod.index, __ATOMIC_RELAXED))
            {
                ++index;

                poff_addr = (index & 1) ? &tx->prod.off1 : &tx->prod.off0;

                __atomic_store_n(poff_addr, 0, __ATOMIC_RELEASE);
                __atomic_store_n(&tx->prod.index, index, __ATOMIC_RELEASE);
            }
            else
            {
                poff_addr = (index & 1) ? &tx->prod.off1 : &tx->prod.off0;
            }

            char * base_addr = static_cast<char *>(data_->tx_queue_addr) + data_->tx_queue_size * static_cast<size_t>(2 * (1+tss) + (index & 1 ? 1 : 0));

            // get the current offset...
            //
            auto offset = __atomic_load_n(poff_addr, __ATOMIC_RELAXED);

            // cut the packet to maxlen:
            //

            // calc the real caplen:
            
            if (pkt.first >= data_->rx_queue_addr && pkt.first < (static_cast<char *>(data_->rx_queue_addr) + 2 * data_->rx_queue_size))
            {
                caplen = 0;
                rx_off = static_cast<uint32_t>(pkt.first - static_cast<char *>(data_->rx_queue_addr));
            }
            else
            {
                caplen = static_cast<uint16_t>(
                    std::min(pkt.second, data_->tx_slot_size - sizeof(struct pfq_pkthdr)));
            }
            
            // compute the current dynamic slot_size:
            //
            auto this_slot_size = align<64>(sizeof(struct pfq_pkthdr) + caplen);

            // ensure there's enough space for the current this_slot_size + the next header:
            //
            if ((static_cast<size_t>(offset) + this_slot_size) < data_->tx_queue_size)
            {
                auto hdr = (struct pfq_pkthdr *)(base_addr + offset);

                hdr->tstamp.tv64      = nsec;
                hdr->len              = static_cast<uint16_t>(pkt.second);
                hdr->caplen           = static_cast<uint16_t>(caplen);
                hdr->info.data.copies = copies;
                hdr->info.ifindex     = ifindex;
                hdr->info.queue       = static_cast<uint8_t>(qindex);
		        hdr->info.data.fwd_off  = rx_off;

		        if (caplen)
			        memcpy(hdr+1, pkt.first, caplen);
                else
                	data_->tx_forward++;

                __atomic_store_n(poff_addr, offset + static_cast<ptrdiff_t>(this_slot_size), __ATOMIC_RELEASE);
                return true;
            }

            return false;
        }

        //! Transmit the packets in the queue.
        /*!
         * Transmit the packets in the queue of the socket. 'queue = 0' is the
         * queue of the socket enabled for synchronous transmission.
         */

        void
        transmit_queue(int queue = 0)
        {
            if (::setsockopt(data()->fd, PF_Q, Q_SO_TX_QUEUE_XMIT, &queue, sizeof(queue)) == -1)
                throw system_error(errno, "PFQ: Tx queue");
        }
    };


    template <typename CharT, typename Traits>
    typename std::basic_ostream<CharT, Traits> &
    operator<<(std::basic_ostream<CharT,Traits> &out, const pfq_stats& rhs)
    {
        return out << "recv:" << rhs.recv << ' '
                   << "lost:" << rhs.lost << ' '
                   << "drop:" << rhs.drop << ' '
                   << "send:" << rhs.sent << ' '
                   << "disc:" << rhs.disc << ' '
                   << "fail:" << rhs.fail << ' '
                   << "frwd:" << rhs.frwd << ' '
                   << "kern:" << rhs.kern;
    }

    inline pfq_stats&
    operator+=(pfq_stats &lhs, const pfq_stats &rhs)
    {
        lhs.recv += rhs.recv;
        lhs.lost += rhs.lost;
        lhs.drop += rhs.drop;

        lhs.sent += rhs.sent;
        lhs.disc += rhs.disc;
        lhs.fail += rhs.fail;

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
        lhs.fail -= rhs.fail;

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

