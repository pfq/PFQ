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

#ifndef _PFQ_HPP_
#define _PFQ_HPP_ 

#include <linux/if_ether.h>
#include <linux/pf_q.h>

#include <sys/types.h>          /* See NOTES */
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <sys/mman.h>
#include <poll.h>

#include <iostream>
#include <sstream>
#include <stdexcept>
#include <iterator>
#include <cstring>
#include <cassert>
#include <cerrno>
#include <cstdint>
#include <thread>
#include <vector>
#include <system_error>


namespace net { 

    
    typedef std::pair<char *, size_t> mutable_buffer;
    typedef std::pair<const char *, const size_t> const_buffer;

    
    template<size_t N, typename T>
    inline T align(T value)
    {
        static_assert((N & (N-1)) == 0, "align: N not a power of two");
        return (value + (N-1)) & ~(N-1);
    }

    static inline
    void mb()  { asm volatile ("mfence" ::: "memory"); }

    static inline
    void wmb() { asm volatile ("lfence" ::: "memory"); }
    
    static inline
    void rmb() { asm volatile ("sfence" ::: "memory"); }


    class queue 
    {
    public:
         
        struct const_iterator;

        /* simple forward iterator over frames */
        struct iterator : public std::iterator<std::forward_iterator_tag, pfq_hdr>
        {
            friend struct queue::const_iterator;

            iterator(pfq_hdr *h, size_t slot_size, size_t index)
            : hdr_(h), slot_size_(slot_size), index_(index)
            {}

            ~iterator() = default;
            
            iterator(const iterator &other)
            : hdr_(other.hdr_), slot_size_(other.slot_size_), index_(other.index_)
            {}

            iterator & 
            operator++()
            {
                hdr_ = reinterpret_cast<pfq_hdr *>(
                        reinterpret_cast<char *>(hdr_) + slot_size_);
                return *this;
            }
            
            iterator 
            operator++(int)
            {
                iterator ret(*this);
                ++(*this);
                return ret;
            }

            pfq_hdr *
            operator->() const
            {
                return hdr_;
            }

            pfq_hdr &
            operator*() const                                  
            {
                return *hdr_;
            }

            void *
            data() const
            {
                return hdr_+1;
            }

            bool
            ready() const
            {
                auto b = const_cast<volatile uint8_t &>(hdr_->commit) != (index_ & 0xff); 
                rmb();
                return b;
            }

            bool 
            operator==(const iterator &other) const
            {
                return hdr_ == other.hdr_;
            }

            bool
            operator!=(const iterator &other) const
            {
                return !(*this == other);
            }

        private:
            pfq_hdr *hdr_;
            size_t   slot_size_;
            size_t   index_;
        };

        /* simple forward const_iterator over frames */
        struct const_iterator : public std::iterator<std::forward_iterator_tag, pfq_hdr>
        {
            const_iterator(pfq_hdr *h, size_t slot_size, size_t index)
            : hdr_(h), slot_size_(slot_size), index_(index)
            {}

            const_iterator(const const_iterator &other)
            : hdr_(other.hdr_), slot_size_(other.slot_size_), index_(other.index_)
            {}

            const_iterator(const queue::iterator &other)
            : hdr_(other.hdr_), slot_size_(other.slot_size_), index_(other.index_)
            {}

            ~const_iterator() = default;

            const_iterator & 
            operator++()
            {
                hdr_ = reinterpret_cast<pfq_hdr *>(
                        reinterpret_cast<char *>(hdr_) + slot_size_);
                return *this;
            }
            
            const_iterator 
            operator++(int)
            {
                const_iterator ret(*this);
                ++(*this);
                return ret;
            }

            const pfq_hdr *
            operator->() const
            {
                return hdr_;
            }

            const pfq_hdr &
            operator*() const
            {
                return *hdr_;
            }

            const void *
            data() const
            {
                return hdr_+1;
            }

            bool
            ready() const
            {
                auto b = const_cast<volatile uint8_t &>(hdr_->commit) != (index_ & 0xff); 
                rmb();
                return b;
            }

            bool 
            operator==(const const_iterator &other) const
            {
                return hdr_ == other.hdr_;
            }

            bool
            operator!=(const const_iterator &other) const
            {
                return !(*this == other);
            }

        private:
            pfq_hdr *hdr_;
            size_t  slot_size_;
            size_t  index_;
        };

    public:
        queue(void *addr, size_t slot_size, size_t queue_len, size_t index)
        : addr_(addr), slot_size_(slot_size), queue_len_(queue_len), index_(index)
        {}

        ~queue() = default;

        size_t
        size() const
        {
            // return the number of packets in this queue.
            return queue_len_;
        }

        bool
        empty() const
        {
            return queue_len_ == 0;
        }

        size_t 
        index() const
        {
            return index_;
        }

        size_t
        slot_size() const
        {
            return slot_size_;
        }

        const void *
        data() const
        {
            return addr_;
        }

        iterator
        begin()  
        {
            return iterator(reinterpret_cast<pfq_hdr *>(addr_), slot_size_, index_);
        }

        const_iterator
        begin() const  
        {
            return const_iterator(reinterpret_cast<pfq_hdr *>(addr_), slot_size_, index_);
        }

        iterator
        end()  
        {
            return iterator(reinterpret_cast<pfq_hdr *>(
                        static_cast<char *>(addr_) + queue_len_ * slot_size_), slot_size_, index_);
        }

        const_iterator
        end() const 
        {
            return const_iterator(reinterpret_cast<pfq_hdr *>(
                        static_cast<char *>(addr_) + queue_len_ * slot_size_), slot_size_, index_);
        }

        const_iterator
        cbegin() const
        {
            return const_iterator(reinterpret_cast<pfq_hdr *>(addr_), slot_size_, index_);
        }

        const_iterator
        cend() const 
        {
            return const_iterator(reinterpret_cast<pfq_hdr *>(
                        static_cast<char *>(addr_) + queue_len_ * slot_size_), slot_size_, index_);
        }

    private:
        void    *addr_;
        size_t  slot_size_;
        size_t  queue_len_;
        size_t  index_;
    };

    static inline void * data_ready(pfq_hdr &h, uint8_t current_commit)
    {
        if (const_cast<volatile uint8_t &>(h.commit) != current_commit)
            return nullptr;
        rmb();
        return &h + 1;
    }

    static inline const void * data_ready(pfq_hdr const &h, uint8_t current_commit)
    {
        if (const_cast<volatile uint8_t &>(h.commit) != current_commit)
            return nullptr;
        rmb();
        return &h + 1;
    }
 

    //////////////////////////////////////////////////////////////////////

    class pfq_error : public std::system_error
    {
    public:
    
        pfq_error(int ev, const char * reason)
        : std::system_error(ev, std::generic_category(), reason)
        {}

        pfq_error(const char *reason)
        : std::system_error(0, std::generic_category(), reason)
        {}

        virtual ~pfq_error() noexcept
        {}
    };

    
    //////////////////////////////////////////////////////////////////////
    // utility functions...
    
    namespace 
    {
        inline int 
        ifindex(int fd, const char *dev) 
        {
            struct ifreq ifreq_io;
            memset(&ifreq_io, 0, sizeof(struct ifreq));
            strncpy(ifreq_io.ifr_name, dev, IFNAMSIZ);
            if (::ioctl(fd, SIOCGIFINDEX, &ifreq_io) == -1)
                throw pfq_error(errno, "PFQ: ioctl SIOCGIFINDEX");
            return ifreq_io.ifr_ifindex;
        }


        inline
        void toggle_promisc(int fd, const char *dev, bool value)
        {
            struct ifreq ifreq_io;

            memset(&ifreq_io, 0, sizeof(struct ifreq));
            strncpy(ifreq_io.ifr_name, dev, IFNAMSIZ);

            if(::ioctl(fd, SIOCGIFFLAGS, &ifreq_io) == -1) 
                throw pfq_error(errno, "PFQ: ioctl SIOCGIFFLAGS");

            if (value)
                ifreq_io.ifr_flags |= IFF_PROMISC;
            else 
                ifreq_io.ifr_flags &= ~IFF_PROMISC;

            if(::ioctl(fd, SIOCSIFFLAGS, &ifreq_io) == -1)
                throw pfq_error(errno, "PFQ: ioctl SIOCSIFFLAGS");

        }


        inline
        int nametoindex(const char *dev)
        {
            auto i = ::if_nametoindex(dev);
            if (i == 0)
                throw pfq_error("PFQ: unknown device");
            return i;
        }

        
        inline
        std::string
        indextoname(int i)
        {
            char buf[IF_NAMESIZE];
            if (::if_indextoname(i, buf) == nullptr)
                throw pfq_error("PFQ: index not available");
            return buf;
        }
    }

    //////////////////////////////////////////////////////////////////////

    // group policies
    // 
    
    enum class group_policy : int16_t
    {
        undefined  = Q_GROUP_UNDEFINED, 
        priv       = Q_GROUP_PRIVATE,
        restricted = Q_GROUP_RESTRICTED,
        shared     = Q_GROUP_SHARED
    };

    // class mask
    //
    
    typedef unsigned long class_mask;

    namespace 
    {
        const class_mask  class_default = Q_CLASS_DEFAULT;
        const class_mask  class_any     = Q_CLASS_ANY;
    }


    //////////////////////////////////////////////////////////////////////
    // PFQ class


    class pfq
    {
        struct pfq_data
        {
            int id;
            int gid;

            void * queue_addr;
            size_t queue_tot_mem;
            size_t queue_slots; 
            size_t queue_caplen;
            size_t queue_offset;
            size_t slot_size;
        };

        int fd_;
        
        std::unique_ptr<pfq_data> pdata_;

    public:

        static constexpr int any_device  = Q_ANY_DEVICE;
        static constexpr int any_queue   = Q_ANY_QUEUE;
        static constexpr int any_group   = Q_ANY_GROUP;

        pfq()
        : fd_(-1)
        , pdata_()
        {}


        pfq(size_t caplen, size_t offset = 0, size_t slots = 131072)
        : fd_(-1)
        , pdata_()
        {
            this->open(class_default, group_policy::priv, caplen, offset, slots); 
        }
    
        pfq(group_policy policy, size_t caplen, size_t offset = 0, size_t slots = 131072)
        : fd_(-1)
        , pdata_()
        {
            this->open(class_default, policy, caplen, offset, slots);
        }

        pfq(class_mask mask, group_policy policy, size_t caplen, size_t offset = 0, size_t slots = 131072)
        : fd_(-1)
        , pdata_()
        {
            this->open(mask, policy, caplen, offset, slots);
        }

        ~pfq()
        {
            this->close();
        }

        /* pfq object is non copyable */
        
        pfq(const pfq&) = delete;
        pfq& operator=(const pfq&) = delete;


        /* pfq object is moveable */

        pfq(pfq &&other)
        : fd_(other.fd_)
        , pdata_(std::move(other.pdata_))
        {
            other.fd_ = -1;
        }

        /* move assignment operator */
        
        pfq& 
        operator=(pfq &&other)
        {
            if (this != &other)
            {
                fd_    = other.fd_;
                pdata_ = std::move(other.pdata_);
                other.fd_ = -1;
            }
            return *this;
        }
        
        /* swap */ 
        
        void 
        swap(pfq &other)
        {
            std::swap(fd_,    other.fd_);
            std::swap(pdata_, other.pdata_);
        }                           

        int 
        id() const
        {
            if (pdata_)
                return pdata_->id;
            return -1;
        }

        int 
        group_id() const
        {
            if (pdata_)
                return pdata_->gid;
            return -1;
        }

        int 
        fd() const
        {
            return fd_;
        }

        void
        open(group_policy policy, size_t caplen, size_t offset = 0, size_t slots = 131072)
        {
            this->open(caplen, offset, slots);
            
            if (policy != group_policy::undefined)
            {
                pdata_->gid = this->join_group(any_group, policy, class_default);
            }
        }
        
        void
        open(class_mask mask, group_policy policy, size_t caplen, size_t offset = 0, size_t slots = 131072)
        {
            this->open(caplen, offset, slots);
            
            if (policy != group_policy::undefined)
            {
                pdata_->gid = this->join_group(any_group, policy, mask);
            }
        }

        void
        open(size_t caplen, size_t offset = 0, size_t slots = 131072)
        {
            if (fd_ != -1)
                throw pfq_error("PFQ: already open");

            fd_ = ::socket(PF_Q, SOCK_RAW, htons(ETH_P_ALL));
            if (fd_ == -1)
                throw pfq_error("PFQ: module not loaded");
            
            /* allocate pdata */
            pdata_.reset(new pfq_data { -1, -1, nullptr, 0, 0, 0, offset, 0 });

            /* get id */
            socklen_t size = sizeof(pdata_->id);
            if (::getsockopt(fd_, PF_Q, Q_SO_GET_ID, &pdata_->id, &size) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_GET_ID");

            /* set queue slots */
            if (::setsockopt(fd_, PF_Q, Q_SO_SLOTS, &slots, sizeof(slots)) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_SLOTS");
            pdata_->queue_slots = slots;
            
            /* set caplen */
            if (::setsockopt(fd_, PF_Q, Q_SO_CAPLEN, &caplen, sizeof(caplen)) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_CAPLEN");
            pdata_->queue_caplen = caplen;
            
            /* set offset */
            if (::setsockopt(fd_, PF_Q, Q_SO_OFFSET, &offset, sizeof(offset)) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_OFFSET");
            
            pdata_->slot_size = align<8>(sizeof(pfq_hdr) + pdata_->queue_caplen);
        }


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
        
        
        void 
        enable()
        {
            int one = 1;

            if(::setsockopt(fd_, PF_Q, Q_SO_TOGGLE_QUEUE, &one, sizeof(one)) == -1) {
                throw pfq_error(errno, "PFQ: Q_SO_TOGGLE_QUEUE");
            }

            size_t tot_mem; socklen_t size = sizeof(tot_mem);
            
            if (::getsockopt(fd_, PF_Q, Q_SO_GET_QUEUE_MEM, &tot_mem, &size) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_GET_QUEUE_MEM");
            
            pdata_->queue_tot_mem = tot_mem;

            if ((pdata_->queue_addr = mmap(nullptr, tot_mem, PROT_READ|PROT_WRITE, MAP_SHARED, fd_, 0)) == MAP_FAILED) 
                throw pfq_error(errno, "PFQ: mmap error");
            
        }

        
        void 
        disable()
        {
            if (fd_ == -1)
                throw pfq_error("PFQ: not open");

            if (munmap(pdata_->queue_addr, pdata_->queue_tot_mem) == -1)
                throw pfq_error(errno, "PFQ: munmap");
            
            pdata_->queue_addr = nullptr;
            pdata_->queue_tot_mem = 0;
            
            int one = 0;
            if(::setsockopt(fd_, PF_Q, Q_SO_TOGGLE_QUEUE, &one, sizeof(one)) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_TOGGLE_QUEUE");
        }

        
        bool 
        is_enabled() const
        {
            if (fd_ != -1)
            {
                int ret; socklen_t size = sizeof(ret);

                if (::getsockopt(fd_, PF_Q, Q_SO_GET_STATUS, &ret, &size) == -1)
                    throw pfq_error(errno, "PFQ: Q_SO_GET_STATUS");
                return ret;
            }
            return false;
        }


        void 
        toggle_time_stamp(bool value)
        {
            int ts = static_cast<int>(value);
            if (::setsockopt(fd_, PF_Q, Q_SO_TSTAMP_TOGGLE, &ts, sizeof(ts)) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_TSTAMP_TOGGLE");
        }


        bool 
        time_stamp() const
        {
           int ret; socklen_t size = sizeof(int);
           if (::getsockopt(fd_, PF_Q, Q_SO_GET_TSTAMP, &ret, &size) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_GET_TSTAMP");
           return ret;
        }


        void 
        caplen(size_t value)
        {
            if (is_enabled()) 
                throw pfq_error("PFQ: enabled (caplen could not be set)");
            
            if (::setsockopt(fd_, PF_Q, Q_SO_CAPLEN, &value, sizeof(value)) == -1) {
                throw pfq_error(errno, "PFQ: Q_SO_CAPLEN");
            }

            pdata_->slot_size = align<8>(sizeof(pfq_hdr)+ value);
        }
        

        size_t 
        caplen() const
        {
           size_t ret; socklen_t size = sizeof(ret);
           if (::getsockopt(fd_, PF_Q, Q_SO_GET_CAPLEN, &ret, &size) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_GET_CAPLEN");
           return ret;
        }


        void 
        offset(size_t value)
        {
            if (is_enabled()) 
                throw pfq_error("PFQ: enabled (offset could not be set)");
            
            if (::setsockopt(fd_, PF_Q, Q_SO_OFFSET, &value, sizeof(value)) == -1) {
                throw pfq_error(errno, "PFQ: Q_SO_OFFSET");
            }
        }


        size_t 
        offset() const
        {
           size_t ret; socklen_t size = sizeof(ret);
           if (::getsockopt(fd_, PF_Q, Q_SO_GET_OFFSET, &ret, &size) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_GET_OFFSET");
           return ret;
        }


        void 
        slots(size_t value) 
        {             
            if (is_enabled()) 
                throw pfq_error("PFQ: enabled (slots could not be set)");
                      
            if (::setsockopt(fd_, PF_Q, Q_SO_SLOTS, &value, sizeof(value)) == -1) {
                throw pfq_error(errno, "PFQ: Q_SO_SLOTS");
            }

            pdata_->queue_slots = value;
        }


        size_t 
        slots() const
        {   
            if (!pdata_)
                throw pfq_error("PFQ: not open");

            return pdata_->queue_slots;
        }


        size_t 
        slot_size() const
        {
            if (!pdata_)
                throw pfq_error("PFQ: not open");
            
            return pdata_->slot_size;
        }


        void 
        bind(const char *dev, int queue = any_queue)
        {
            auto gid = group_id();
            if (gid < 0)
                throw pfq_error("PFQ: default group undefined");

            bind_group(gid, dev, queue);
        }                              


        void
        bind_group(int gid, const char *dev, int queue = any_queue)
        {
            auto index = ifindex(this->fd(), dev);
            if (index == -1)
                throw pfq_error("PFQ: device not found");

            struct pfq_binding b = { gid, index, queue };
            if (::setsockopt(fd_, PF_Q, Q_SO_ADD_BINDING, &b, sizeof(b)) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_ADD_BINDING");
        }


        void 
        unbind(const char *dev, int queue = any_queue)
        {
            auto gid = group_id();
            if (gid < 0)
                throw pfq_error("PFQ: default group undefined");
        
            unbind_group(gid, dev, queue);
        }  


        void 
        unbind_group(int gid, const char *dev, int queue = any_queue)
        {
            auto index = ifindex(this->fd(), dev);
            if (index == -1)
                throw pfq_error("PFQ: device not found");
        
            struct pfq_binding b = { gid, index, queue };
            if (::setsockopt(fd_, PF_Q, Q_SO_REMOVE_BINDING, &b, sizeof(b)) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_REMOVE_BINDING");
        }


        unsigned long
        groups_mask() const
        {
            unsigned long mask; socklen_t size = sizeof(mask);
            if (::getsockopt(fd_, PF_Q, Q_SO_GET_GROUPS, &mask, &size) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_GET_GROUPS");
            return mask;
        }


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

        void
        set_steering_function(int gid, const char *fun)
        {
            struct pfq_steering s { fun, gid };
            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_STEER, &s, sizeof(s)) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_GROUP_STEER");
        }
        
        template <typename T>
        void
        set_group_state(int gid, const T &state)
        {
            struct pfq_group_state s { &state, sizeof(state), gid };
            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_STATE, &s, sizeof(s)) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_GROUP_STATE");
        }

        void
        reset_group_state(int gid)
        {
            struct pfq_group_state s { NULL, 0, gid };
            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_STATE, &s, sizeof(s)) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_GROUP_STATE");
        }

        int
        join_group(int gid, group_policy pol = group_policy::shared, class_mask mask = class_default)
        {
            if (pol == group_policy::undefined)
                throw pfq_error("PFQ: join with undefined policy!");

            struct pfq_group_join group { gid, static_cast<int16_t>(pol), mask };

            socklen_t size = sizeof(group);
            if (::getsockopt(fd_, PF_Q, Q_SO_GROUP_JOIN, &group, &size) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_GROUP_JOIN");
            return group.gid;
        }
        

        void
        leave_group(int gid)
        {
            if (::setsockopt(fd_, PF_Q, Q_SO_GROUP_LEAVE, &gid, sizeof(gid)) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_GROUP_LEAVE");
        }


        int 
        poll(long int microseconds = -1 /* infinite */)
        {
            if (fd_ == -1)
                throw pfq_error("PFQ: not open");

            struct pollfd fd = {fd_, POLLIN, 0 };
            struct timespec timeout = { microseconds/1000000, (microseconds%1000000) * 1000};

            int ret = ::ppoll(&fd, 1, microseconds < 0 ? nullptr : &timeout, nullptr);
            if (ret < 0)
               throw pfq_error(errno, "PFQ: ppoll");
            return ret; 
        }

        
        queue
        read(long int microseconds = -1) 
        {
            if (!pdata_ || !pdata_->queue_addr)
                throw pfq_error("PFQ: not enabled");

            struct pfq_queue_descr * q = static_cast<struct pfq_queue_descr *>(pdata_->queue_addr);
            
            size_t data = q->data;
            size_t index = DBMP_QUEUE_INDEX(data);
            size_t q_size = pdata_->queue_slots * pdata_->slot_size;

            //  watermark for polling...
            
            if( DBMP_QUEUE_LEN(data) < (pdata_->queue_slots >> 1) ) {
                this->poll(microseconds);
            }

            // reset the next buffer...
            
            data = __sync_lock_test_and_set(&q->data, (unsigned int)((index+1) << 24));
            
            
            auto queue_len =  std::min(static_cast<size_t>(DBMP_QUEUE_LEN(data)), pdata_->queue_slots);

            
            return queue(static_cast<char *>(pdata_->queue_addr) + 
						 sizeof(pfq_queue_descr) + 
						 (index & 1) * q_size, 
                         pdata_->slot_size, queue_len, index);
        }


        uint8_t
        current_commit() const
        {
            struct pfq_queue_descr * q = static_cast<struct pfq_queue_descr *>(pdata_->queue_addr);
            return DBMP_QUEUE_INDEX(q->data);
        }


        queue
        recv(const mutable_buffer &buff, long int microseconds = -1) 
        {
            if (fd_ == -1)
                throw pfq_error("PFQ: not open");
            
            auto this_queue = this->read(microseconds);
            
            if (buff.second < pdata_->queue_slots * pdata_->slot_size)
                throw pfq_error("PFQ: buffer too small");

            memcpy(buff.first, this_queue.data(), this_queue.slot_size() * this_queue.size());
            return queue(buff.first, this_queue.slot_size(), this_queue.size(), this_queue.index());
        }

        // typedef void (*pfq_handler)(char *user, const struct pfq_hdr *h, const char *data); 

        template <typename Fun>
        size_t dispatch(Fun callback, long int microseconds = -1, char *user = nullptr)
        {
            auto many = this->read(microseconds); 
            
            auto it = std::begin(many),
                 it_e = std::end(many);
            size_t n = 0;
            for(; it != it_e; ++it)
            {
                while (!it->ready)
                    std::this_thread::yield();

                callback(user, &(*it), reinterpret_cast<const char *>(it.data()));
                n++;
            }
            return n;
        }


        pfq_stats
        stats() const
        {
            pfq_stats stat;
            socklen_t size = sizeof(struct pfq_stats);
            if (::getsockopt(fd_, PF_Q, Q_SO_GET_STATS, &stat, &size) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_GET_STATS");
            return stat;
        }


        pfq_stats
        group_stats(int gid) const
        {
            pfq_stats stat;
            stat.recv = static_cast<unsigned long>(gid);
            socklen_t size = sizeof(struct pfq_stats);
            if (::getsockopt(fd_, PF_Q, Q_SO_GET_GROUP_STATS, &stat, &size) == -1)
                throw pfq_error(errno, "PFQ: Q_SO_GET_GROUP_STATS");
            return stat;
        }


        size_t
        mem_size() const
        {
            if (pdata_)
                return pdata_->queue_tot_mem;
            return 0;
        }


        const void *
        mem_addr() const
        {
            if (pdata_)
                return pdata_->queue_addr;
            return nullptr;
        }

    };


    template <typename CharT, typename Traits>
    typename std::basic_ostream<CharT, Traits> &
    operator<<(std::basic_ostream<CharT,Traits> &out, const pfq_stats& rhs)
    {
        return out << rhs.recv << ' ' << rhs.lost << ' ' << rhs.drop;
    }

    inline pfq_stats&
    operator+=(pfq_stats &lhs, const pfq_stats &rhs)
    {
        lhs.recv += rhs.recv;
        lhs.lost += rhs.lost;
        lhs.drop += rhs.drop;
        return lhs;
    }
    
    inline pfq_stats&
    operator-=(pfq_stats &lhs, const pfq_stats &rhs)
    {
        lhs.recv -= rhs.recv;
        lhs.lost -= rhs.lost;
        lhs.drop -= rhs.drop;
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

#endif /* _PFQ_HPP_ */
