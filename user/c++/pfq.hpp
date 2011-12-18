/***************************************************************
 *                                                
 * (C) 2011 - Nicola Bonelli <nicola.bonelli@cnit.it>   
 *
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
#include <stdexcept>
#include <iterator>
#include <cstring>
#include <cassert>
#include <cerrno>
#include <cstdint>
#include <thread>

namespace net { 

    typedef std::pair<char *, size_t> mutable_buffer;
    typedef std::pair<const char *, const size_t> const_buffer;

    template <intptr_t N, typename T>
    inline T *align(T *value)
    {
        static_assert((N & (N-1)) == 0, "align: N not a power of two");
        return  reinterpret_cast<T *>(
                    (reinterpret_cast<intptr_t>(value) + intptr_t(N-1)) & ~intptr_t(N-1));
    }
    
    class batch 
    {
    public:
         
        struct const_iterator;

        /* simple forward iterator over frames */
        struct iterator : public std::iterator<std::forward_iterator_tag, pfq_hdr>
        {
            friend struct batch::const_iterator;

            iterator(pfq_hdr *h)
            : hdr(h)
            {}

            ~iterator()
            {}
            
            iterator(const iterator &other)
            : hdr(other.hdr)
            {}

            iterator & 
            operator++()
            {
                hdr = reinterpret_cast<pfq_hdr *>(
                        align<8>(reinterpret_cast<char *>(hdr+1) + hdr->caplen));
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
                return hdr;
            }

            pfq_hdr &
            operator*() const                                  
            {
                return *hdr;
            }

            char *
            data() const
            {
                return reinterpret_cast<char *>(hdr+1);
            }

            bool 
            operator==(const iterator &other) const
            {
                return hdr == other.hdr;
            }

            bool
            operator!=(const iterator &other) const
            {
                return !(*this == other);
            }

        private:
            pfq_hdr *hdr;
        };

        /* simple forward const_iterator over frames */
        struct const_iterator : public std::iterator<std::forward_iterator_tag, pfq_hdr>
        {
            const_iterator(pfq_hdr *h)
            : hdr(h) 
            {}

            const_iterator(const const_iterator &other)
            : hdr(other.hdr)
            {}

            const_iterator(const batch::iterator &other)
            : hdr(other.hdr)
            {}

            ~const_iterator()
            {}

            const_iterator & 
            operator++()
            {
                hdr = reinterpret_cast<pfq_hdr *>(
                        align<8>(reinterpret_cast<char *>(hdr+1) + hdr->caplen));
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
                return hdr;
            }

            const pfq_hdr &
            operator*() const
            {
                return *hdr;
            }

            const char *
            data() const
            {
                return reinterpret_cast<const char *>(hdr+1);
            }

            bool 
            operator==(const const_iterator &other) const
            {
                return hdr == other.hdr;
            }

            bool
            operator!=(const const_iterator &other) const
            {
                return !(*this == other);
            }

        private:
            pfq_hdr *hdr;
        };

    public:

        batch(char *queue, uint64_t valid_data)
        : m_ptr(queue), m_data(valid_data)
        {}

        size_t
        size() const
        {
            // for the iterator size() means the number of packets in this batch.
            // for the shared queue point of view means DBMP_QUEUE_LEN. 
            return DBMP_QUEUE_LEN(m_data);
        }

        const void *
        data() const
        {
            return m_ptr;
        }

        size_t
        nbytes() const
        {
            return DBMP_QUEUE_SIZE(m_data);
        }

        iterator
        begin()  
        {
            return iterator(reinterpret_cast<pfq_hdr *>(m_ptr));
        }

        const_iterator
        begin() const  
        {
            return const_iterator(reinterpret_cast<pfq_hdr *>(m_ptr));
        }

        iterator
        end()  
        {
            return iterator(reinterpret_cast<pfq_hdr *>(m_ptr + DBMP_QUEUE_SIZE(m_data)));
        }

        const_iterator
        end() const 
        {
            return const_iterator(reinterpret_cast<pfq_hdr *>(m_ptr + DBMP_QUEUE_SIZE(m_data)));
        }

        const_iterator
        cbegin() const
        {
            return const_iterator(reinterpret_cast<pfq_hdr *>(m_ptr));
        }

        const_iterator
        cend() const 
        {
            return const_iterator(reinterpret_cast<pfq_hdr *>(m_ptr + DBMP_QUEUE_SIZE(m_data)));
        }

    private:
        char    *m_ptr;
        uint64_t m_data;
    };

    //////////////////////////////////////////////////////////////////////

    struct pfq_open_t {};
    
    namespace
    {
        pfq_open_t pfq_open = {};
    }

    class pfq
    {
    public:
        static const int any_device = Q_ANY_DEVICE;
        static const int any_queue  = Q_ANY_QUEUE;

        pfq()
        : m_q(-1),
          m_id(0),
          m_mem(NULL),
          m_mem_size(0)
        {}

        pfq(pfq_open_t)
        : m_q(socket(PF_Q, SOCK_RAW, htons(ETH_P_ALL))),
          m_id(get_id()),             
          m_mem(NULL),
          m_mem_size(0),
          m_queue_size(0)
        {
            if (m_q == -1)
                throw std::runtime_error("PFQ module not loaded");

            int tot_mem; socklen_t size = sizeof(int);
            if (::getsockopt(m_q, PF_Q, SO_GET_TOT_MEM, &tot_mem, &size) == -1)
                throw std::runtime_error("pfq: SO_GET_TOT_MEM");

            if ((m_mem = mmap(NULL, tot_mem, PROT_READ|PROT_WRITE, MAP_SHARED, m_q, 0)) == MAP_FAILED) 
                throw std::runtime_error(std::string(__PRETTY_FUNCTION__).append(": mmap"));

            m_mem_size = tot_mem;
           
            size_t queue_size; size = sizeof(queue_size);
            if (::getsockopt(m_q, PF_Q, SO_GET_QUEUE_MEM, &queue_size, &size) == -1)
                throw std::runtime_error(__PRETTY_FUNCTION__);
           
            m_queue_size = queue_size;

        }

        ~pfq()
        {
            this->dtor();
        }

        pfq(const pfq&) = delete;
        pfq& operator=(const pfq&) = delete;

    public:

        pfq(pfq &&other)
        : m_q(other.m_q), 
          m_id(other.m_id),
          m_mem(other.m_mem), 
          m_mem_size(other.m_mem_size),
          m_queue_size(other.m_queue_size)
        {
            other.m_q = -1;
            other.m_id = -1;
        }

        pfq& operator=(pfq &&other)
        {
            if (this != &other)
            {
                this->dtor();

                m_q = other.m_q;
                m_id = other.m_id;
                m_mem = other.m_mem;
                m_mem_size = other.m_mem_size;
                m_queue_size = other.m_queue_size;
        
                other.m_q = -1;    
                other.m_id = -1;
            }
            return *this;
        }
        
        void swap(pfq &other)
        {
            std::swap(m_q,          other.m_q);
            std::swap(m_id,         other.m_id);
            std::swap(m_mem,        other.m_mem);
            std::swap(m_mem_size,   other.m_mem_size);
            std::swap(m_queue_size, other.m_queue_size);
        }

        int id() const
        {
            return m_id;
        }

        void enable()
        {
            char one = 1;
            if(::setsockopt(m_q, PF_Q, SO_TOGGLE_QUEUE, &one, sizeof(one)) == -1)
                throw std::runtime_error(__PRETTY_FUNCTION__);
        }

        void disable()
        {
            char one = 0;
            if(::setsockopt(m_q, PF_Q, SO_TOGGLE_QUEUE, &one, sizeof(one)) == -1)
                throw std::runtime_error(__PRETTY_FUNCTION__);
        }

        bool is_enabled() const
        {
            int ret; socklen_t size = sizeof(int);
            if (::getsockopt(m_q, PF_Q, SO_GET_STATUS, &ret, &size) == -1)
                throw std::runtime_error(__PRETTY_FUNCTION__);
            return ret;
        }

        void load_balance(bool value)
        {
            int one = value;
            if (::setsockopt(m_q, PF_Q, SO_LOAD_BALANCE, &one, sizeof(one)) == -1)
                throw std::runtime_error(__PRETTY_FUNCTION__);
        }

        int ifindex(const char *dev)
        {
            struct ifreq ifreq_io;
            strncpy(ifreq_io.ifr_name, dev, IFNAMSIZ);
            if (::ioctl(m_q, SIOCGIFINDEX, &ifreq_io) == -1)
                return -1;
            return ifreq_io.ifr_ifindex;
        }

        void tstamp(bool value)
        {
            size_t ts = static_cast<int>(value);
            if (::setsockopt(m_q, PF_Q, SO_TSTAMP_TYPE, &ts, sizeof(int)) == -1)
                throw std::runtime_error(__PRETTY_FUNCTION__);
        }

        bool tstamp() const
        {
           int ret; socklen_t size = sizeof(int);
           if (::getsockopt(m_q, PF_Q, SO_GET_TSTAMP_TYPE, &ret, &size) == -1)
                throw std::runtime_error(__PRETTY_FUNCTION__);
           return ret;
        }

        void caplen(size_t value)
        {
            if (::setsockopt(m_q, PF_Q, SO_CAPLEN, &value, sizeof(value)) == -1)
                throw std::runtime_error(__PRETTY_FUNCTION__);
        }

        size_t caplen() const
        {
           size_t ret; socklen_t size = sizeof(ret);
           if (::getsockopt(m_q, PF_Q, SO_GET_CAPLEN, &ret, &size) == -1)
                throw std::runtime_error(__PRETTY_FUNCTION__);
           return ret;
        }

        void add_device(int index, int queue = any_queue)
        {
            struct pfq_dev_queue dq = { index, queue };
            if (::setsockopt(m_q, PF_Q, SO_ADD_DEVICE, &dq, sizeof(dq)) == -1)
                throw std::runtime_error(__PRETTY_FUNCTION__);
        }
        
        void add_device(const char *dev, int queue = any_queue)
        {
            auto index = ifindex(dev);
            if (index == -1)
                throw std::runtime_error(std::string(__PRETTY_FUNCTION__).append(": device not found"));
            add_device(index, queue);
        }                              

        void remove_device(int index, int queue = any_queue)
        {
            struct pfq_dev_queue dq = { index, queue };
            if (::setsockopt(m_q, PF_Q, SO_REMOVE_DEVICE, &dq, sizeof(dq)) == -1)
                throw std::runtime_error(__PRETTY_FUNCTION__);
        }

        void remove_device(const char *dev, int queue = any_queue)
        {
            auto index = ifindex(dev);
            if (index == -1)
                throw std::runtime_error(std::string(__PRETTY_FUNCTION__).append(": device not found"));
            remove_device(index, queue);
        }                              
        unsigned long 
        owners(int index, int queue) const
        {
            struct pfq_dev_queue dq = { index, queue };
            socklen_t s = sizeof(struct pfq_dev_queue);

            if (::getsockopt(m_q, PF_Q, SO_GET_OWNERS, &dq, &s) == -1)
                throw std::runtime_error(__PRETTY_FUNCTION__);
            return *reinterpret_cast<unsigned long *>(&dq);
        }

        unsigned long 
        owners(const char *dev, int queue) const
        {
            return owners(dev,queue);
        }

        int 
        poll(long int microseconds = -1 /* infinite */)
        {
            struct pollfd fd = {m_q, POLLIN, 0 };
            struct timespec timeout = { microseconds/1000000, (microseconds%1000000) * 1000};

            int ret = ::ppoll(&fd, 1, microseconds < 0 ? NULL : &timeout, NULL);
            if (ret < 0)
               throw std::runtime_error(std::string(__PRETTY_FUNCTION__).append(strerror(errno)));
            return ret; 
        }

        batch
        read(long int microseconds = -1) 
        {
            struct pfq_queue_descr * q = static_cast<struct pfq_queue_descr *>(m_mem);

            uint64_t data =  q->data;               // volatile read
            
            bool index = DBMP_QUEUE_INDEX(data);
                
            if( DBMP_QUEUE_SIZE(data) < (m_queue_size >> 1) ) {
                this->poll(microseconds);
            }

            // atomic exchange: swap the queues...
            data = __sync_lock_test_and_set(&q->data,  ( index ? 0ULL : 0x8000000000000000ULL ));
            
            if( DBMP_QUEUE_SIZE(data) > m_queue_size) {

                // polling for the valid_data... (synchronize with the kernel)
                while( !(data = q->valid_data) ) 
                {
                    std::this_thread::yield();
                }
                 
                // reset the valid-data...
                q->valid_data = 0;

                // just in case the queue was blocked, re-enable it
                q->disable = 0;
                
            }            
            
            return batch(static_cast<char *>(m_mem) + sizeof(pfq_queue_descr) + index * m_queue_size, data);
        }
        
        batch
        recv(const mutable_buffer &buff, long int microseconds = -1) 
        {
            auto this_batch = this->read(microseconds);
            
            if (buff.second < m_queue_size)
                throw std::runtime_error(std::string(__PRETTY_FUNCTION__).append(": buffer too short"));

            memcpy(buff.first, this_batch.data(), this_batch.nbytes());
            return batch(buff.first, static_cast<uint64_t>(this_batch.size()<<32) | this_batch.nbytes());
        }

        pfq_stats
        stats() const
        {
            pfq_stats stat;
            socklen_t size = sizeof(struct pfq_stats);
            if (::getsockopt(m_q, PF_Q, SO_GET_STATS, &stat, &size) == -1)
                throw std::runtime_error(__PRETTY_FUNCTION__);
            return stat;
        }

        size_t
        mem_size() const
        {
            return m_mem_size;
        }

        const void *
        mem_addr() const
        {
            return m_mem;
        }

        size_t
        queue_size() const
        {
            return m_queue_size;
        }

        int get_id() const
        {
            if (m_q == -1)
                return -1;

            int ret; socklen_t size = sizeof(int);
            if (::getsockopt(m_q, PF_Q, SO_GET_ID, &ret, &size) == -1)
                throw std::runtime_error(__PRETTY_FUNCTION__);
            return ret;
        }

    private:
        void dtor()
        {
            if (m_q != -1) {
                ::close(m_q);
                m_q = -1;
                if ( munmap(m_mem, m_mem_size) == -1)
                    throw std::runtime_error(std::string(__PRETTY_FUNCTION__).append(": munmap"));
            }
        }

        int m_q;
        int m_id;

        void * m_mem;
        size_t m_mem_size;
        size_t m_queue_size; // bytes
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
