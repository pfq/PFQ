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

#include <iterator>

#include <linux/pf_q.h>


namespace pfq {

    //! This class represent a queue of packets.
    /*!
     * The memory where packets are stored is not owned by this class.
     */

    class queue
    {
    public:

        struct const_iterator;

        //! Forward iterator over packets.

        struct iterator : public std::iterator<std::forward_iterator_tag, pfq_pkt_hdr>
        {
            friend struct queue::const_iterator;

            iterator(pfq_pkt_hdr *h, size_t slot_size, size_t index)
            : hdr_(h), slot_size_(slot_size), index_(index)
            {}

            ~iterator() = default;

            iterator(const iterator &other)
            : hdr_(other.hdr_), slot_size_(other.slot_size_), index_(other.index_)
            {}

            iterator &
            operator++()
            {
                hdr_ = reinterpret_cast<pfq_pkt_hdr *>(
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

            pfq_pkt_hdr *
            operator->() const
            {
                return hdr_;
            }

            pfq_pkt_hdr &
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
                auto b = const_cast<volatile uint8_t &>(hdr_->commit) == index_;
                smp_rmb();
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
            pfq_pkt_hdr *hdr_;
            size_t   slot_size_;
            size_t   index_;
        };

        //! Constant forward iterator over packets.

        struct const_iterator : public std::iterator<std::forward_iterator_tag, pfq_pkt_hdr>
        {
            const_iterator(pfq_pkt_hdr *h, size_t slot_size, size_t index)
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
                hdr_ = reinterpret_cast<pfq_pkt_hdr *>(
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

            const pfq_pkt_hdr *
            operator->() const
            {
                return hdr_;
            }

            const pfq_pkt_hdr &
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
                auto b = const_cast<volatile uint8_t &>(hdr_->commit) == index_;
                smp_rmb();
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
            pfq_pkt_hdr *hdr_;
            size_t  slot_size_;
            size_t  index_;
        };

    public:

        //! Constructor
        /*!
         * Construct a queue descriptor, stored at the given address.
         */

        queue(void *addr, size_t slot_size, size_t queue_len, size_t index)
        : addr_(addr), slot_size_(slot_size), queue_len_(queue_len), index_(index)
        {}

        ~queue() = default;

        //! Return the number of packets stored in this queue.

        size_t
        size() const
        {
            return queue_len_;
        }

        //! Check whether the queue is empty.

        bool
        empty() const
        {
            return queue_len_ == 0;
        }

        //! Return the index position.

        size_t
        index() const
        {
            return index_;
        }

        //! Return the size of the queue slot, in bytes.

        size_t
        slot_size() const
        {
            return slot_size_;
        }

        //! Return the pointer to the packet.

        const void *
        data() const
        {
            return addr_;
        }

        //! Return an iterator to the first slot of a non-empty queue.
        /*!
         * Return end() in case of empty queue.
         */

        iterator
        begin()
        {
            return iterator(reinterpret_cast<pfq_pkt_hdr *>(addr_), slot_size_, index_);
        }

        //! Return a constant iterator to the first slot of a non-empty queue.
        /*!
         * Return end() in case of empty queue.
         */

        const_iterator
        begin() const
        {
            return const_iterator(reinterpret_cast<pfq_pkt_hdr *>(addr_), slot_size_, index_);
        }

        //! Return an iterator past to the end of the queue.

        iterator
        end()
        {
            return iterator(reinterpret_cast<pfq_pkt_hdr *>(
                        static_cast<char *>(addr_) + queue_len_ * slot_size_), slot_size_, index_);
        }

        //! Return a constant iterator past to the end of the queue.

        const_iterator
        end() const
        {
            return const_iterator(reinterpret_cast<pfq_pkt_hdr *>(
                        static_cast<char *>(addr_) + queue_len_ * slot_size_), slot_size_, index_);
        }

        //! Return a constant iterator to the first slot of an non-empty queue.
        /*!
         * Return cend() in case of empty queue.
         */

        const_iterator
        cbegin() const
        {
            return const_iterator(reinterpret_cast<pfq_pkt_hdr *>(addr_), slot_size_, index_);
        }

        //! Return a constant iterator past to the end of the queue.

        const_iterator
        cend() const
        {
            return const_iterator(reinterpret_cast<pfq_pkt_hdr *>(
                        static_cast<char *>(addr_) + queue_len_ * slot_size_), slot_size_, index_);
        }

    private:
        void    *addr_;
        size_t  slot_size_;
        size_t  queue_len_;
        size_t  index_;
    };

    //! Return the pointer to the packet.
    /*!
     * Return the pointer to the packet, if the data is available to read;
     * return a nullptr otherwise.
     */

    static inline void * data_ready(pfq_pkt_hdr &h, uint8_t current_commit)
    {
        if (const_cast<volatile uint8_t &>(h.commit) != current_commit)
            return nullptr;
        smp_rmb();
        return &h + 1;
    }

    //! Return a constant pointer to the packet.
    /*!
     * Return a constant pointer to the packet, if the data is available to read;
     * return a nullptr otherwise.
     */

    static inline const void * data_ready(pfq_pkt_hdr const &h, uint8_t current_commit)
    {
        if (const_cast<volatile uint8_t &>(h.commit) != current_commit)
            return nullptr;
        smp_rmb();
        return &h + 1;
    }

} // namespace pfq
