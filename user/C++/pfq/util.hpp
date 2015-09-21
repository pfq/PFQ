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

#include <vector>
#include <tuple>
#include <string>
#include <cstring>
#include <stdexcept>
#include <system_error>
#include <fstream>
#include <iterator>
#include <thread>
#include <algorithm>

#include <linux/pf_q.h>
#include <linux/if_ether.h>
#include <linux/ip.h>
#include <linux/udp.h>

#include <arpa/inet.h>
#include <sys/ioctl.h>
#include <net/if.h>

#include <pfq/exception.hpp>


namespace pfq {

    using mutable_buffer = std::pair<char *, size_t>;
    using const_buffer   = std::pair<const char *, const size_t>;


    //! Return the value aligned to the next power of two.

    template<size_t N, typename T>
    inline T align(T value)
    {
        static_assert((N & (N-1)) == 0, "align: N not a power of two");
        return (value + static_cast<T>(N-1)) & ~static_cast<T>(N-1);
    }


    //! Given a device name, return the interface index.

    inline int
    ifindex(int fd, const char *dev)
    {
        struct ifreq ifreq_io;
        memset(&ifreq_io, 0, sizeof(struct ifreq));
        strncpy(ifreq_io.ifr_name, dev, IFNAMSIZ);
        if (::ioctl(fd, SIOCGIFINDEX, &ifreq_io) == -1)
            throw pfq_error(errno, ("PFQ: " + std::string (dev) + ": ifindex"));

        return ifreq_io.ifr_ifindex;
    }

    //! Set/unset the promiscuous mode for the given device.

    inline void
    set_promisc(int fd, const char *dev, bool value)
    {
        struct ifreq ifreq_io;

        memset(&ifreq_io, 0, sizeof(struct ifreq));
        strncpy(ifreq_io.ifr_name, dev, IFNAMSIZ);

        if(::ioctl(fd, SIOCGIFFLAGS, &ifreq_io) == -1)
            throw pfq_error(errno, "PFQ: " + std::string(dev) + ": set_promisc");

        if (value)
            ifreq_io.ifr_flags |= IFF_PROMISC;
        else
            ifreq_io.ifr_flags &= ~IFF_PROMISC;

        if(::ioctl(fd, SIOCSIFFLAGS, &ifreq_io) == -1)
            throw pfq_error(errno, "PFQ: " + std::string(dev) + ": set_promisc");
    }

    //! Given the device name return the related index.

    inline size_t
    nametoindex(const char *dev)
    {
        auto i = ::if_nametoindex(dev);
        if (i == 0)
            throw pfq_error(errno, "PFQ: " + std::string(dev) + ": nametoindex");
        return i;
    }

    //! Given the index return the related device name.

    inline std::string
    indextoname(unsigned int i)
    {
        char buf[IF_NAMESIZE];
        if (::if_indextoname(i, buf) == nullptr)
            throw pfq_error(errno, "PFQ: " + std::to_string(i) + ": indextoname");
        return buf;
    }


    //! Trim pending and trailing whitespaces from a string.

    inline std::string
    trim(std::string str)
    {
        auto b = str.find_first_not_of(" \n\r\t");
        auto e = str.find_last_not_of(" \n\r\t");
        b = b == std::string::npos ? 0 : b;
        e = e == std::string::npos ? 0 : (e + 1 - b);
        return str.substr(b, e);
    }

    //! Split a string by means of the given separator.

    inline std::vector<std::string>
    split(std::string str, const char *sep)
    {
        std::vector<std::string> ret;
        auto len = std::strlen(sep);

        for(std::string::size_type n; (n = str.find(sep)) != std::string::npos;)
        {
            ret.push_back(str.substr(0,n));
            str = str.substr(n + len, std::string::npos);
        }

        ret.push_back(std::move(str));
        return ret;
    }

    //! Useful functor map.

    template <typename F, template <typename ...> class Fun, typename V>
    inline auto fmap(F f, Fun<V> const &xs)
    -> Fun<decltype(f(std::declval<V>()))>
    {
        Fun<decltype(f(std::declval<V>()))> out;

        for(auto & x : xs)
            out.push_back(f(x));

        return out;
    }


    //! Hardware concurrency.

    inline unsigned int
    hardware_concurrency()
    {
        auto proc = []() {
            std::ifstream cpuinfo("/proc/cpuinfo");
            return std::count(std::istream_iterator<std::string>(cpuinfo),
                              std::istream_iterator<std::string>(),
                              std::string("processor"));
        };

        auto c =  std::thread::hardware_concurrency();
        return c ? c : static_cast<unsigned int>(proc());
    }


    //! IRQ

    inline std::vector<int>
    get_irq_by_device(const char *dev, const char *kind = "TxRx")
    {
        std::ifstream irq("/proc/interrupts");
        std::vector<int> res;
        std::string vector_name = std::string(dev) + "-" + kind;

        for(std::string line; std::getline(irq, line); )
        {
            auto pos = line.find(vector_name);
            if (pos == std::string::npos)
                continue;
            res.push_back(std::stoi(line));
        }

        return res;
    }


    inline size_t
    get_num_queues(const char *dev, const char *kind = "TxRx")
    {
        return get_irq_by_device(dev, kind).size();
    }


    //! HugePages

    inline std::string
    hugepages_mountpoint()
    {
        std::ifstream ms("/proc/mounts");
        for(std::string line; std::getline(ms, line); )
        {
            auto pos = line.find("hugetlbfs");
            if (pos == std::string::npos)
                continue;
            return split(line, " ").at(1);
        }
        return {};
    }


    //! symmetric hash (TSS).

    inline uint32_t
    symmetric_hash(const char *buf) noexcept
    {
        const char *ptr = buf;

        auto eh = reinterpret_cast<const ethhdr *>(ptr);
        if (eh->h_proto != htons(0x800))
            return 0;

        ptr += sizeof(ethhdr);

        auto ih = reinterpret_cast<const iphdr *>(ptr);
        if (ih->protocol != IPPROTO_TCP &&
            ih->protocol != IPPROTO_UDP)
            return (ih->saddr ^ ih->daddr);

        ptr += sizeof(ih->ihl << 2);

        auto uh = reinterpret_cast<const udphdr *>(ptr);
        return (ih->saddr ^ ih->daddr ^ uh->source ^ uh->dest);
    }


    inline uint32_t
    fold(uint32_t hash, uint32_t n) noexcept
    {
        hash = hash ^ (hash >> 8) ^ (hash >> 16) ^ (hash >> 24);
        return hash % n;

        //
        // switch(n)
        // {
        //     case 1: return 0;
        //     case 2: return hash & 1;
        //     case 3: {
        //         return (hash & 3) != 3 ? (hash & 3) :
        //                ((hash >> 2) & 3) != 3 ? ((hash >> 2) & 3) :
        //                ((hash >> 4) & 3) != 3 ? ((hash >> 4) & 3) :
        //                ((hash >> 6) & 3) != 3 ? ((hash >> 6) & 3) :
        //                ((hash >> 8) & 3) != 3 ? ((hash >> 8) & 3) : 0;
        //     }
        //     case 4: return hash & 3;
        // }

        // return hash % n;
    }

    namespace param
    {
        namespace details
        {
            template <typename ... Ts> struct type_index;
            template <typename T, typename ... Ts>
            struct type_index<T, T, Ts...>
            {
                enum { value = 0 }; // stop recursion here
            };
            template <typename T, typename T0, typename ... Ts>
            struct type_index<T, T0, Ts...>
            {
                enum { value = 1 + type_index<T, Ts...>::value };
            };
            template <typename Tp>
            struct type_index<Tp>
            {
                enum { value = 1 };
            };
        }

        template <typename T, typename ...Ts>
        auto get(std::tuple<Ts...> &tup)
        -> decltype (std::get<details::type_index<T, Ts...>::value>(tup))
        {
            return std::get<details::type_index<T, Ts...>::value>(tup);
        }

        template <typename Tup, typename ...Ts>
        void
        load(Tup &tup, Ts&& ... arg)
        {
            typedef int eval[];
            (void)eval { ((param::get<Ts>(tup) = std::forward<Ts>(arg)),0)... };
        }

    } // namespace param

} // namespace pfq

