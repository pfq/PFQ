/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#include <tuple>
#include <string>
#include <cstring>
#include <stdexcept>
#include <system_error>

#include <linux/pf_q.h>
#include <sys/ioctl.h>
#include <net/if.h>

#include <pfq-except.hpp>


namespace net {

    using mutable_buffer = std::pair<char *, size_t>;
    using const_buffer   = std::pair<const char *, const size_t>;

    template<size_t N, typename T>
    inline T align(T value)
    {
        static_assert((N & (N-1)) == 0, "align: N not a power of two");
        return (value + (N-1)) & ~(N-1);
    }


    inline int
    ifindex(int fd, const char *dev)
    {
        struct ifreq ifreq_io;
        memset(&ifreq_io, 0, sizeof(struct ifreq));
        strncpy(ifreq_io.ifr_name, dev, IFNAMSIZ);
        if (::ioctl(fd, SIOCGIFINDEX, &ifreq_io) == -1)
            throw pfq_error(errno, "PFQ: ioctl get ifindex");
        return ifreq_io.ifr_ifindex;
    }


    inline
    void set_promisc(int fd, const char *dev, bool value)
    {
        struct ifreq ifreq_io;

        memset(&ifreq_io, 0, sizeof(struct ifreq));
        strncpy(ifreq_io.ifr_name, dev, IFNAMSIZ);

        if(::ioctl(fd, SIOCGIFFLAGS, &ifreq_io) == -1)
            throw pfq_error(errno, "PFQ: ioctl getflags");

        if (value)
            ifreq_io.ifr_flags |= IFF_PROMISC;
        else
            ifreq_io.ifr_flags &= ~IFF_PROMISC;

        if(::ioctl(fd, SIOCSIFFLAGS, &ifreq_io) == -1)
            throw pfq_error(errno, "PFQ: ioctl setflags");

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
            (void)eval { ((get<Ts>(tup) = std::forward<Ts>(arg)),0)... };
        }

    } // namespace param

} // namespace net

