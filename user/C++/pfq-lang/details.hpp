/***************************************************************
 *
 * (C) 2014 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#include <pfq-lang/lang.hpp>
#include <functional>

#include <arpa/inet.h>

namespace pfq_lang
{
    namespace details
    {
        // polymorphic lambda are available starting from C++14! In the meanwhile...
        //

        struct hcomp
        {
            template <typename P>
            auto operator()(std::string name, P const &p)
            -> decltype(hcomputation(std::move(name), p))
            {
                return hcomputation(std::move(name), p);
            }
        };

        struct hcomp1
        {
            template <typename P, typename C>
            auto operator()(std::string name, P const &p, C const &c)
            -> decltype(hcomputation1(std::move(name), p, c))
            {
                return hcomputation1(std::move(name), p, c);
            }
        };

        struct hcomp2
        {
            template <typename P, typename C1, typename C2>
            auto operator()(std::string name, P const &p, C1 const &c1, C2 const &c2)
            -> decltype(hcomputation2(std::move(name), p, c1, c2))
            {
                return hcomputation2(std::move(name), p, c1, c2);
            }
        };

        // utility function
        //

        static inline uint32_t
        prefix2mask(size_t n)
        {
            return htonl(~((1ULL << (32-n)) - 1));
        };


        struct network_addr
        {
            uint32_t    addr;
            int         prefix;
        };


        static inline
        struct network_addr
        make_netaddr(const char *net, int prefix)
        {
            network_addr netaddr;

            if (inet_pton(AF_INET, net, &netaddr.addr) <= 0)
                throw std::runtime_error("pfq_lang::net");

            netaddr.prefix = prefix;

            return netaddr;
        }


    } // namespace details

} // namespace pfq_lang
