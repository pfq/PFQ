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

#include <stdexcept>
#include <functional>

#include <arpa/inet.h>

namespace pfq_lang
{

#if __cplusplus > 201103L

    template <std::size_t... I>
    using index_sequence = std::index_sequence<I...>;

    template <std::size_t N>
    using make_index_sequence = std::make_index_sequence<N>;

#else
    namespace details
    {
        template <std::size_t ...>
        struct seq { };

        template <std::size_t N, std::size_t ...Xs>
        struct gen_forward : gen_forward<N-1, N-1, Xs...> { };

        template <std::size_t ...Xs>
        struct gen_forward<0, Xs...> {
            typedef seq<Xs...> type;
        };
    }

    template <std::size_t ...I>
    using index_sequence = details::seq<I...>;

    template <std::size_t N>
    using make_index_sequence = typename details::gen_forward<N>::type;

#endif

    namespace details
    {
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
