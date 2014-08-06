/***************************************************************
 *
 * (C) 2014 Nicola Bonelli <nicola@pfq.io>
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

namespace pfq
{
namespace lang
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
        // polymorphic binders (required for C++ < C++14).

        struct polymorphic_hfunction
        {
            template <typename P>
            auto operator()(std::string symb, P const &p)
            -> decltype(hfunction(std::move(symb), p))
            {
                return hfunction(std::move(symb), p);
            }
        };

        struct polymorphic_hfunction1
        {
            template <typename P, typename C>
            auto operator()(std::string symb, P const &p, C const &c)
            -> decltype(hfunction1(std::move(symb), p, c))
            {
                return hfunction1(std::move(symb), p, c);
            }
        };

        struct polymorphic_hfunction2
        {
            template <typename P, typename C1, typename C2>
            auto operator()(std::string symb, P const &p, C1 const &c1, C2 const &c2)
            -> decltype(hfunction2(std::move(symb), p, c1, c2))
            {
                return hfunction2(std::move(symb), p, c1, c2);
            }
        };

        struct polymorphic_hfunction3
        {
            template <typename F>
            auto operator()(std::string symb, F const &fun)
            -> decltype(hfunction3(std::move(symb), fun))
            {
                return hfunction3(std::move(symb), fun);
            }
        };

        struct polymorphic_hfunction4
        {
            template <typename F, typename G>
            auto operator()(std::string symb, F const &f, G const &g)
            -> decltype(hfunction4(std::move(symb), f, g))
            {
                return hfunction4(std::move(symb), f, g);
            }
        };

        struct polymorphic_mfunction2
        {
            template <typename T, typename P>
            auto operator()(std::string symb, const T &arg, P const &p)
            -> decltype(mfunction2(std::move(symb), arg, p))
            {
                return mfunction2(std::move(symb), arg, p);
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
                throw std::runtime_error("pfq::lang::inet_pton");

            netaddr.prefix = prefix;

            return netaddr;
        }

        template <typename CharT, typename Traits>
        typename std::basic_ostream<CharT, Traits> &
        operator<<(std::basic_ostream<CharT,Traits>& out, network_addr const&  that)
        {
            char addr[16];

            if (inet_ntop(AF_INET, &that.addr, addr, sizeof(addr)) == nullptr)
                throw std::runtime_error("pfq::lang::inet_ntop");

            return out << std::string(addr) << '/' << std::to_string(that.prefix);
        }


    } // namespace details

} // namespace lang
} // namespace pfq
