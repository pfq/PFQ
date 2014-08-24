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
#include <string>
#include <vector>

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

        struct polymorphic_mfunctionP
        {
            template <typename P>
            auto operator()(std::string symb, P const &p)
            -> decltype(mfunctionP(std::move(symb), p))
            {
                return mfunctionP(std::move(symb), p);
            }
        };

        struct polymorphic_mfunctionPF
        {
            template <typename P, typename C>
            auto operator()(std::string symb, P const &p, C const &c)
            -> decltype(mfunctionPF(std::move(symb), p, c))
            {
                return mfunctionPF(std::move(symb), p, c);
            }
        };

        struct polymorphic_mfunctionPFF
        {
            template <typename P, typename C1, typename C2>
            auto operator()(std::string symb, P const &p, C1 const &c1, C2 const &c2)
            -> decltype(mfunctionPFF(std::move(symb), p, c1, c2))
            {
                return mfunctionPFF(std::move(symb), p, c1, c2);
            }
        };

        struct polymorphic_mfunctionF
        {
            template <typename F>
            auto operator()(std::string symb, F const &fun)
            -> decltype(mfunctionF(std::move(symb), fun))
            {
                return mfunctionF(std::move(symb), fun);
            }
        };

        struct polymorphic_mfunctionFF
        {
            template <typename F, typename G>
            auto operator()(std::string symb, F const &f, G const &g)
            -> decltype(mfunctionFF(std::move(symb), f, g))
            {
                return mfunctionFF(std::move(symb), f, g);
            }
        };

        struct polymorphic_mfunction1P
        {
            template <typename T, typename P>
            auto operator()(std::string symb, const T &arg, P const &p)
            -> decltype(mfunction1P(std::move(symb), arg, p))
            {
                return mfunction1P(std::move(symb), arg, p);
            }
        };


        // utility function
        //

        static inline uint32_t
        prefix2mask(size_t n)
        {
            return htonl(static_cast<uint32_t>(~((1ULL << (32-n)) - 1)));
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

        static inline uint32_t inet_addr(const std::string &addr)
        {
            uint32_t ret;
            if (inet_pton(AF_INET, addr.c_str(), &ret) <= 0)
                throw std::runtime_error("pfq::lang::inet_pton");
            return ret;
        }

        template <typename A, typename Fun>
        static inline auto fmap(Fun fun, std::vector<A> const &xs)
            -> std::vector<decltype(fun(xs.front()))>
        {
            std::vector< decltype(fun( xs.front() )) > out;
            out.reserve(xs.size());

            for(auto & x : xs)
            {
                out.push_back(fun(x));
            }

            return out;
        }


    } // namespace details

} // namespace lang
} // namespace pfq
