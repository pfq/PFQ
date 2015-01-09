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

namespace pfq { namespace lang
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
        inline uint32_t
        inet_addr(const std::string &addr)
        {
            uint32_t ret;
            if (inet_pton(AF_INET, addr.c_str(), &ret) <= 0)
                throw std::runtime_error("pfq::lang::inet_pton");
            return ret;
        }
    }

    template <typename A, typename Fun>
    inline auto fmap(Fun fun, std::vector<A> const &xs)
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

    //
    // tuple helpers
    //

    namespace details
    {
        template <typename TupleT, typename Fun>
        void tuple_for_each(TupleT &&, Fun, index_sequence<>)
        {
        }

        template <typename TupleT, typename Fun, std::size_t ...S>
        void tuple_for_each(TupleT &&tup, Fun fun, index_sequence<S...>)
        {
            //
            // 8.5.4: Within the initializer-list of a braced-init-list, the initializer-clauses,
            // including any that result from pack expansions (14.5.3), are evaluated in the order in which they appear
            //

            auto sink __attribute__((unused)) = { (fun(std::get<S>(tup)),0)... };
        }
    }

    template <typename TupleT, typename Fun>
    void tuple_for_each(TupleT &&tup, Fun fun)
    {
        details::tuple_for_each(std::forward<TupleT>(tup), fun, make_index_sequence<std::tuple_size<typename std::decay<TupleT>::type>::value>());
    }

    //
    // tuple_const
    //

    template <std::size_t N> struct tuple_const;

    template <std::size_t N>
    struct tuple_const
    {
        template <typename T>
        static auto make(T const & x)
        -> decltype (std::tuple_cat( std::make_tuple(x), tuple_const<N-1>::make(x)))
        {
            return std::tuple_cat( std::make_tuple(x), tuple_const<N-1>::make(x));
        }
    };
    template <>
    struct tuple_const<1>
    {
        template <typename T>
        static std::tuple<T> make(T const & x)
        {
            return std::make_tuple(x);
        }
    };
    template <>
    struct tuple_const<0>
    {
        template <typename T>
        static std::tuple<> make(T const &)
        {
            return std::tuple<>{};
        }
    };

    //
    // tuple_pad
    //

    namespace details
    {
        template <typename Tp, typename ...Ts>
        std::tuple<Ts...>
        tuple_pad(Tp const &, std::tuple<Ts...> const &t, std::integral_constant<std::size_t, 0>)
        {
            return t;
        }

        template <typename Tp, std::size_t N, typename ...Ts>
        auto tuple_pad(Tp const &pad, std::tuple<Ts...> const &t, std::integral_constant<std::size_t, N>)
        -> decltype (std::tuple_cat(t, tuple_const<N>::make(pad)))
        {
            return std::tuple_cat(t, tuple_const<N>::make(pad));
        }
    }

    template <std::size_t N, typename Tp, typename ...Ts>
    auto tuple_pad(Tp const &pad, std::tuple<Ts...> const &tup)
    -> decltype(details::tuple_pad(pad, tup, std::integral_constant<std::size_t, N - sizeof...(Ts)>{}))
    {
        return details::tuple_pad(pad, tup, std::integral_constant<std::size_t, N - sizeof...(Ts)>{});
    }


} // namespace lang
} // namespace pfq
