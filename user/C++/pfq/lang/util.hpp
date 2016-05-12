/***************************************************************
 *
 * (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
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

#include <iostream>
#include <stdexcept>
#include <sstream>
#include <string>
#include <cstring>
#include <cstddef>
#include <vector>
#include <tuple>
#include <type_traits>
#include <functional>

#include <arpa/inet.h>


namespace pfq { namespace lang
{
    //////// bool_type:

    template <bool Value>
    using bool_type = std::integral_constant<bool, Value>;


    //////// is_same_type_constructor:

    template <typename T, template <typename ...> class Tp>
    struct is_same_type_constructor : std::false_type
    { };

    template <template <typename ...> class Tp, typename ...Ti>
    struct is_same_type_constructor<Tp<Ti...>, Tp> : std::true_type
    { };


    //////// vector concat:

    template <typename Tp>
    inline std::vector<Tp>
    operator+(std::vector<Tp> v1, std::vector<Tp> &&v2)
    {
        v1.insert(v1.end(), std::make_move_iterator(v2.begin()),
                  std::make_move_iterator(v2.end()));
        return v1;
    }

    template <typename Tp>
    inline std::vector<Tp>
    operator+(std::vector<Tp> v1, std::vector<Tp> const &v2)
    {
        v1.insert(v1.end(), v2.begin(), v2.end());
        return v1;
    }

    //////// has_insertion_operator:

    template <typename C> char  has_insertion_test(typename std::remove_reference< decltype((std::cout << std::declval<C>())) >::type *);
    template <typename C> short has_insertion_test(...);

    template <typename T>
    struct has_insertion_operator : bool_type<sizeof(has_insertion_test<T>(0)) == sizeof(char)> {};


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

    //
    // show and pretty
    //

    inline std::string show(std::string const &that)
    {
        return '"' + that + '"';
    }

    template <typename T, typename std::enable_if<has_insertion_operator<T>::value>::type * = nullptr >
    inline std::string show(T const &that)
    {
        std::stringstream out;
        out << that;
        return out.str();
    }

    template <typename T, typename std::enable_if<!has_insertion_operator<T>::value>::type * = nullptr >
    inline std::string show(T const &)
    {
        return "()";
    }

    template <typename T>
    inline std::string show(std::vector<T> const &that)
    {
        std::string out("[");
        size_t n = 0;
        for(auto const &elem : that) {
            out += n++ ? (' ' + show(elem)) : show(elem);
        }
        return out + ']';
    }

    inline std::string pretty(std::string const &that)
    {
        return "\"" + that + "\"";
    }

    template <typename T, typename std::enable_if<has_insertion_operator<T>::value>::type * = nullptr >
    inline std::string pretty(T const &that)
    {
        std::stringstream out;
        out << that;
        return out.str();
    }

    template <typename T, typename std::enable_if<!has_insertion_operator<T>::value>::type * = nullptr >
    inline std::string pretty(T const &)
    {
        return "()";
    }

    template <typename T>
    inline std::string pretty(std::vector<T> const &that)
    {
        std::string out("[");
        size_t n = 0;
        for(auto const &elem : that) {
            out += n++ ? (' ' + pretty(elem)) : pretty(elem);
        }
        return out + ']';
    }


    //
    // Polymorphic tuple printers
    //

    struct pretty_tuple
    {
        pretty_tuple(std::string &s)
        : ref_(s)
        {}

        template <typename Tp>
        void operator()(Tp const &arg)
        {
            ref_ += ' ' + pretty(arg);
        }

        std::string & ref_;
    };

    struct show_tuple
    {
        show_tuple(std::string &s)
        : ref_(s)
        {}

        template <typename Tp>
        void operator()(Tp const &arg)
        {
            ref_ += ' ' + show(arg);
        }

        std::string & ref_;
    };


    //////// StorableShowBase polymorphic class:

    struct StorableShowBase
    {
        virtual std::string forall_show() const = 0;
        virtual void const *forall_addr() const = 0;
        virtual ~StorableShowBase() { }

        static const void *get_addr(std::string const &that)
        {
            return that.c_str();
        }

        template <typename T>
        static const void *get_addr(T const &that)
        {
            return &that;
        }

        template <typename T>
        static const void *get_addr(const std::vector<T> &that)
        {
            return that.data();
        }
    };

    //////// StorableShow class:

    template <typename Tp>
    struct StorableShow final : StorableShowBase
    {
        StorableShow(Tp v)
        : value(std::move(v))
        {}

        Tp value;

        const void *forall_addr() const override
        {
            return get_addr(value);
        }

        std::string forall_show() const override
        {
            return show(value);
        }
    };

    template <>
    struct StorableShow<std::vector<std::string>> final : StorableShowBase
    {
        StorableShow(std::vector<std::string> v)
        : value(std::move(v))
        {
            for(auto & s : value)
               pod.push_back(s.c_str());
        }

        std::vector<std::string> value;
        std::vector<const char *> pod;

        const void *forall_addr() const override
        {
            return pod.data();
        }

        std::string forall_show() const override
        {
            return show(value);
        }
    };

} // namespace lang
} // namespace pfq

