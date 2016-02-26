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

#include <iostream>
#include <sstream>
#include <string>
#include <cstring>
#include <cstddef>
#include <vector>
#include <memory>
#include <array>
#include <tuple>
#include <type_traits>

#include <linux/pf_q.h>
#include <arpa/inet.h>

#include <pfq/lang/util.hpp>

namespace pfq { namespace lang
{
    //
    // sk_buff placeholder
    //

    struct SkBuff { };

    //
    // Monadic Action
    //

    template <typename a>
    struct Action { };

    //
    // Function type constructor
    //

    template <typename Sig>
    struct KFunction
    {
        using type = KFunction<Sig>;
    };

    using NetFunction  = KFunction< Action<SkBuff>(SkBuff) >;
    using NetPredicate = KFunction< bool(SkBuff) >;
    using NetProperty  = KFunction< uint64_t(SkBuff) >;

    // ipv4_t, network byte order type with converting constructor
    //

    struct ipv4_t
    {
        ipv4_t() = default;

        ipv4_t(const char *addr)
        {
            if (inet_pton(AF_INET, addr, &value) <= 0)
                throw std::runtime_error("pfq::lang::ipv4_t");
        }

        uint32_t value;
    };

    inline std::string
    show(ipv4_t value)
    {
        char buff[16];
        if (inet_ntop(AF_INET, &value, buff, sizeof(buff)) == NULL)
            throw std::runtime_error("pfq::lang::inet_ntop");

        return buff;
    }

    inline std::string
    pretty(ipv4_t value)
    {
        return show(value);
    }

    // CIDR: network address + prefix notation.
    //

    struct CIDR
    {
        CIDR() = default;

        CIDR(const char *a, int p)
        : prefix(p)
        {
            if (inet_pton(AF_INET, a, &addr) <= 0)
                throw std::runtime_error("pfq::lang::CIDR");
        }

        CIDR(const char *descr)
        {
            auto slash = strchr(descr, '/');
            if (slash == nullptr)
                throw std::runtime_error("CIDR: bad format (slash missing)");

            std::string a(descr, slash);

            if (inet_pton(AF_INET, a.c_str(), &addr) <= 0)
                throw std::runtime_error("pfq::lang::CIDR");

            prefix = atoi(slash+1);
        }

        uint32_t addr;
        int      prefix;
    };


    inline std::string
    show(CIDR value)
    {
        char buff[16];
        if (inet_ntop(AF_INET, &value.addr, buff, sizeof(buff)) == NULL)
            throw std::runtime_error("pfq::lang::CIDR::inet_ntop");
        return "CIDR{" + std::string{buff} + ',' + std::to_string(value.prefix) + '}';
    }

    inline std::string
    pretty(CIDR value)
    {
        char buff[16];
        if (inet_ntop(AF_INET, &value.addr, buff, sizeof(buff)) == NULL)
            throw std::runtime_error("pfq::lang::CIDR::inet_ntop");
        return std::string{buff} + '/' + std::to_string(value.prefix);
    }


    //
    // pfq-lang DSL...
    //

    template <typename ...Ts>
    struct Property;

    template <typename ...Ts>
    struct Predicate;

    template <typename ...Ts>
    struct Function;

    template <typename F, typename G>
    struct Kleisli;


    template <typename Tp>
    struct is_property : std::false_type
    { };
    template <typename ...Ts>
    struct is_property<Property<Ts...>> : std::true_type
    { };

    template <typename Tp>
    struct is_predicate : std::false_type
    { };
    template <typename ...Ts>
    struct is_predicate<Predicate<Ts...>> : std::true_type
    { };

    template <typename Tp>
    struct is_monadic_function :
        bool_type<is_same_type_constructor<Tp, Function>::value   ||
                  is_same_type_constructor<Tp, Kleisli>::value>
    { };

    //////// Function argument_type class:

    struct funptr_t { } funptr = {};

    struct argument_type
    {
        argument_type()
        : ptr()
        , size()
        , nelem()
        {}

        argument_type(std::nullptr_t)
        : ptr()
        , size()
        , nelem()
        {}

        template <typename Tp, typename std::enable_if<std::is_trivial<Tp>::value >::type * = nullptr>
        argument_type(Tp const &pod)
        : ptr(std::make_shared<StorableShow<Tp>>(pod))
        , size(sizeof(Tp))
        , nelem(static_cast<std::size_t>(-1))
        { }

        template <typename Tp, typename std::enable_if<std::is_trivial<Tp>::value>::type * = nullptr>
        argument_type(std::vector<Tp> const &vec)
        : ptr(std::make_shared<StorableShow<std::vector<Tp>>>(vec))
        , size(sizeof(Tp))
        , nelem(vec.size())
        { }

        argument_type(const char *p)
        : ptr(std::make_shared<StorableShow<std::string>>(p))
        , size(0)
        , nelem(static_cast<std::size_t>(-1))
        { }

        argument_type(std::string str)
        : ptr(std::make_shared<StorableShow<std::string>>(std::move(str)))
        , size(0)
        , nelem(static_cast<std::size_t>(-1))
        { }

        argument_type(std::vector<std::string> const &vec)
        {
            nelem = vec.size();
            ptr   = std::make_shared<StorableShow<std::vector<std::string>>>(std::move(vec));
            size  = 0;
        }

        argument_type(funptr_t, std::size_t n)
        : ptr()
        , size(n)
        , nelem(static_cast<std::size_t>(-1))
        { }

        argument_type(std::shared_ptr<StorableShowBase> p, size_t s, size_t n)
        : ptr(std::move(p))
        , size(s)
        , nelem(n)
        {}

        template <typename Tp, typename std::enable_if<!std::is_trivial<Tp>::value >::type * = nullptr>
        argument_type(Tp const &)
        {
            throw std::logic_error("data: T must be a trivial type!");
        }

        std::shared_ptr<StorableShowBase> ptr;
        size_t size;
        size_t nelem;
    };


    inline std::string
    show(const argument_type &arg)
    {
        std::stringstream out;

        if (!arg.ptr && arg.size == 0 && arg.nelem == 0)  {
            out << "null";
        }
        else if (arg.ptr && arg.size != 0 && arg.nelem == static_cast<size_t>(-1))  {
            out << "data " << arg.ptr->forall_show();
        }
        else if (arg.ptr && arg.nelem != static_cast<size_t>(-1)) {
            out << "vector " << arg.ptr->forall_show();
        }
        else if (arg.ptr && arg.size == 0)  {
            out << "string " << arg.ptr->forall_show();
        }
        else {
            out << "funptr " << arg.size;
        }

        return out.str();
    }

    inline std::string
    pretty(const argument_type &arg)
    {
        if (arg.ptr)
            return arg.ptr->forall_show();
        else if (arg.size)
            return "arg#" + std::to_string(arg.size) + "";
        return "";
    }

    //////// Function descriptor class:

    struct FunctionDescr
    {
        std::string                     symbol;
        std::array<argument_type, 8>    arg;
        std::ptrdiff_t                  index;
        std::ptrdiff_t                  link;
    };


    inline std::string
    show(const FunctionDescr &descr)
    {
        std::string out;
        size_t n = 0;

        out = "FunctionDescr " + descr.symbol + " [";

        for(auto const &a : descr.arg)
        {
            out += n++ ? (' ' + show(a)) : show(a);
        }
        out +=  "] " + std::to_string(descr.index) + " " + std::to_string(descr.link) + ')';

        return out;
    }

    //
    ///////// generic serialize:
    //

    template <typename T>
    argument_type make_argument(T const &x, std::vector<FunctionDescr> const &ser)
    {
        if (ser.empty())
            return argument_type(x);

        return argument_type(funptr, static_cast<std::size_t>(ser[0].index));
    }

    template <typename ...Ts, typename ...Ti>
    std::array<argument_type, 8>
    make_arguments(std::tuple<Ts...> const &args, std::tuple<Ti...> const &ref)
    {
        static_assert(sizeof...(Ts) == 8, "Internal error");

        return std::array<argument_type, 8>
        {{
            make_argument(std::get<0>(args), std::get<0>(ref)),
            make_argument(std::get<1>(args), std::get<1>(ref)),
            make_argument(std::get<2>(args), std::get<2>(ref)),
            make_argument(std::get<3>(args), std::get<3>(ref)),
            make_argument(std::get<4>(args), std::get<4>(ref)),
            make_argument(std::get<5>(args), std::get<5>(ref)),
            make_argument(std::get<6>(args), std::get<6>(ref)),
            make_argument(std::get<7>(args), std::get<7>(ref))
        }};
    }

    //
    // fix the link of this computation
    //

    inline void fix_computation(std::ptrdiff_t n, std::vector<FunctionDescr> &vec)
    {
        for(auto & e : vec)
        {
            if (e.link == n + static_cast<std::ptrdiff_t>(vec.size()))
                e.link = -1;
        }
    }

    //
    // serialize arguments...
    //

    template <typename Ts, typename std::enable_if<!is_monadic_function<Ts>::value>::type * = nullptr>
    inline std::pair<std::vector<FunctionDescr>, std::ptrdiff_t>
    serialize(Ts const &, std::ptrdiff_t n)
    {
        return std::make_pair(std::vector<FunctionDescr>(), n);
    }

    template <typename ...Ts>
    inline std::pair<std::vector<FunctionDescr>, std::ptrdiff_t>
    serialize_all(std::string symb, std::ptrdiff_t n, bool cont, std::tuple<Ts...> const &args_)
    {
        std::vector<FunctionDescr> s1, s2, s3, s4, s5, s6, s7, s8, s9;
        std::ptrdiff_t n1, n2, n3, n4, n5, n6, n7, n8, n9;

        auto args = pfq::lang::tuple_pad<8>(nullptr, args_);

        std::tie(s2, n2) = serialize(std::get<0>(args), n+1);
        std::tie(s3, n3) = serialize(std::get<1>(args), n2);
        std::tie(s4, n4) = serialize(std::get<2>(args), n3);
        std::tie(s5, n5) = serialize(std::get<3>(args), n4);
        std::tie(s6, n6) = serialize(std::get<4>(args), n5);
        std::tie(s7, n7) = serialize(std::get<5>(args), n6);
        std::tie(s8, n8) = serialize(std::get<6>(args), n7);
        std::tie(s9, n9) = serialize(std::get<7>(args), n8);

        std::tie(s1, n1) = std::make_pair
        (
            std::vector<FunctionDescr>
            {
                FunctionDescr { symb,
                    make_arguments(args, std::forward_as_tuple(s2,s3,s4,s5,s6,s7,s8,s9)),
                        n, (cont ? n9 : -1) }
            },

            n+1
        );

        fix_computation(n1, s2);
        fix_computation(n2, s3);
        fix_computation(n3, s4);
        fix_computation(n4, s5);
        fix_computation(n5, s6);
        fix_computation(n6, s7);
        fix_computation(n7, s8);
        fix_computation(n8, s9);

        return std::make_pair(s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9, n9);
    }

    ///////////////////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////////////////////

    //////// Property:

    template <typename ... Ts>
    struct Property : NetProperty
    {
        template <typename ...Tp>
        Property(std::string symbol, Tp && ... args)
        : symbol_(std::move(symbol))
        , args_(std::forward<Tp>(args)...)
        { }

        std::string             symbol_;
        std::tuple<Ts...>       args_;
    };

    template <typename ...Ts>
    Property<typename std::decay<Ts>::type...>
    property(std::string symbol, Ts &&...args)
    {
        return Property<typename std::decay<Ts>::type...>(std::move(symbol), std::forward<Ts>(args)...);
    }

    ///////// pretty property:

    inline std::string
    pretty(Property<> const &p)
    {
       return p.symbol_;
    }

    template <typename ...Ts>
    inline std::string
    pretty(Property<Ts...> const &p)
    {
        std::string ret = '(' + p.symbol_;
        tuple_for_each(p.args_, pretty_tuple(ret));
        return ret + ')';
    }

    ///////// show property:

    inline std::string
    show(Property<> const &p)
    {
       return p.symbol_;
    }

    template <typename ...Ts>
    inline std::string
    show(Property<Ts...> const &p)
    {
        std::string ret = "(Property " + p.symbol_;
        tuple_for_each(p.args_, show_tuple(ret));
        return ret + ')';
    }

    ///////// serialize property:

    template <typename ...Ts>
    inline std::pair<std::vector<FunctionDescr>, std::ptrdiff_t>
    serialize(Property<Ts...> const &p, std::ptrdiff_t n)
    {
        return serialize_all(p.symbol_, n, false, p.args_);
    }

    ///////////////////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////////////////////

    //////// Predicate:

    template <typename ... Ts>
    struct Predicate : NetPredicate
    {
        template <typename ...Tp>
        Predicate(std::string symbol, Tp &&...args)
        : symbol_(std::move(symbol))
        , args_(std::forward<Tp>(args)...)
        { }

        std::string             symbol_;
        std::tuple<Ts...>       args_;
    };

    template <typename ...Ts>
    Predicate<typename std::decay<Ts>::type...>
    predicate(std::string symbol, Ts &&...args)
    {
        return Predicate<typename std::decay<Ts>::type...>(std::move(symbol), std::forward<Ts>(args)...);
    }

    ///////// pretty predicate:

    inline std::string
    pretty(Predicate<> const &p)
    {
       return p.symbol_;
    }

    template <typename ...Ts>
    inline std::string
    pretty(Predicate<Ts...> const &p)
    {
        std::string ret = '(' + p.symbol_;
        tuple_for_each(p.args_, pretty_tuple(ret));
        return ret + ')';
    }

    ///////// show predicate:

    inline std::string
    show(Predicate<> const &p)
    {
       return p.symbol_;
    }

    template <typename ...Ts>
    inline std::string
    show(Predicate<Ts...> const &p)
    {
        std::string ret = "(Predicate " + p.symbol_;
        tuple_for_each(p.args_, show_tuple(ret));
        return ret + ')';
    }

    ///////// serialize predicate:

    template <typename ...Ts>
    inline std::pair<std::vector<FunctionDescr>, std::ptrdiff_t>
    serialize(Predicate<Ts...> const &p, std::ptrdiff_t n)
    {
        return serialize_all(p.symbol_, n, false, p.args_);
    }

    ///////////////////////////////////////////////////////////////////////////////////////
    ///////////////////////////////////////////////////////////////////////////////////////

    //////// Function:

    template <typename ... Ts>
    struct Function : NetFunction
    {
        template <typename ...Tp>
        Function(std::string symbol, Tp &&...args)
        : symbol_(std::move(symbol))
        , args_(std::forward<Tp>(args)...)
        { }

        std::string             symbol_;
        std::tuple<Ts...>       args_;
    };

    template <typename ...Ts>
    Function<typename std::decay<Ts>::type...>
    function(std::string symbol, Ts &&...args)
    {
        return Function<typename std::decay<Ts>::type...>(std::move(symbol), std::forward<Ts>(args)...);
    }

    ///////// pretty function:

    inline std::string
    pretty(Function<> const &f)
    {
       return f.symbol_;
    }

    template <typename ...Ts>
    inline std::string
    pretty(Function<Ts...> const &f)
    {
        std::string ret = '(' + f.symbol_;
        tuple_for_each(f.args_, pretty_tuple(ret));
        return ret + ')';
    }

    ///////// show function:

    inline std::string
    show(Function<> const &f)
    {
       return f.symbol_;
    }

    template <typename ...Ts>
    inline std::string
    show(Function<Ts...> const &f)
    {
        std::string ret = "(Function " + f.symbol_;
        tuple_for_each(f.args_, show_tuple(ret));
        return ret + ')';
    }

    ///////// serialize function:

    template <typename ...Ts>
    inline std::pair<std::vector<FunctionDescr>, std::ptrdiff_t>
    serialize(Function<Ts...> const &p, std::ptrdiff_t n)
    {
        return serialize_all(p.symbol_, n, true, p.args_);
    }

    ///////// serialize vector of functions:

    inline std::pair<std::vector<FunctionDescr>, std::ptrdiff_t>
    serialize (std::vector<Function<>> const &cont, std::ptrdiff_t n)
    {
        std::vector<FunctionDescr> ret, v;

        for(auto & f : cont)
        {
            std::tie(v, n) = serialize(f, n);
            ret = std::move(ret) + v;
        }

        return { ret, n };
    }


    //
    // Kleisli
    //

    template <typename F, typename G> struct kleisly;

    template <typename F, typename G>
    struct Kleisli
    {
        static_assert(is_monadic_function<F>::value, "composition: argument 1 must be a monadic function");
        static_assert(is_monadic_function<G>::value, "composition: argument 2 must be a monadic function");

        using type = typename kleisly<typename F::type, typename G::type>::type;

        F f_;
        G g_;
    };

    template <template <typename > class M, typename A, typename B, typename C>
    struct kleisly< KFunction< M<B>(A) >, KFunction < M<C>(B)> >
    {
        using type = KFunction < M<C>(A) >;
    };

    template <template <typename > class M, typename A, typename B, typename F, typename G>
    struct kleisly< KFunction< M<B>(A) >, Kleisli <F, G> >
    {
        using type = typename kleisly< KFunction<M<B>(A)>, typename kleisly<F,G>::type>::type;
    };

    //
    // Kleisli composition: >>
    //

    template <typename C1,
              typename C2,
              typename = typename kleisly<typename C1::type, typename C2::type>::type>
    inline Kleisli<C1, C2>
    operator>>(C1 c1, C2 c2)
    {
        return { std::move(c1), std::move(c2) };
    }

    template <typename C1, typename C2>
    inline std::string
    pretty(Kleisli<C1,C2> const &comp)
    {
        return pretty(comp.f_) + " >-> " + pretty(comp.g_);
    }

    template <typename C1, typename C2>
    inline std::string
    show(Kleisli<C1,C2> const &comp)
    {
        return "(Kleisli " + show(comp.f_) + " " + show(comp.g_) + ")";
    }

    template <typename C1, typename C2>
    inline std::pair<std::vector<FunctionDescr>, std::size_t>
    serialize(Kleisli<C1, C2> const &f, std::ptrdiff_t n)
    {
        std::vector<FunctionDescr> v1, v2;
        std::size_t n1, n2;

        std::tie(v1, n1) = serialize(f.f_, n);
        std::tie(v2, n2) = serialize(f.g_, n1);

        return { std::move(v1) + std::move(v2), n2 };
    }

} // namespace lang
} // namespace pfq

