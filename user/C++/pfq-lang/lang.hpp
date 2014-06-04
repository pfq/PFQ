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

#include <sstream>
#include <string>
#include <cstring>
#include <vector>
#include <memory>
#include <type_traits>

#include <linux/pf_q.h>

#include "details.hpp"

namespace pfq_lang
{
    using ::pfq_functional_descr;

    static inline std::string
    show(pfq_functional_descr const &descr)
    {
        std::stringstream out;

        out << "functional_descr "
            << "symbol:"    << descr.symbol     << ' '
            << "signature:" << descr.signature  << ' '
            << "arg:{ ";

        for(auto const &arg : descr.arg)
        {
            out << '(' << arg.ptr << ',' << arg.size << ") ";
        }

        out << "} " << "left:"  << descr.left << ' '
                    << "right:" << descr.right;

        return out.str();
    }

    //
    // Argument and FunctionDescr
    //

    struct Showbase
    {
        virtual std::string forall_show() = 0;
    };

    template <typename Tp>
    struct Showable : Showbase
    {
        Showable(Tp v)
        : value(std::move(v))
        {}

        Tp value;

        Tp *get()
        {
            return &value;
        }

        Tp const *get() const
        {
            return &value;
        }

        std::string forall_show() override
        {
            std::stringstream out;
            out << value;
            return out.str();
        }
    };

    struct Argument
    {
        static Argument Null()
        {
            return Argument{ std::shared_ptr<Showbase>(), 0};
        }

        template <typename Tp>
        static Argument Data(Tp const &pod)
        {
            static_assert( std::is_pod<Tp>::value, "Data argument must be a pod type");

            auto ptr = std::make_shared<Showable<Tp>>(pod);

            return Argument{ std::dynamic_pointer_cast<Showbase>(ptr), sizeof(pod) };
        }

        static Argument String(std::string str)
        {
            auto ptr = std::make_shared<Showable<std::string>>(std::move(str));
            return Argument{ std::dynamic_pointer_cast<Showbase>(ptr), 0 };
        }

        static Argument Fun(int n)
        {
            return Argument{ std::shared_ptr<Showbase>(), static_cast<size_t>(n) };
        }

        std::shared_ptr<Showbase> ptr;
        size_t size;
    };


    static inline std::string
    show(const Argument &arg)
    {
        std::stringstream out;

        if (!arg.ptr && arg.size == 0)  {
            out << "ArgNull";
        }
        else if (arg.ptr && arg.size != 0)  {
            out << "ArgData (" << arg.ptr->forall_show() << "," << arg.size << ")";
        }
        else if (arg.ptr && arg.size == 0)  {
            out << "ArgString '" << arg.ptr->forall_show() << "'";
        }
        else {
            out << "ArgFun " << arg.size;
        }

        return out.str();
    }

    static inline std::string
    pretty(const Argument &arg)
    {
        if (arg.ptr)
            return arg.ptr->forall_show();
        else if (arg.size)
            return "f[" + std::to_string(arg.size) + "]";

        return "";
    }

    //
    // FunctionDescr
    //

    struct FunctionDescr
    {
        std::string         symbol;
        std::string         signature;
        Argument            arg[4];
        int                 left;
        int                 right;
    };

    static inline std::string
    show(const FunctionDescr &descr)
    {
        std::string out;

        out = "FunctionDescr " + descr.symbol + ' '
                               + descr.signature + " [";

        for(auto const &a : descr.arg)
        {
            out += show(a) + ", ";
        }
        out +=  "] ("
                + std::to_string(descr.left)  + ", "
                + std::to_string(descr.right) + ')';

        return out;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    using Symbol    = std::string;
    using Signature = std::string;

    struct SkBuff { };

    template <typename a>
    struct Action { };

    template <typename Sig, typename ... Elem>
    struct Function
    {
        Symbol                symbol;
        Signature             signature;
        std::vector<Argument> arg;

        std::tuple<Elem...>   elem;
    };

    //
    // Aliases...
    //

    template <typename ...Ts>
    using NetFunction  = Function< Action<SkBuff>(SkBuff), Ts...>;

    template <typename ...Ts>
    using NetPredicate = Function< bool(SkBuff), Ts...>;

    template <typename ...Ts>
    using NetProperty  = Function< uint64_t(SkBuff), Ts...>;

    //
    // Constructors:
    //

    // monadic functions:

    NetFunction<> mfunction(std::string sym, std::string sig)
    {
        return NetFunction<> { std::move(sym), std::move(sig), { }, { } };
    }

    template <typename T>
    NetFunction<> mfunction1(std::string sym, std::string sig, T const &arg)
    {
        return NetFunction<> { std::move(sym), std::move(sig), { Argument::Data(arg) } , { } };
    }

    NetFunction<> mfunction2(std::string sym, std::string sig, std::string str)
    {
        return NetFunction<> { std::move(sym), std::move(sig), { Argument::String(std::move(str)) } , { } };
    }

    template <typename ...Ts>
    NetFunction<NetPredicate<Ts...>>
    hfunction(std::string sym, std::string sig, NetPredicate<Ts...> p)
    {
        return NetFunction<NetPredicate<Ts...>> { std::move(sym), std::move(sig), { }, std::make_tuple(p) };
    }

    template <typename ...Ts, typename ...Vs>
    NetFunction<NetPredicate<Ts...>, NetFunction<Vs...>>
    hfunction1(std::string sym, std::string sig, NetPredicate<Ts...> p, NetFunction<Vs...> f)
    {
        return NetFunction<NetPredicate<Ts...>, NetFunction<Vs...>> { std::move(sym), std::move(sig), { }, std::make_tuple(p, f) };
    }

    template <typename ...Ts, typename ...Vs, typename ...Ws>
    NetFunction<NetPredicate<Ts...>, NetFunction<Vs...>, NetFunction<Ws...>>
    hfunction2(std::string sym, std::string sig, NetPredicate<Ts...> p, NetFunction<Vs...> f, NetFunction<Ws...> g)
    {
        return NetFunction<NetPredicate<Ts...>, NetFunction<Vs...>, NetFunction<Ws...>>{ std::move(sym), std::move(sig), { }, std::make_tuple(p, f, g) };
    }

    // predicates:

    NetPredicate<>
    predicate(std::string sym, std::string sig)
    {
        return NetPredicate<>{ std::move(sym), std::move(sig), { } , { } };
    }

    template <typename T>
    NetPredicate<>
    predicate1(std::string sym, std::string sig, const T &arg)
    {
        return NetPredicate<>{ std::move(sym), std::move(sig), { Argument::Data(arg) }, { } };
    }

    template <typename ...Ts>
    NetPredicate<NetProperty<Ts...>>
    predicate2(std::string sym, std::string sig, NetProperty<Ts...> p)
    {
        return NetPredicate<NetProperty<Ts...>>{ std::move(sym), std::move(sig), { } , std::make_tuple(p) };
    }

    template <typename T, typename ...Ts>
    NetPredicate<NetProperty<Ts...>>
    predicate3(std::string sym, std::string sig, NetProperty<Ts...> p, const T &arg)
    {
        return NetPredicate<NetProperty<Ts...>>{ std::move(sym), std::move(sig), { Argument::Data(arg) }, std::make_tuple(p) };
    }

    // properties:

    NetProperty<>
    property(std::string sym, std::string sig)
    {
        return NetProperty<>{ std::move(sym), std::move(sig), { }, { } };
    }

    template <typename T>
    NetProperty<>
    property1(std::string sym, std::string sig, const T &arg)
    {
        return NetProperty<>{ std::move(sym), std::move(sig), { Argument::Data(arg) }, { } };
    }

    // combinators:

    template <typename ...Ts>
    NetPredicate<NetPredicate<Ts...>>
    combinator1(std::string sym, std::string sig, NetPredicate<Ts...> p)
    {
        return NetPredicate<NetPredicate<Ts...>>{ std::move(sym), std::move(sig), { }, std::make_tuple(p) };
    }

    template <typename ...Ts, typename ...Vs>
    NetPredicate<NetPredicate<Ts...>, NetPredicate<Vs...>>
    combinator2(std::string sym, std::string sig, NetPredicate<Ts...> p1, NetPredicate<Vs...> p2)
    {
        return NetPredicate<NetPredicate<Ts...>, NetPredicate<Vs...>>{ std::move(sym), std::move(sig), { }, std::make_tuple(p1,p2) };
    }

    //
    // polymorphic binders:
    //

    using Bind = details::polymorphic_bind<std::string, std::string>;

    struct Mfunction1: Bind
    {
        Mfunction1(std::string sym, std::string sig)
        : Bind { std::move(sym), std::move(sig) }
        { }

        template <typename Tp>
        NetFunction<>
        operator()(const Tp &arg)
        {
            return this->apply(mfunction1<Tp>, arg);
        }
    };

    struct Mfunction2: Bind
    {
        Mfunction2(std::string sym, std::string sig)
        : Bind { std::move(sym), std::move(sig) }
        { }

        NetFunction<>
        operator()(std::string arg)
        {
            return this->apply(mfunction2, std::move(arg));
        }
    };

    struct HFunction : Bind
    {
        HFunction(std::string sym, std::string sig)
        : Bind { std::move(sym), std::move(sig) }
        { }

        template <typename ...Ts>
        NetFunction<NetPredicate<Ts...>>
        operator()(NetPredicate<Ts...> p)
        {
            return this->apply(hfunction<Ts...>, p);
        }
    };

    struct HFunction1 : Bind
    {
        HFunction1(std::string sym, std::string sig)
        : Bind { std::move(sym), std::move(sig) }
        { }

        template <typename ...Ts, typename ...Vs>
        NetFunction<NetPredicate<Ts...>, NetFunction<Vs...>>
        operator()(NetPredicate<Ts...> p, NetFunction<Vs...> f)
        {
            return this->apply(hfunction1<Ts..., Vs...>, p, f);
        }
    };

    struct HFunction2 : Bind
    {
        HFunction2(std::string sym, std::string sig)
        : Bind { std::move(sym), std::move(sig) }
        { }

        template <typename ...Ts, typename ...Vs, typename ...Ws>
        NetFunction<NetPredicate<Ts...>, NetFunction<Vs...>, NetFunction<Ws...>>
        operator()(NetPredicate<Ts...> p, NetFunction<Vs...> f, NetFunction<Ws...> g)
        {
            return this->apply(hfunction2<Ts..., Vs..., Ws...>, p, f, g);
        }
    };

    struct Combinator1 : Bind
    {
        Combinator1(std::string sym, std::string sig)
        : Bind { std::move(sym), std::move(sig) }
        { }

        template <typename ...Ts>
        NetPredicate<NetPredicate<Ts...>>
        operator()(NetPredicate<Ts...> p)
        {
            return this->apply(combinator1<Ts...>, p);
        }
    };

    struct Combinator2 : Bind
    {
        Combinator2(std::string sym, std::string sig)
        : Bind { std::move(sym), std::move(sig) }
        { }

        template <typename ...Ts, typename ...Vs>
        NetPredicate<NetPredicate<Ts...>, NetPredicate<Vs...>>
        operator()(NetPredicate<Ts...> p1, NetPredicate<Vs...> p2)
        {
            return apply(combinator2<Ts..., Vs...>, p1, p2);
        }
    };

    struct Property1 : Bind
    {
        Property1(std::string sym, std::string sig)
        : Bind { std::move(sym), std::move(sig) }
        { }

        template <typename Tp>
        NetProperty<>
        operator()(const Tp &arg)
        {
            return this->apply(property1<Tp>, arg);
        }
    };

    struct Predicate1 : Bind
    {
        Predicate1(std::string sym, std::string sig)
        : Bind { std::move(sym), std::move(sig) }
        { }

        template <typename Tp>
        NetPredicate<>
        operator()(const Tp &arg)
        {
            return this->apply(predicate1<Tp>, arg);
        }
    };

    struct Predicate2 : Bind
    {
        Predicate2(std::string sym, std::string sig)
        : Bind { std::move(sym), std::move(sig) }
        { }

        template <typename ...Ts>
        NetPredicate<NetProperty<Ts...>>
        operator()(NetProperty<Ts...> p)
        {
            return this->apply(predicate2<Ts...>, p);
        }
    };

    struct Predicate3 : Bind
    {
        Predicate3(std::string sym, std::string sig)
        : Bind { std::move(sym), std::move(sig) }
        { }

        template <typename Tp, typename ...Ts>
        NetPredicate<NetProperty<Ts...>>
        operator()(NetProperty<Ts...> p, const Tp &arg)
        {
            return this->apply(predicate3<Tp, Ts...>, p, arg);
        }
    };

    //
    // Kleisli composition:
    //

    template <template <typename > class M, typename A, typename B, typename C, typename ...Ts, typename ...Vs>
    auto operator>>(Function< M<B>(A), Ts...> f, Function< M<C>(B), Vs...> g)
    -> decltype (compose(f, g))
    {
        return compose(f,g);
    }

    template <template <typename > class M, typename A, typename B, typename C, typename ...Ts, typename ...Vs>
    Function< M<C>(A), Function<M<B>(A), Ts...>,
                       Function<M<C>(B), Vs...>>
    compose(Function<M<B>(A), Ts...> f, Function<M<C>(B), Vs...> g)
    {
        using F = Function<M<B>(A), Ts...>;
        using G = Function<M<C>(B), Vs...>;

        return Function< M<C>(A), F, G> { "", "", {}, std::make_tuple(f, g) };
    }

    //
    // pretty:
    //

    static inline std::string
    pretty(NetFunction<> const &f)
    {
        if (f.arg.empty())
            return f.symbol;

        std::string out = "(" + f.symbol + ' ';
        for(auto &a : f.arg)
        {
            out += pretty(a) + ' ';
        }

        return out + ')';
    }

    static inline std::string
    pretty(NetProperty<> const &p)
    {
        if (p.arg.empty())
            return p.symbol;

        std::string out =  '(' + p.symbol + ' ';
        for(auto &a : p.arg)
        {
            out += pretty(a) + ' ';
        }
        return out + ')';
    }


    static inline std::string
    pretty(NetPredicate<> const &p)
    {
        if (p.arg.empty())
            return p.symbol;

        std::string out =  '(' + p.symbol + ' ';
        for(auto &a : p.arg)
        {
            out += pretty(a) + ' ';
        }
        return out + ')';
    }

    template <typename ...Ts, typename ...Vs>
    static inline std::string
    pretty(NetFunction<NetFunction<Ts...>, NetFunction<Vs...>> const &f)
    {
        return pretty (std::get<0>(f.elem) ) + " >-> " + pretty (std::get<1>(f.elem));
    }

    template <typename ...Ts>
    static inline std::string
    pretty(NetFunction<NetPredicate<Ts...>> const &f)
    {
        return '(' + f.symbol + ' ' + pretty( std::get<0>(f.elem)) + ')';
    }

    template <typename ...Ts, typename ...Vs>
    static inline std::string
    pretty(NetFunction<NetPredicate<Ts...>, NetFunction<Vs...>> const &f)
    {
        return '(' + f.symbol + ' ' + pretty(std::get<0>(f.elem)) + ' ' + pretty(std::get<1>(f.elem)) + ')';
    }

    template <typename ...Ts, typename ...Vs, typename ...Ws>
    static inline std::string
    pretty(NetFunction<NetPredicate<Ts...>, NetFunction<Vs...>, NetFunction<Ws...>> const &f)
    {
        return '(' + f.symbol + ' ' + pretty(std::get<0>(f.elem)) + ' ' + pretty(std::get<1>(f.elem)) + ' ' + pretty(std::get<2>(f.elem)) + ')';
    }

    static inline std::string
    pretty(NetPredicate<NetProperty<>> const &p)
    {
        return '(' + p.symbol + ' ' + pretty(std::get<0>(p.elem)) + ')';
    }

    template <typename ...Ts>
    static inline std::string
    pretty(NetPredicate<NetPredicate<Ts...>> const &p)
    {
        return '(' + p.symbol + ' ' + pretty(std::get<0>(p.elem)) + ')';
    }

    template <typename ...Ts, typename ...Vs>
    static inline std::string
    pretty(NetPredicate<NetPredicate<Ts...>, NetPredicate<Vs...>> const &p)
    {
        return '(' + pretty(std::get<0>(p.elem)) + ' ' + p.symbol + ' ' + pretty(std::get<1>(p.elem)) + ')';
    }


} // namespace pfq_lang
