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

#include <pfq-lang/details.hpp>

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

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    // vector concat (utility):
    //

    template <typename Tp>
    inline std::vector<Tp>
    operator+(std::vector<Tp> v1, std::vector<Tp> &&v2)
    {
        v1.insert(v1.end(),
                  std::make_move_iterator(v2.begin()),
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

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    //
    // Argument
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

    template <typename Sig, typename ... Tp>
    struct Function
    {
        Symbol              symbol;
        Signature           signature;
        std::tuple<Tp...>   arg;
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

    template <typename Tp>
    struct is_Function : std::false_type
    { };

    template <typename S, typename ...Ts>
    struct is_Function<Function<S, Ts...>> : std::true_type
    { };

    //
    // Constructors:
    //

    // monadic functions:

    inline NetFunction<> mfunction(std::string sym, std::string sig)
    {
        return NetFunction<> { std::move(sym), std::move(sig), { } };
    }

    template <typename T>
    inline NetFunction<T> mfunction1(std::string sym, std::string sig, T const &arg)
    {
        return NetFunction<T> { std::move(sym), std::move(sig), std::make_tuple(arg) };
    }

    inline NetFunction<std::string> mfunction2(std::string sym, std::string sig, std::string str)
    {
        return NetFunction<std::string> { std::move(sym), std::move(sig), std::make_tuple(str) };
    }


    template <typename ...Ts>
    inline NetFunction<NetPredicate<Ts...>>
    hfunction(std::string sym, std::string sig, NetPredicate<Ts...> p)
    {
        return NetFunction<NetPredicate<Ts...>> { std::move(sym), std::move(sig), std::make_tuple(p) };
    }

    template <typename ...Ts, typename ...Vs>
    inline NetFunction<NetPredicate<Ts...>, NetFunction<Vs...>>
    hfunction1(std::string sym, std::string sig, NetPredicate<Ts...> p, NetFunction<Vs...> f)
    {
        return NetFunction<NetPredicate<Ts...>, NetFunction<Vs...>> { std::move(sym), std::move(sig), std::make_tuple(p, f) };
    }

    template <typename ...Ts, typename ...Vs, typename ...Ws>
    inline NetFunction<NetPredicate<Ts...>, NetFunction<Vs...>, NetFunction<Ws...>>
    hfunction2(std::string sym, std::string sig, NetPredicate<Ts...> p, NetFunction<Vs...> f, NetFunction<Ws...> g)
    {
        return NetFunction<NetPredicate<Ts...>, NetFunction<Vs...>, NetFunction<Ws...>>{ std::move(sym), std::move(sig), std::make_tuple(p, f, g) };
    }

    // predicates:

    inline NetPredicate<>
    predicate(std::string sym, std::string sig)
    {
        return NetPredicate<>{ std::move(sym), std::move(sig), { } };
    }

    template <typename T>
    inline NetPredicate<T>
    predicate1(std::string sym, std::string sig, const T &arg)
    {
        return NetPredicate<T>{ std::move(sym), std::move(sig), std::make_tuple(arg) };
    }

    template <typename ...Ts>
    inline NetPredicate<NetProperty<Ts...>>
    predicate2(std::string sym, std::string sig, NetProperty<Ts...> p)
    {
        return NetPredicate<NetProperty<Ts...>>{ std::move(sym), std::move(sig), std::make_tuple(p) };
    }

    template <typename T, typename ...Ts>
    inline NetPredicate<NetProperty<Ts...>, T>
    predicate3(std::string sym, std::string sig, NetProperty<Ts...> p, const T &arg)
    {
        return NetPredicate<NetProperty<Ts...>, T>{ std::move(sym), std::move(sig), std::make_tuple(p, arg) };
    }

    // properties:

    inline NetProperty<>
    property(std::string sym, std::string sig)
    {
        return NetProperty<>{ std::move(sym), std::move(sig), { } };
    }

    template <typename T>
    inline NetProperty<T>
    property1(std::string sym, std::string sig, const T &arg)
    {
        return NetProperty<T>{ std::move(sym), std::move(sig), std::make_tuple(arg) };
    }

    // combinators:

    template <typename ...Ts>
    inline NetPredicate<NetPredicate<Ts...>>
    combinator1(std::string sym, std::string sig, NetPredicate<Ts...> p)
    {
        return NetPredicate<NetPredicate<Ts...>>{ std::move(sym), std::move(sig), std::make_tuple(p) };
    }

    template <typename ...Ts, typename ...Vs>
    inline NetPredicate<NetPredicate<Ts...>, NetPredicate<Vs...>>
    combinator2(std::string sym, std::string sig, NetPredicate<Ts...> p1, NetPredicate<Vs...> p2)
    {
        return NetPredicate<NetPredicate<Ts...>, NetPredicate<Vs...>>{ std::move(sym), std::move(sig), std::make_tuple(p1,p2) };
    }

    //
    // polymorphic binders:
    //

    struct FunctionBase
    {
        std::string symbol;
        std::string signature;
    };

    struct Mfunction1 : public FunctionBase
    {
        Mfunction1(std::string sym, std::string sig)
        : FunctionBase { std::move(sym), std::move(sig) }
        { }

        template <typename Tp>
        NetFunction<Tp>
        operator()(const Tp &arg)
        {
            return mfunction1(symbol, signature, arg);
        }
    };

    struct Mfunction2: public FunctionBase
    {
        Mfunction2(std::string sym, std::string sig)
        : FunctionBase { std::move(sym), std::move(sig) }
        { }

        NetFunction<std::string>
        operator()(std::string arg)
        {
            return mfunction2(symbol, signature, std::move(arg));
        }
    };

    struct HFunction : public FunctionBase
    {
        HFunction(std::string sym, std::string sig)
        : FunctionBase { std::move(sym), std::move(sig) }
        { }

        template <typename ...Ts>
        NetFunction<NetPredicate<Ts...>>
        operator()(NetPredicate<Ts...> p)
        {
            return hfunction(symbol, signature, p);
        }
    };

    struct HFunction1 : public FunctionBase
    {
        HFunction1(std::string sym, std::string sig)
        : FunctionBase { std::move(sym), std::move(sig) }
        { }

        template <typename ...Ts, typename ...Vs>
        NetFunction<NetPredicate<Ts...>, NetFunction<Vs...>>
        operator()(NetPredicate<Ts...> p, NetFunction<Vs...> f)
        {
            return hfunction1(symbol, signature, p, f);
        }
    };

    struct HFunction2 : public FunctionBase
    {
        HFunction2(std::string sym, std::string sig)
        : FunctionBase { std::move(sym), std::move(sig) }
        { }

        template <typename ...Ts, typename ...Vs, typename ...Ws>
        NetFunction<NetPredicate<Ts...>, NetFunction<Vs...>, NetFunction<Ws...>>
        operator()(NetPredicate<Ts...> p, NetFunction<Vs...> f, NetFunction<Ws...> g)
        {
            return hfunction2(symbol, signature, p, f, g);
        }
    };

    struct Combinator1 : public FunctionBase
    {
        Combinator1(std::string sym, std::string sig)
        : FunctionBase { std::move(sym), std::move(sig) }
        { }

        template <typename ...Ts>
        NetPredicate<NetPredicate<Ts...>>
        operator()(NetPredicate<Ts...> p)
        {
            return combinator1(symbol, signature, p);
        }
    };

    struct Combinator2 : public FunctionBase
    {
        Combinator2(std::string sym, std::string sig)
        : FunctionBase { std::move(sym), std::move(sig) }
        { }

        template <typename ...Ts, typename ...Vs>
        NetPredicate<NetPredicate<Ts...>, NetPredicate<Vs...>>
        operator()(NetPredicate<Ts...> p1, NetPredicate<Vs...> p2)
        {
            return combinator2(symbol, signature, p1, p2);
        }
    };

    struct Property1 : public FunctionBase
    {
        Property1(std::string sym, std::string sig)
        : FunctionBase { std::move(sym), std::move(sig) }
        { }

        template <typename Tp>
        NetProperty<Tp>
        operator()(const Tp &arg)
        {
            return property1(symbol, signature, arg);
        }
    };

    struct Predicate1 : public FunctionBase
    {
        Predicate1(std::string sym, std::string sig)
        : FunctionBase { std::move(sym), std::move(sig) }
        { }

        template <typename Tp>
        NetPredicate<Tp>
        operator()(const Tp &arg)
        {
            return predicate1(symbol, signature, arg);
        }
    };

    struct Predicate2 : public FunctionBase
    {
        Predicate2(std::string sym, std::string sig)
        : FunctionBase { std::move(sym), std::move(sig) }
        { }

        template <typename ...Ts>
        NetPredicate<NetProperty<Ts...>>
        operator()(NetProperty<Ts...> p)
        {
            return predicate2(symbol, signature, p);
        }
    };

    struct Predicate3 : public FunctionBase
    {
        Predicate3(std::string sym, std::string sig)
        : FunctionBase { std::move(sym), std::move(sig) }
        { }

        template <typename Tp, typename ...Ts>
        NetPredicate<NetProperty<Ts...>>
        operator()(NetProperty<Ts...> p, const Tp &arg)
        {
            return predicate3(symbol, signature, p, arg);
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

        return Function< M<C>(A), F, G> { ">->", "", std::make_tuple(f, g) };
    }

    //
    // pretty:
    //

    template <typename Tp,
              typename std::enable_if< is_Function<Tp>::value >::type * = nullptr >
    void prettyArg(std::string &out, Tp const &arg, std::string const &sep, bool enable)
    {
        if (enable)
            out += sep;
        out += pretty(arg);
    }

    template <typename Tp,
              typename std::enable_if<std::is_function<Tp>::value>::type * = nullptr>
    void prettyArg(std::string &, Tp const &, std::string const &, bool)
    {
    }

    template <typename Tp,
              typename std::enable_if<!std::is_function<Tp>::value && !is_Function<Tp>::value >::type * = nullptr>
    void prettyArg(std::string &out, Tp const &arg, std::string const &sep, bool enable)
    {
        std::stringstream s; s << arg;
        if (enable)
            out += sep;
        out += s.str();
    }

    template <typename Tuple, std::size_t ...S>
    void prettyTuple(std::string &out, index_sequence<S...>, Tuple const & t, std::string const &sep = " ")
    {
        int expand[] = { (prettyArg(out, std::get<S>(t), sep, S != 0),0)... };
        (void)expand;
    }

    template <typename ...Ts>
    static inline std::string
    pretty(NetPredicate<Ts...> const & f)
    {
        std::string out;

        if (sizeof...(Ts) > 0) {
            out = '(' + f.symbol + ' ';
            prettyTuple(out, make_index_sequence<sizeof...(Ts)>{}, f.arg);
            out += ')';
        }
        else {
            out = f.symbol;
        }
        return out;
    }

    template <typename ...Ts>
    static inline std::string
    pretty(NetProperty<Ts...> const & f)
    {
        std::string out;

        if (sizeof...(Ts) > 0) {
            out = '(' + f.symbol + ' ';
            prettyTuple(out, make_index_sequence<sizeof...(Ts)>{}, f.arg);
            out += ')';
        }
        else {
            out = f.symbol;
        }
        return out;
    }

    template <typename ...Ts>
    static inline std::string
    pretty(NetFunction<Ts...> const & f)
    {
        std::string out;

        if (sizeof...(Ts) > 0) {
            if (f.symbol == ">->")
            {
                prettyTuple(out, make_index_sequence<sizeof...(Ts)>{}, f.arg, " >-> ");
            }
            else {
                out = '(' + f.symbol + ' ';
                prettyTuple(out, make_index_sequence<sizeof...(Ts)>{}, f.arg, " ");
                out += ')';
            }
        }
        else {
            out = f.symbol;
        }
        return out;
    }

} // namespace pfq_lang
