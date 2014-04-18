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

#include <sstream>
#include <string>
#include <cstring>
#include <vector>
#include <memory>
#include <type_traits>

#include <linux/pf_q.h>


namespace pfq_lang
{
    using ::pfq_functional_type;
    using ::pfq_functional_descr;

    static inline std::string
    show(enum pfq_functional_type ft)
    {
        switch(ft)
        {
            case pfq_monadic_fun:    return "fun";
            case pfq_high_order_fun: return "hfun";
            case pfq_predicate_fun:  return "pred";
            case pfq_combinator_fun: return "comb";
        }

        throw std::logic_error("unknown type");
    }

    static inline std::string
    show(pfq_functional_descr const &descr)
    {
        std::stringstream out;

        out << "functional_descr "
            << "type:"      << show (descr.type) << ' '
            << "symbol:"    << descr.symbol     << ' '
            << "arg_ptr:"   << descr.arg_ptr    << ' '
            << "arg_size:"  << descr.arg_size   << ' '
            << "left:"      << descr.l_index    << ' '
            << "right:"     << descr.r_index;

        return out.str();
    }

    //
    // Functional descriptor
    //

    struct FunDescr
    {
        enum pfq_functional_type    type;
        std::string                 symbol;

        std::shared_ptr<void>       arg_ptr;
        size_t                      arg_size;

        int                         left;
        int                         right;
    };

    static inline std::string
    show(FunDescr const &descr)
    {
        return "FunDescr { " + show(descr.type) + ' '
                             + descr.symbol + ' '
                             + std::to_string((unsigned long)descr.arg_ptr.get()) + ' '
                             + std::to_string(descr.arg_size) + ' '
                             + std::to_string(descr.left) + ' '
                             + std::to_string(descr.right) + " }";
    }

    static inline void
    relinkFunDescr(int n, int m, FunDescr &descr)
    {
        if (descr.left == n)
            descr.left = m;
        if (descr.right == n)
            descr.right = m;
    }

    ///////////////////////////////////////////////////

    namespace term
    {
        //////// is_same_template:

        template <typename T, template <typename ...> class Tp>
        struct is_same_template : std::false_type
        { };

        template <template <typename ...> class Tp, typename ...Ti>
        struct is_same_template<Tp<Ti...>, Tp> : std::true_type
        { };


        //////// Combinator:

        struct Combinator
        {
            std::string name;
        };

        static inline std::string
        show(Combinator const &comb)
        {
            if (comb.name == "or")
                return "|";
            if (comb.name == "and")
                return "&";
            if (comb.name == "xor")
                return "^";

            throw std::logic_error("unknown");
        }

        static inline std::pair<std::vector<FunDescr>, int>
        serialize(int n, Combinator const &comb)
        {
            return std::make_pair(std::vector<FunDescr>
            {
                FunDescr { pfq_combinator_fun, comb.name, std::shared_ptr<void>(), 0, -1, -1 }
            }, n+1);
        }

        //////// Predicates:

        struct Pred
        {
            std::string name_;
        };

        struct Pred1
        {
            template <typename T>
            Pred1(std::string name, T const &arg)
            : name_(std::move(name))
            , ptr_(std::shared_ptr<void>{ new T(arg) })
            , size_(sizeof(T))
            {
            }

            std::string           name_;
            std::shared_ptr<void> ptr_;
            size_t                size_;
        };

        template <typename P1, typename P2>
        struct Pred2
        {
            Combinator      comb_;
            P1              left_;
            P2              right_;
        };

        //////// is_predicate:

        template <typename Tp>
        struct is_predicate :
            std::integral_constant<bool,
                std::is_same<Tp, term::Pred>::value  ||
                std::is_same<Tp, term::Pred1>::value ||
                is_same_template<Tp, term::Pred2>::value>
        { };

        ///////// show predicates:

        static inline std::string
        show(Pred const &descr)
        {
            return descr.name_;
        }

        static inline std::string
        show(Pred1 const &descr)
        {
            std::ostringstream out;
            out << '(' << descr.name_ << ' ' << descr.ptr_.get() << ':' << std::to_string(descr.size_) << ')';
            return out.str();
        }

        template <typename P1, typename P2>
        static inline std::string
        show(Pred2<P1,P2> const &descr)
        {
            return '(' + show(descr.left_) + ' ' + show(descr.comb_) + ' ' + show(descr.right_) + ')';
        }

        //////// serialize predicates:

        static inline std::pair<std::vector<FunDescr>, int>
        serialize(int n, Pred const &p)
        {
            return std::make_pair(std::vector<FunDescr>
            {
                FunDescr { pfq_predicate_fun, p.name_, std::shared_ptr<void>(), 0, -1, -1 }
            }, n+1);
        }

        static inline std::pair<std::vector<FunDescr>, int>
        serialize(int n, Pred1 const &p)
        {
            return std::make_pair(std::vector<FunDescr>
            {
                FunDescr { pfq_predicate_fun, p.name_, p.ptr_, p.size_, -1, -1 }
            }, n+1);
        }

        template <typename  P1, typename P2>
        static inline std::pair<std::vector<FunDescr>, int>
        serialize(int n, Pred2<P1,P2> const &p)
        {
            std::vector<FunDescr> ret, left, right;
            int n1, n2, n3;

            std::tie(ret, n1)   = serialize(n, p.comb_);
            std::tie(left, n2)  = serialize(n1, p.left_);
            std::tie(right, n3) = serialize(n2, p.left_);

            ret[0].left = n1;
            ret[0].right = n2;

            std::move(left.begin(), left.end(),   std::back_inserter(ret));
            std::move(right.begin(), right.end(), std::back_inserter(ret));

            return std::make_pair(ret, n3);
        }

    } // namespace term


    inline term::Combinator
    combinator(std::string name)
    {
        return term::Combinator{ std::move(name) };
    }

    inline term::Pred
    predicate(std::string name)
    {
        return term::Pred{ std::move(name) };
    }

    template <typename T>
    inline typename std::enable_if<
          std::is_pod<T>::value,
    term::Pred1>::type
    predicate(std::string name, const T &arg)
    {
        return term::Pred1{ std::move(name), arg };
    }

    template <typename P1, typename P2>
    inline typename std::enable_if<
        term::is_predicate<P1>::value &&
        term::is_predicate<P2>::value,
    term::Pred2<P1,P2>>::type
    predicate(term::Combinator c, P1 const &left, P2 const &right)
    {
        return term::Pred2<P1,P2>{ c, left, right };
    }

    //
    // TODO
    //

    // struct QFunction
    // {
    //     std::string name;
    //     std::pair<std::shared_ptr<char>, size_t> arg;

    //     std::vector<QFunction>
    //     operator()() const
    //     {
    //         return std::vector<QFunction> { *this };
    //     }
    // };

   //  //
   //  // Computation
   //  //

   //  template <typename Comp>
   //  struct Computation
   //  {
   //      Comp left;
   //      QFunction right;

   //      std::vector<QFunction>
   //      operator()() const
   //      {
   //          auto l = left();
   //          auto r = right();

   //          l.reserve(l.size() + r.size());
   //          std::move(std::begin(r), std::end(r), std::back_inserter(l));

   //          return l;
   //      }
   //  };

   //  inline std::string
   //  show(QFunction const &fun)
   //  {
   //      std::stringstream out;

   //      out << fun.name;
   //      if (fun..first)
   //          out << ' ' << fun..second << "@" << (void *)fun.context.first.get();

   //      return out.str();
   //  }

   //  template <typename Tp>
   //  inline std::string
   //  show(Computation<Tp> const &comp)
   //  {
   //      return show(comp.left) + " >-> " + show(comp.right);
   //  }

   //  //
   //  // evaluate a computation, creating a pfq_meta_prog.
   //  //

   //  template <typename Comp>
   //  inline std::unique_ptr<pfq_meta_prog>
   //  eval(Comp &comp)
   //  {
   //      return eval(comp());
   //  }

   //  inline std::unique_ptr<pfq_meta_prog>
   //  eval(std::vector<QFunction> const &vec)
   //  {
   //      auto prg = reinterpret_cast<pfq_meta_prog *>(std::malloc(sizeof(pfq_meta_prog) + sizeof(pfq_fun_t) * sizeof(vec)));

   //      prg->size = vec.size();

   //      for(unsigned int i = 0; i < prg->size; ++i)
   //      {
   //          prg->fun[i].symbol       = vec[i].name.c_str();
   //          prg->fun[i]..addr = vec[i].context.first.get();
   //          prg->fun[i]..size = vec[i].context.second;
   //      }

   //      return std::unique_ptr<pfq_meta_prog>(prg);
   //  }

   //  //
   //  // generic function constructor
   //  //

   //  inline QFunction
   //  qfun(std::string n)
   //  {
   //      return QFunction { std::move(n), std::make_pair(std::shared_ptr<char>(), 0) };
   //  }

   //  template <typename Tp>
   //  inline QFunction
   //  qfun(std::string n, Tp const &)
   //  {
   //      // note: is_trivially_copyable is still unimplemented in g++-4.7
#if  0
   //      static_assert(std::is_trivially_copyable<Tp>::value, " must be trivially copyable");
#else
   //      static_assert(std::is_pod<Tp>::value, " must be a pod type");
#endif
   //      auto ptr  = std::shared_ptr<char>(new char[sizeof(Tp)], [](char *addr) { delete[] addr; });
   //      auto size = sizeof(Tp);

   //      memcpy(ptr.get(), &, sizeof(Tp));
   //      return QFunction { std::move(n), std::make_pair(std::move(ptr), size) };
   //  }

   //  //
   //  // Kleisli composition: >->
   //  //

   //  template <typename Fun>
   //  inline Computation<QFunction>
   //  operator>>(QFunction lhs, Fun &&rhs)
   //  {
   //      return { std::move(lhs), std::forward<Fun>(rhs) };
   //  }

   //  template <typename Tp, typename Fun>
   //  inline Computation<Computation<Tp>>
   //  operator>>(Computation<Tp> lhs, Fun &&rhs)
   //  {
   //      return { std::move(lhs), std::forward<Fun>(rhs) };
   //  }

    //
    // default in-kernel PFQ functions...
    //

#define PFQ_MAKE_FUN(fn, name) \
    inline QFunction fn() \
    { \
        return qfun(name); \
    }

#define PFQ_MAKE_FUN1(fn,name, typ) \
    inline QFunction fn(typ const &) \
    { \
        return qfun(name, ); \
    }
//
//     namespace
//     {
//         PFQ_MAKE_FUN(steer_mac    , "steer_mac"    )
//         PFQ_MAKE_FUN(steer_vlan   , "steer_vlan"   )
//         PFQ_MAKE_FUN(steer_ip     , "steer_ip"     )
//         PFQ_MAKE_FUN(steer_ipv6   , "steer_ipv6"   )
//         PFQ_MAKE_FUN(steer_flow   , "steer_flow"   )
//
//         PFQ_MAKE_FUN(legacy       , "legacy"       )
//         PFQ_MAKE_FUN(broadcast    , "broadcast"    )
//         PFQ_MAKE_FUN(sink         , "sink"         )
//         PFQ_MAKE_FUN(drop         , "drop"         )
//
//         PFQ_MAKE_FUN(id           , "id"           )
//
//         PFQ_MAKE_FUN(ip           , "ip"           )
//         PFQ_MAKE_FUN(ipv6         , "ipv6"         )
//         PFQ_MAKE_FUN(udp          , "udp"          )
//         PFQ_MAKE_FUN(tcp          , "tcp"          )
//         PFQ_MAKE_FUN(vlan         , "vlan"         )
//         PFQ_MAKE_FUN(icmp         , "icmp"         )
//         PFQ_MAKE_FUN(flow         , "flow"         )
//
//         PFQ_MAKE_FUN(rtp          , "rtp"          )
//         PFQ_MAKE_FUN(steer_rtp    , "steer_rtp"    )
//
//         PFQ_MAKE_FUN1(dummy       , "dummy",   int      )
//         PFQ_MAKE_FUN1(counter     , "counter", int      )
//         PFQ_MAKE_FUN1(class_      , "class",   uint16_t )
//     }

} // namespace pfq_lang
