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
#include <algorithm>

#include <linux/pf_q.h>

namespace pfq_lang
{
    //
    // qFunction
    //

    struct qFunction
    {
        std::string name;
        std::pair<std::shared_ptr<char>, size_t> context;

        std::vector<qFunction>
        operator()() const
        {
            return std::vector<qFunction> { *this };
        }
    };

    //
    // qComputation
    //

    template <typename Comp>
    struct qComputation
    {
        Comp left;
        qFunction right;

        std::vector<qFunction>
        operator()() const
        {
            auto l = left();
            auto r = right();

            l.reserve(l.size() + r.size());
            std::move(std::begin(r), std::end(r), std::back_inserter(l));

            return l;
        }
    };

    inline std::string
    show(qFunction const &fun)
    {
        std::stringstream out;

        out << fun.name;
        if (fun.context.first)
            out << ' ' << fun.context.second << "@" << (void *)fun.context.first.get();

        return out.str();
    }

    template <typename Tp>
    inline std::string
    show(qComputation<Tp> const &comp)
    {
        return show(comp.left) + " >-> " + show(comp.right);
    }

    //
    // evaluate a computation, creating a pfq_meta_prog.
    //

    template <typename Comp>
    inline std::unique_ptr<pfq_meta_prog>
    eval(Comp &comp)
    {
        return eval(comp());
    }

    inline std::unique_ptr<pfq_meta_prog>
    eval(std::vector<qFunction> const &vec)
    {
        auto prg = reinterpret_cast<pfq_meta_prog *>(std::malloc(sizeof(pfq_meta_prog) + sizeof(pfq_fun_t) * sizeof(vec)));

        prg->size = vec.size();

        for(unsigned int i = 0; i < prg->size; ++i)
        {
            prg->fun[i].symbol       = vec[i].name.c_str();
            prg->fun[i].context.addr = vec[i].context.first.get();
            prg->fun[i].context.size = vec[i].context.second;
        }

        return std::unique_ptr<pfq_meta_prog>(prg);
    }

    //
    // generic function constructor
    //

    inline qFunction
    qfun(std::string n)
    {
        return qFunction { std::move(n), std::make_pair(std::shared_ptr<char>(), 0) };
    }

    template <typename Tp>
    inline qFunction
    qfun(std::string n, Tp const &context)
    {
        // note: is_trivially_copyable is still unimplemented in g++-4.7
#if 0
        static_assert(std::is_trivially_copyable<Tp>::value, "context must be trivially copyable");
#else
        static_assert(std::is_pod<Tp>::value, "context must be a pod type");
#endif
        auto ptr  = std::shared_ptr<char>(new char[sizeof(Tp)], [](char *addr) { delete[] addr; });
        auto size = sizeof(Tp);

        memcpy(ptr.get(), &context, sizeof(Tp));
        return qFunction { std::move(n), std::make_pair(std::move(ptr), size) };
    }

    //
    // Kleisli composition: >->
    //

    template <typename Fun>
    inline qComputation<qFunction>
    operator>>(qFunction lhs, Fun &&rhs)
    {
        return { std::move(lhs), std::forward<Fun>(rhs) };
    }

    template <typename Tp, typename Fun>
    inline qComputation<qComputation<Tp>>
    operator>>(qComputation<Tp> lhs, Fun &&rhs)
    {
        return { std::move(lhs), std::forward<Fun>(rhs) };
    }

    //
    // default in-kernel PFQ functions...
    //

#define PFQ_MAKE_FUN(fn, name) \
    inline qFunction fn() \
    { \
        return qfun(name); \
    }

#define PFQ_MAKE_FUN1(fn,name, typ) \
    inline qFunction fn(typ const &context) \
    { \
        return qfun(name, context); \
    }

    namespace
    {
        PFQ_MAKE_FUN(steer_mac    , "steer-mac"    )
        PFQ_MAKE_FUN(steer_vlan   , "steer-vlan-id")
        PFQ_MAKE_FUN(steer_ip     , "steer-ip"     )
        PFQ_MAKE_FUN(steer_ipv6   , "steer-ipv6"   )
        PFQ_MAKE_FUN(steer_flow   , "steer-flow"   )

        PFQ_MAKE_FUN(legacy       , "legacy"       )
        PFQ_MAKE_FUN(clone        , "clone"        )
        PFQ_MAKE_FUN(broadcast    , "broadcast"    )
        PFQ_MAKE_FUN(sink         , "sink"         )
        PFQ_MAKE_FUN(drop         , "drop"         )

        PFQ_MAKE_FUN(id           , "id"           )

        PFQ_MAKE_FUN(ip           , "ip"           )
        PFQ_MAKE_FUN(ipv6         , "ipv6"         )
        PFQ_MAKE_FUN(udp          , "udp"          )
        PFQ_MAKE_FUN(tcp          , "tcp"          )
        PFQ_MAKE_FUN(vlan         , "vlan"         )
        PFQ_MAKE_FUN(icmp         , "icmp"         )
        PFQ_MAKE_FUN(flow         , "flow"         )

        PFQ_MAKE_FUN(rtp          , "rtp"          )
        PFQ_MAKE_FUN(steer_rtp    , "steer-rtp"    )

        PFQ_MAKE_FUN1(dummy       , "dummy",   int      )
        PFQ_MAKE_FUN1(counter     , "counter", int      )
        PFQ_MAKE_FUN1(class_      , "class",   uint16_t )
    }

} // namespace pfq_lang
