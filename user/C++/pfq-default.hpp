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

#include <pfq-lang.hpp>
#include <functional>

namespace pfq_lang
{
    using namespace std::placeholders;

    // default combinators:

    template <typename P1, typename P2>
    auto inline operator&(P1 const &p1, P2 const &p2)
    -> decltype(predicate2(combinator(nullptr), p1, p2))
    {
        return predicate2(combinator("and"), p1, p2);
    }

    template <typename P1, typename P2>
    auto inline operator|(P1 const &p1, P2 const &p2)
    -> decltype(predicate2(combinator(nullptr), p1, p2))
    {
        return predicate2(combinator("or"), p1, p2);
    }

    template <typename P1, typename P2>
    auto inline operator^(P1 const &p1, P2 const &p2)
    -> decltype(predicate2(combinator(nullptr), p1, p2))
    {
        return predicate2(combinator("xor"), p1, p2);
    }

    // polymorphic lambda are available starting from C++14! In the meanwhile...
    //

    struct hcomp
    {
        template <typename P>
        auto operator()(std::string name, P const &p)
        -> decltype(hcomputation(std::move(name), p))
        {
            return hcomputation(std::move(name), p);
        }
    };

    struct hcomp1
    {
        template <typename P, typename C>
        auto operator()(std::string name, P const &p, C const &c)
        -> decltype(hcomputation1(std::move(name), p, c))
        {
            return hcomputation1(std::move(name), p, c);
        }
    };

    struct hcomp2
    {
        template <typename P, typename C1, typename C2>
        auto operator()(std::string name, P const &p, C1 const &c1, C2 const &c2)
        -> decltype(hcomputation2(std::move(name), p, c1, c2))
        {
            return hcomputation2(std::move(name), p, c1, c2);
        }
    };

    namespace
    {
        // default predicates:

        auto is_ip      = predicate ("is_ip");
        auto is_ip6     = predicate ("is_ip6");
        auto is_udp     = predicate ("is_udp");
        auto is_tcp     = predicate ("is_tcp");
        auto is_icmp    = predicate ("is_icmp");
        auto is_udp6    = predicate ("is_udp6");
        auto is_tcp6    = predicate ("is_tcp6");
        auto is_icmp6   = predicate ("is_icmp6");
        auto is_flow    = predicate ("is_flow");
        auto has_vlan   = predicate ("has_vlan");

        auto has_vid    = [] (int value) { return predicate1 ("has_vid", value); };
        auto has_mark   = [] (unsigned long value) { return predicate1("has_mark", value); };

        // default computations:

        auto steer_mac  = computation("steer_mac");
        auto steer_vlan = computation("steer_vlan");
        auto steer_ip   = computation("steer_ip");
        auto steer_ip6  = computation("steer_ip6");
        auto steer_flow = computation("steer_flow");
        auto steer_rtp  = computation("steer_rtp");

        // others:

        auto ip         = computation("ip");
        auto ip6        = computation("ip6");
        auto udp        = computation("udp");
        auto tcp        = computation("tcp");
        auto udp6       = computation("udp6");
        auto tcp6       = computation("tcp6");
        auto icmp6      = computation("icmp6");
        auto vlan       = computation("vlan");
        auto icmp       = computation("icmp");
        auto flow       = computation("flow");
        auto rtp        = computation("rtp");

        auto broadcast  = computation("broadcast");
        auto kernel     = computation("kernel");
        auto sink       = computation("sink");
        auto drop       = computation("drop");
        auto id         = computation("id");

        auto mark       = [] (unsigned long value) { return computation1("mark", value); };
        auto dummy      = [] (int value) { return computation1("dummy", value); };
        auto counter    = [] (int value) { return computation1("counter", value); };
        auto class_     = [] (int value) { return computation1("class", value); };

        auto hdummy      = std::bind(hcomp(),  "hdummy", _1);
        auto when        = std::bind(hcomp1(), "when", _1, _2);
        auto unless      = std::bind(hcomp1(), "unless", _1, _2);
        auto conditional = std::bind(hcomp2(), "conditional", _1, _2, _3);

    }
}
