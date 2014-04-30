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

#include <arpa/inet.h>

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

    // default comparators:

    template <typename P>
    auto inline
    operator<(P const &prop, uint64_t arg)
    -> decltype(predicate4(std::string(nullptr), prop, arg))
    {
        return predicate4("less", prop, arg);
    }

    template <typename P>
    auto inline
    operator<=(P const &prop, uint64_t arg)
    -> decltype(predicate4(std::string(nullptr), prop, arg))
    {
        return predicate4("less_eq", prop, arg);
    }

    template <typename P>
    auto inline
    operator>(P const &prop, uint64_t arg)
    -> decltype(predicate4(std::string(nullptr), prop, arg))
    {
        return predicate4("greater", prop, arg);
    }

    template <typename P>
    auto inline
    operator>=(P const &prop, uint64_t arg)
    -> decltype(predicate4(std::string(nullptr), prop, arg))
    {
        return predicate4("greater_eq", prop, arg);
    }

    template <typename P>
    auto inline
    operator==(P const &prop, uint64_t arg)
    -> decltype(predicate4(std::string(nullptr), prop, arg))
    {
        return predicate4("equal", prop, arg);
    }

    template <typename P>
    auto inline
    operator!=(P const &prop, uint64_t arg)
    -> decltype(predicate4(std::string(nullptr), prop, arg))
    {
        return predicate4("not_equal", prop, arg);
    }

    template <typename P>
    auto inline any_bit(P const &prop, uint64_t mask)
    -> decltype(predicate4("any_bit", prop, mask))
    {
        return predicate4("any_bit", prop, mask);
    }

    template <typename P>
    auto inline all_bit(P const &prop, uint64_t mask)
    -> decltype(predicate4("all_bit", prop, mask))
    {
        return predicate4("all_bit", prop, mask);
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

    // utility function
    //

    static inline uint32_t
    prefix2mask(size_t n)
    {
        return htonl(~((1ULL << (32-n)) - 1));
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

        auto is_l3_proto = [] (uint16_t type) { return predicate1 ("is_l3_proto", type); };
        auto is_l4_proto = [] (uint8_t proto) { return predicate1 ("is_l4_proto", proto); };

        auto has_port     = [] (uint16_t port) { return predicate1 ("is_port", port); };
        auto has_src_port = [] (uint16_t port) { return predicate1 ("is_src_port", port); };
        auto has_dst_port = [] (uint16_t port) { return predicate1 ("is_dst_port", port); };

        auto has_addr = [] (const char *net, int prefix)
        {
            struct in_addr addr;
            if (inet_pton(AF_INET, net, &addr) <= 0)
                throw std::runtime_error("pfq_lang::net");

            return predicate1("has_addr", static_cast<uint64_t>(addr.s_addr) << 32 | prefix2mask(prefix));
        };

        auto has_src_addr = [] (const char *net, int prefix)
        {
            struct in_addr addr;
            if (inet_pton(AF_INET, net, &addr) <= 0)
                throw std::runtime_error("pfq_lang::net");

            return predicate1("has_src_addr", static_cast<uint64_t>(addr.s_addr) << 32 | prefix2mask(prefix));
        };

        auto has_dst_addr = [] (const char *net, int prefix)
        {
            struct in_addr addr;
            if (inet_pton(AF_INET, net, &addr) <= 0)
                throw std::runtime_error("pfq_lang::net");

            return predicate1("has_dst_addr", static_cast<uint64_t>(addr.s_addr) << 32 | prefix2mask(prefix));
        };

        auto is_flow    = predicate ("is_flow");
        auto has_vlan   = predicate ("has_vlan");

        auto has_vid    = [] (int value) { return predicate1 ("has_vid", value); };
        auto has_mark   = [] (unsigned long value) { return predicate1("has_mark", value); };

        // default properties:

        auto ip_tos     = property("ip_tos");
        auto ip_tot_len = property("ip_tot_len");
        auto ip_id      = property("ip_id");
        auto ip_frag    = property("ip_frag");
        auto ip_ttl     = property("ip_ttl");

        auto tcp_source = property("tcp_source");
        auto tcp_dest   = property("tcp_dest");
        auto tcp_hdrlen = property("tcp_hdrlen");

        auto udp_source = property("udp_source");
        auto udp_dest   = property("udp_dest");
        auto udp_len    = property("udp_len");

        auto icmp_type  = property("icmp_type");
        auto icmp_code  = property("icmp_code");

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
        auto unit       = computation("unit");

        auto forward    = [] (int index) { return computation1("forward", index); };
        auto mark       = [] (unsigned long value) { return computation1("mark", value); };
        auto dummy      = [] (int value) { return computation1("dummy", value); };
        auto counter    = [] (int value) { return computation1("counter", value); };
        auto class_     = [] (int value) { return computation1("class", value); };
        auto l3_proto   = [] (uint16_t type) { return computation1 ("l3_proto", type); };
        auto l4_proto   = [] (uint8_t proto) { return computation1 ("l4_proto", proto); };

        auto port       = [] (uint16_t port) { return computation1 ("port", port); };
        auto src_port   = [] (uint16_t port) { return computation1 ("src_port", port); };
        auto dst_port   = [] (uint16_t port) { return computation1 ("dst_port", port); };

        auto addr = [] (const char *net, int prefix)
        {
            struct in_addr addr;
            if (inet_pton(AF_INET, net, &addr) <= 0)
                throw std::runtime_error("pfq_lang::net");

            return computation1("addr", static_cast<uint64_t>(addr.s_addr) << 32 | prefix2mask(prefix));
        };

        auto src_addr = [] (const char *net, int prefix)
        {
            struct in_addr addr;
            if (inet_pton(AF_INET, net, &addr) <= 0)
                throw std::runtime_error("pfq_lang::net");

            return computation1("src_addr", static_cast<uint64_t>(addr.s_addr) << 32 | prefix2mask(prefix));
        };

        auto dst_addr = [] (const char *net, int prefix)
        {
            struct in_addr addr;
            if (inet_pton(AF_INET, net, &addr) <= 0)
                throw std::runtime_error("pfq_lang::net");

            return computation1("dst_addr", static_cast<uint64_t>(addr.s_addr) << 32 | prefix2mask(prefix));
        };

        auto hdummy      = std::bind(hcomp(),  "hdummy", _1);
        auto when        = std::bind(hcomp1(), "when", _1, _2);
        auto unless      = std::bind(hcomp1(), "unless", _1, _2);
        auto conditional = std::bind(hcomp2(), "conditional", _1, _2, _3);

    }
}
