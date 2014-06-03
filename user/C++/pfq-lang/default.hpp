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

#include <pfq-lang/lang.hpp>
#include <pfq-lang/details.hpp>

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

    template <typename P>
    auto inline not_(P const &p)
    -> decltype(predicate2(combinator(nullptr), p, p))
    {
        return predicate2(combinator("not"), p, p);
    }

    // default comparators:

    template <typename P>
    auto inline
    operator<(P const &prop, uint64_t arg)
    -> decltype(predicate4(nullptr, prop, arg))
    {
        return predicate4("less", prop, arg);
    }

    template <typename P>
    auto inline
    operator<=(P const &prop, uint64_t arg)
    -> decltype(predicate4(nullptr, prop, arg))
    {
        return predicate4("less_eq", prop, arg);
    }

    template <typename P>
    auto inline
    operator>(P const &prop, uint64_t arg)
    -> decltype(predicate4(nullptr, prop, arg))
    {
        return predicate4("greater", prop, arg);
    }

    template <typename P>
    auto inline
    operator>=(P const &prop, uint64_t arg)
    -> decltype(predicate4(nullptr, prop, arg))
    {
        return predicate4("greater_eq", prop, arg);
    }

    template <typename P>
    auto inline
    operator==(P const &prop, uint64_t arg)
    -> decltype(predicate4(nullptr, prop, arg))
    {
        return predicate4("equal", prop, arg);
    }

    template <typename P>
    auto inline
    operator!=(P const &prop, uint64_t arg)
    -> decltype(predicate4(nullptr, prop, arg))
    {
        return predicate4("not_equal", prop, arg);
    }

    template <typename P>
    auto inline any_bit(P const &prop, uint64_t mask)
    -> decltype(predicate4(nullptr, prop, mask))
    {
        return predicate4("any_bit", prop, mask);
    }

    template <typename P>
    auto inline all_bit(P const &prop, uint64_t mask)
    -> decltype(predicate4(nullptr, prop, mask))
    {
        return predicate4("all_bit", prop, mask);
    }

    namespace
    {
        // default predicates:

        auto is_ip          = predicate ("is_ip");
        auto is_ip6         = predicate ("is_ip6");
        auto is_udp         = predicate ("is_udp");
        auto is_tcp         = predicate ("is_tcp");
        auto is_icmp        = predicate ("is_icmp");
        auto is_udp6        = predicate ("is_udp6");
        auto is_tcp6        = predicate ("is_tcp6");
        auto is_icmp6       = predicate ("is_icmp6");

        auto is_l3_proto    = [] (uint16_t type) { return predicate1 ("is_l3_proto", type); };
        auto is_l4_proto    = [] (uint8_t proto) { return predicate1 ("is_l4_proto", proto); };

        auto has_port       = [] (uint16_t port) { return predicate1 ("is_port", port); };
        auto has_src_port   = [] (uint16_t port) { return predicate1 ("is_src_port", port); };
        auto has_dst_port   = [] (uint16_t port) { return predicate1 ("is_dst_port", port); };

        auto has_addr = [] (const char *net, int prefix)
        {
            auto addr = details::make_netaddr(net, prefix);
            return predicate1("has_addr", addr);
        };

        auto has_src_addr = [] (const char *net, int prefix)
        {
            auto addr = details::make_netaddr(net, prefix);
            return predicate1("has_src_addr", addr);
        };

        auto has_dst_addr = [] (const char *net, int prefix)
        {
            auto addr = details::make_netaddr(net, prefix);
            return predicate1("has_dst_addr", addr);
        };

        auto is_flow        = predicate ("is_flow");
        auto has_vlan       = predicate ("has_vlan");

        auto has_vid        = [] (int value) { return predicate1 ("has_vid", value); };
        auto has_mark       = [] (unsigned long value) { return predicate1("has_mark", value); };

        auto is_frag        = predicate ("is_frag");
        auto is_first_frag  = predicate ("is_first_frag");
        auto is_more_frag   = predicate ("is_more_frag");

        // default properties:

        auto get_mark   = property("get_mark");

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

        // default netfunctions:

        auto steer_link = netfunction("steer_link");
        auto steer_vlan = netfunction("steer_vlan");
        auto steer_ip   = netfunction("steer_ip");
        auto steer_ip6  = netfunction("steer_ip6");
        auto steer_flow = netfunction("steer_flow");
        auto steer_rtp  = netfunction("steer_rtp");

        auto steer_net  = [] (const char *net, int prefix, int subprefix) {

                                struct supernet {
                                    uint32_t addr;
                                    int      prefix;
                                    int      subprefix;
                                } na = { 0, prefix, subprefix };

                                if (inet_pton(AF_INET, net, &na.addr) <= 0)
                                    throw std::runtime_error("pfq_lang::steer_net");

                                return netfunction1("steer_net", na);
                             };

        // others:

        auto ip             = netfunction("ip");
        auto ip6            = netfunction("ip6");
        auto udp            = netfunction("udp");
        auto tcp            = netfunction("tcp");
        auto udp6           = netfunction("udp6");
        auto tcp6           = netfunction("tcp6");
        auto icmp6          = netfunction("icmp6");
        auto vlan           = netfunction("vlan");
        auto icmp           = netfunction("icmp");
        auto flow           = netfunction("flow");
        auto rtp            = netfunction("rtp");

        auto no_frag        = netfunction("no_frag");
        auto no_more_frag   = netfunction("no_more_frag");

        auto broadcast      = netfunction("broadcast");
        auto kernel         = netfunction("kernel");
        auto forward_kernel = netfunction("forward_kernel");
        auto drop           = netfunction("drop");
        auto unit           = netfunction("unit");

        auto log_msg        = [] (const std::string &msg) { return netfunction("log_msg") };
        auto log_packet     = netfunction("log_packet");

        auto class_         = [] (int value) { return netfunction1("class", value); };
        auto deliver        = [] (int value) { return netfunction1("deliver", value); };

        auto forward        = [] (int index) { return netfunction1("forward", index); };
        auto mark           = [] (unsigned long value) { return netfunction1("mark", value); };
        auto dummy          = [] (int value) { return netfunction1("dummy", value); };
        auto inc            = [] (int value) { return netfunction1("inc", value); };
        auto dec            = [] (int value) { return netfunction1("dec", value); };

        auto l3_proto       = [] (uint16_t type) { return netfunction1 ("l3_proto", type); };
        auto l4_proto       = [] (uint8_t proto) { return netfunction1 ("l4_proto", proto); };

        auto port           = [] (uint16_t port) { return netfunction1 ("port", port); };
        auto src_port       = [] (uint16_t port) { return netfunction1 ("src_port", port); };
        auto dst_port       = [] (uint16_t port) { return netfunction1 ("dst_port", port); };

        auto addr = [] (const char *net, int prefix)
        {
            auto addr = details::make_netaddr(net, prefix);
            return netfunction1("addr", addr);
        };

        auto src_addr = [] (const char *net, int prefix)
        {
            auto addr = details::make_netaddr(net, prefix);
            return netfunction1("src_addr", addr);
        };

        auto dst_addr = [] (const char *net, int prefix)
        {
            auto addr = details::make_netaddr(net, prefix);
            return netfunction1("dst_addr", addr);
        };

        auto hdummy      = std::bind(details::hcomp(),  "hdummy", _1);
        auto when        = std::bind(details::hcomp1(), "when", _1, _2);
        auto unless      = std::bind(details::hcomp1(), "unless", _1, _2);
        auto conditional = std::bind(details::hcomp2(), "conditional", _1, _2, _3);

        auto crc16       = netfunction("crc16");
    }
}
