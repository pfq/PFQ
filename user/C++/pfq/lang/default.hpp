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

#include <pfq/lang/lang.hpp>
#include <pfq/lang/details.hpp>

#include <functional>
#include <vector>
#include <string>
#include <cmath>

#include <arpa/inet.h>

namespace pfq
{
namespace lang
{
    using namespace std::placeholders;

    // default combinators:

    template <typename P1, typename P2>
    auto inline operator&(P1 const &p1, P2 const &p2)
    -> decltype(combinator2(nullptr, p1, p2))
    {
        return combinator2("and", p1, p2);
    }

    template <typename P1, typename P2>
    auto inline operator|(P1 const &p1, P2 const &p2)
    -> decltype(combinator2(nullptr, p1, p2))
    {
        return combinator2("or", p1, p2);
    }

    template <typename P1, typename P2>
    auto inline operator^(P1 const &p1, P2 const &p2)
    -> decltype(combinator2(nullptr, p1, p2))
    {
        return combinator2("xor", p1, p2);
    }

    template <typename P>
    auto inline not_(P const &p)
    -> decltype(combinator1(nullptr, p))
    {
        return combinator1("not", p);
    }

    // default comparators:

    template <typename P>
    auto inline
    operator<(P const &prop, uint64_t arg)
    -> decltype(predicateR1(nullptr, prop, arg))
    {
        return predicateR1("less", prop, arg);
    }

    template <typename P>
    auto inline
    operator<=(P const &prop, uint64_t arg)
    -> decltype(predicateR1(nullptr, prop, arg))
    {
        return predicateR1("less_eq", prop, arg);
    }

    template <typename P>
    auto inline
    operator>(P const &prop, uint64_t arg)
    -> decltype(predicateR1(nullptr, prop, arg))
    {
        return predicateR1("greater", prop, arg);
    }

    template <typename P>
    auto inline
    operator>=(P const &prop, uint64_t arg)
    -> decltype(predicateR1(nullptr, prop, arg))
    {
        return predicateR1("greater_eq", prop, arg);
    }

    template <typename P>
    auto inline
    operator==(P const &prop, uint64_t arg)
    -> decltype(predicateR1(nullptr, prop, arg))
    {
        return predicateR1("equal", prop, arg);
    }

    template <typename P>
    auto inline
    operator!=(P const &prop, uint64_t arg)
    -> decltype(predicateR1(nullptr, prop, arg))
    {
        return predicateR1("not_equal", prop, arg);
    }

    template <typename P>
    auto inline any_bit(P const &prop, uint64_t mask)
    -> decltype(predicateR1(nullptr, prop, mask))
    {
        return predicateR1("any_bit", prop, mask);
    }

    template <typename P>
    auto inline all_bit(P const &prop, uint64_t mask)
    -> decltype(predicateR1(nullptr, prop, mask))
    {
        return predicateR1("all_bit", prop, mask);
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

        auto vlan_id        = [] (std::vector<int> const &vs) {
                                    return predicate1("vlan_id", vs);
                                };

        auto vlan_id_filter = [] (std::vector<int> const &vs) {
                                    return mfunction1("vlan_id_filter", vs);
                              };
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

        auto steer_link = mfunction("steer_link");
        auto steer_vlan = mfunction("steer_vlan");
        auto steer_ip   = mfunction("steer_ip");
        auto steer_ip6  = mfunction("steer_ip6");
        auto steer_flow = mfunction("steer_flow");
        auto steer_rtp  = mfunction("steer_rtp");

        auto steer_net  = [] (const char *net, int prefix, int subprefix)
        {
            struct supernet {
                uint32_t addr;
                int      prefix;
                int      subprefix;
            } na = { 0, prefix, subprefix };

            if (inet_pton(AF_INET, net, &na.addr) <= 0)
                throw std::runtime_error("pfq::lang::steer_net");

            return mfunction1("steer_net", na);
        };

        auto steer_field = [] (int off_bytes, int size_bits) {
                                return mfunction2("steer_field", off_bytes, size_bits);
                           };
        // filters:

        auto filter         = std::bind(details::polymorphic_mfunctionP(), "filter", _1);

        auto ip             = mfunction("ip");
        auto ip6            = mfunction("ip6");
        auto udp            = mfunction("udp");
        auto tcp            = mfunction("tcp");
        auto udp6           = mfunction("udp6");
        auto tcp6           = mfunction("tcp6");
        auto icmp6          = mfunction("icmp6");
        auto vlan           = mfunction("vlan");
        auto icmp           = mfunction("icmp");
        auto flow           = mfunction("flow");
        auto rtp            = mfunction("rtp");

        auto no_frag        = mfunction("no_frag");
        auto no_more_frag   = mfunction("no_more_frag");

        auto kernel         = mfunction("kernel");
        auto broadcast      = mfunction("broadcast");
        auto drop           = mfunction("drop");
        auto unit           = mfunction("unit");

        auto log_msg        = [] (std::string msg) { return mfunction1("log_msg", std::move(msg)); };
        auto log_buff       = mfunction("log_buff");
        auto log_packet     = mfunction("log_packet");

        auto forward    = [] (std::string dev) { return mfunction1("forward", std::move(dev)); };
        auto forwardIO  = [] (std::string dev) { return mfunction1("forwardIO", std::move(dev)); };
        auto bridge     = [] (std::string dev) { return mfunction1("bridge", std::move(dev)); };
        auto tee_       = std::bind(details::polymorphic_mfunction1P(), "tee", _1, _2);
        auto tap        = std::bind(details::polymorphic_mfunction1P(), "tap", _1, _2);

        auto mark           = [] (unsigned long value) { return mfunction1("mark", value); };
        auto inc            = [] (int value) { return mfunction1("inc", value); };
        auto dec            = [] (int value) { return mfunction1("dec", value); };

        auto l3_proto       = [] (uint16_t type) { return mfunction1 ("l3_proto", type); };
        auto l4_proto       = [] (uint8_t proto) { return mfunction1 ("l4_proto", proto); };

        auto port           = [] (uint16_t p) { return mfunction1 ("port", p); };
        auto src_port       = [] (uint16_t p) { return mfunction1 ("src_port", p); };
        auto dst_port       = [] (uint16_t p) { return mfunction1 ("dst_port", p); };

        auto addr = [] (const char *net, int prefix)
        {
            auto a = details::make_netaddr(net, prefix);
            return mfunction1("addr", a);
        };

        auto src_addr = [] (const char *net, int prefix)
        {
            auto a = details::make_netaddr(net, prefix);
            return mfunction1("src_addr", a);
        };

        auto dst_addr = [] (const char *net, int prefix)
        {
            auto a = details::make_netaddr(net, prefix);
            return mfunction1("dst_addr", a);
        };

        auto when        = std::bind(details::polymorphic_mfunctionPF(), "when", _1, _2);
        auto unless      = std::bind(details::polymorphic_mfunctionPF(), "unless", _1, _2);
        auto conditional = std::bind(details::polymorphic_mfunctionPFF(),  "conditional", _1, _2, _3);

        auto inv         = std::bind(details::polymorphic_mfunctionF(),  "inv", _1);
        auto par         = std::bind(details::polymorphic_mfunctionFF(), "par", _1, _2);

        // bloom filters:

        auto bloom      = [] (int m, std::vector<std::string> const &ips) {
                                auto addrs = details::fmap(details::inet_addr, ips);
                                return predicate2("bloom", m, std::move(addrs));
                          };

        auto bloom_src  = [] (int m, std::vector<std::string> const &ips) {
                                auto addrs = details::fmap(details::inet_addr, ips);
                                return predicate2("bloom_src", m, std::move(addrs));
                          };

        auto bloom_dst  = [] (int m, std::vector<std::string> const &ips) {
                                auto addrs = details::fmap(details::inet_addr, ips);
                                return predicate2("bloom_dst", m, std::move(addrs));
                          };


        auto bloom_filter      = [] (int m, std::vector<std::string> const &ips) {
                                    auto addrs = details::fmap(details::inet_addr, ips);
                                    return mfunction2("bloom_filter", m, std::move(addrs));
                                };

        auto bloom_src_filter  = [] (int m, std::vector<std::string> const &ips) {
                                    auto addrs = details::fmap(details::inet_addr, ips);
                                    return mfunction2("bloom_src_filter", m, std::move(addrs));
                                };

        auto bloom_dst_filter  = [] (int m, std::vector<std::string> const &ips) {
                                    auto addrs = details::fmap(details::inet_addr, ips);
                                    return mfunction2("bloom_dst_filter", m, std::move(addrs));
                                };

        // bloom filter, utility functions:
        //

        constexpr int bloomK = 4;

        inline int bloom_calc_m(int n, double p)
        {
            return static_cast<int>(std::ceil( -static_cast<double>(bloomK) * n / std::log( 1.0 - std::pow(p, 1.0 / bloomK) )));
        }

        inline int bloom_calc_n(int m, double p)
        {
            return static_cast<int>(std::ceil( -static_cast<double>(m) * std::log( 1.0 - std::pow(p, 1.0 / bloomK) ) / bloomK ));
        }

        inline double
        bloom_calc_p(int n, int m)
        {
            return std::pow(1 - std::pow(1 - 1.0/m, n * bloomK), bloomK);
        }

    }

} // namespace lang
} // naemspace pfq
