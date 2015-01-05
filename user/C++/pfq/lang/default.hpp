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

/*! \file default.hpp
 *  \brief This header contains the PFQ/lang eDSL functions.
 */

namespace pfq
{
namespace lang
{
    using namespace std::placeholders;

    //
    // default combinators:
    //

    //! Combine two predicate expressions with a specific boolean 'and' operation.

    template <typename P1, typename P2>
    auto inline operator&(P1 const &p1, P2 const &p2)
    -> decltype(combinator2(nullptr, p1, p2))
    {
        return combinator2("and", p1, p2);
    }

    //! Combine two predicate expressions with a specific boolean 'or' operation.

    template <typename P1, typename P2>
    auto inline operator|(P1 const &p1, P2 const &p2)
    -> decltype(combinator2(nullptr, p1, p2))
    {
        return combinator2("or", p1, p2);
    }

    //! Combine two predicate expressions with a specific boolean 'xor' operation.

    template <typename P1, typename P2>
    auto inline operator^(P1 const &p1, P2 const &p2)
    -> decltype(combinator2(nullptr, p1, p2))
    {
        return combinator2("xor", p1, p2);
    }

    //! Return a new predicate that evaluates to true, when the given one evaluates to false, and vice versa.

    template <typename P>
    auto inline not_(P const &p)
    -> decltype(combinator1(nullptr, p))
    {
        return combinator1("not", p);
    }

    //
    // default comparators:
    //

    //! Return a predicate that evaluates to \c true, if the property is less than the given value.
    /*!
     * Example:
     *
     * when (ip_ttl < 64,  drop)
     */

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

    //! Return a predicate that evaluates to \c true, if the property has at least one bit set among those specified by the given mask.

    template <typename P>
    auto inline any_bit(P const &prop, uint64_t mask)
    -> decltype(predicateR1(nullptr, prop, mask))
    {
        return predicateR1("any_bit", prop, mask);
    }

    //! Return a predicate that evaluates to \c true, if the property has all bits set among those specified in the given mask.

    template <typename P>
    auto inline all_bit(P const &prop, uint64_t mask)
    -> decltype(predicateR1(nullptr, prop, mask))
    {
        return predicateR1("all_bit", prop, mask);
    }

    namespace
    {
        //
        // default predicates:
        //

        //! Evaluate to \c true if the SkBuff is an IPv4 packet.

        auto is_ip          = predicate ("is_ip");

        //! Evaluate to \c true if the SkBuff is an IPv6 packet.

        auto is_ip6         = predicate ("is_ip6");

        //! Evaluate to \c true if the SkBuff is an UDP packet.

        auto is_udp         = predicate ("is_udp");

        //! Evaluate to \c true if the SkBuff is a TCP packet.

        auto is_tcp         = predicate ("is_tcp");

        //! Evaluate to \c true if the SkBuff is an ICMP packet.

        auto is_icmp        = predicate ("is_icmp");

        //! Evaluate to \c true if the SkBuff is an UDP packet, on top of IPv6.

        auto is_udp6        = predicate ("is_udp6");

        //! Evaluate to \c true if the SkBuff is a TCP packet, on top of IPv6.

        auto is_tcp6        = predicate ("is_tcp6");

        //! Evaluate to \c true if the SkBuff is an ICMP packet, on top of IPv6.

        auto is_icmp6       = predicate ("is_icmp6");

        //! Evaluate to \c true if the SkBuff is an UDP or TCP packet.

        auto is_flow        = predicate ("is_flow");

        //! Evaluate to \c true if the SkBuff is a TCP fragment.

        auto is_frag        = predicate ("is_frag");

        //! Evaluate to \c true if the SkBuff is the first TCP fragment.

        auto is_first_frag  = predicate ("is_first_frag");

        //! Evaluate to \c true if the SkBuff is a TCP fragment, but the first.

        auto is_more_frag   = predicate ("is_more_frag");

        //! Evaluate to \c true if the SkBuff has the given Layer3 protocol.

        auto is_l3_proto    = [] (uint16_t type) { return predicate1 ("is_l3_proto", type); };

        //! Evaluate to \c true if the SkBuff has the given Layer4 protocol.

        auto is_l4_proto    = [] (uint8_t proto) { return predicate1 ("is_l4_proto", proto); };

        //! Evaluate to \c true if the SkBuff has the given source or destination port.
        /*!
         * If the transport protocol is not present or has no port, the predicate evaluates to False.
         *
         * Example:
         *
         * has_port(80)
         */

        auto has_port       = [] (uint16_t port) { return predicate1 ("is_port", port); };

        //! Evaluate to \c true if the SkBuff has the given source port.
        /*!
         * If the transport protocol is not present or has no port, the predicate evaluates to False.
         */

        auto has_src_port   = [] (uint16_t port) { return predicate1 ("is_src_port", port); };

        //! Evaluate to \c true if the SkBuff has the given destination port.
        /*!
         * If the transport protocol is not present or has no port, the predicate evaluates to False.
         */

        auto has_dst_port   = [] (uint16_t port) { return predicate1 ("is_dst_port", port); };

        //! Evaluate to \c true if the source or destination IP address matches the given network address. I.e.,
        /*!
         * Example:
         *
         * has_addr ("192.168.0.0",24)
         */

        auto has_addr = [] (const char *addr, int prefix)
        {
            return predicate2("has_addr", ipv4_t {addr}, prefix);
        };

        //! Evaluate to \c true if the source IP address matches the given network address.

        auto has_src_addr = [] (const char *addr, int prefix)
        {
            return predicate2("has_src_addr", ipv4_t {addr}, prefix);
        };

        //! Evaluate to \c true if the destination IP address matches the given network address.

        auto has_dst_addr = [] (const char *addr, int prefix)
        {
            return predicate2("has_dst_addr", ipv4_t{addr}, prefix);
        };

        //! Evaluate to \c true if the SkBuff has the given \c mark, set by mark function.
        /*!
         * Example:
         *
         * has_mark(11)
         *
         * \see mark
         */

        auto has_mark       = [] (unsigned long value) { return predicate1("has_mark", value); };

        //! Evaluate to \c true if the SkBuff has a vlan tag.

        auto has_vlan       = predicate ("has_vlan");

        //! Evaluate to \c true if the SkBuff has the given vlan id.
        /*!
         * Example:
         *
         * has_vid(42)
         */

        auto has_vid        = [] (int value) { return predicate1 ("has_vid", value); };

        //! Predicate which evaluates to \c true when the packet has one of the
        /*!
         * vlan id specified by the list. Example:
         *
         * when (vland_id ({1,13,42,43}), msg_log("Got a packet!"))
         */

        auto vlan_id        = [] (std::vector<int> const &vs) {
                                    return predicate1("vlan_id", vs);
                                };

        //! Monadic function, counterpart of \c vlan_id function. \see vlan_id

        auto vlan_id_filter = [] (std::vector<int> const &vs) {
                                    return mfunction1("vlan_id_filter", vs);
                              };
        //
        // default properties:
        //

        //! Evaluate to the mark set by \c mark function.
        /*! By default packets are marked with 0.
         *
         * \see mark
         */

        auto get_mark   = property("get_mark");

        //! Evaluate to the /tos/ field of the IP header.

        auto ip_tos     = property("ip_tos");

        //! Evaluate to the /tot_len/ field of the IP header.

        auto ip_tot_len = property("ip_tot_len");

        //! Evaluate to the /ip_id/ field of the IP header.

        auto ip_id      = property("ip_id");

        //! Evaluate to the /frag/ field of the IP header.

        auto ip_frag    = property("ip_frag");

        //! Evaluate to the /TTL/ field of the IP header.

        auto ip_ttl     = property("ip_ttl");

        //! Evaluate to the /source port/ of the TCP header.

        auto tcp_source = property("tcp_source");

        //! Evaluate to the /destination port/ of the TCP header.

        auto tcp_dest   = property("tcp_dest");

        //! Evaluate to the /length/ field of the TCP header.

        auto tcp_hdrlen = property("tcp_hdrlen");

        //! Evaluate to the /source port/ of the UDP header.

        auto udp_source = property("udp_source");

        //! Evaluate to the /destination port/ of the UDP header.

        auto udp_dest   = property("udp_dest");

        //! Evaluate to the /length/ field of the UDP header.

        auto udp_len    = property("udp_len");

        //! Evaluate to the /type/ field of the ICMP header.

        auto icmp_type  = property("icmp_type");

        //! Evaluate to the /code/ field of the ICMP header.

        auto icmp_code  = property("icmp_code");

        //
        // default netfunctions:
        //

        //! Dispatch the packet across the sockets.
        /*!
         * Dispatch with a randomized algorithm that maintains the integrity
         * of physical links. Example:
         *
         * ip >> steer_link
         */

        auto steer_link = mfunction("steer_link");

        //! Dispatch the packet across the sockets
        /*!
         * Dispatch with a randomized algorithm that maintains the integrity
         * of vlan links. Example:
         *
         * steer_vlan
         */

        auto steer_vlan = mfunction("steer_vlan");

        //! Dispatch the packet across the sockets
        /*!
         * Dispatch with a randomized algorithm that maintains the integrity
         * of IP flows. Example:
         *
         * steer_ip
         */

        auto steer_ip   = mfunction("steer_ip");

        //! Dispatch the packet across the sockets
        /*!
         * Dispatch with a randomized algorithm that maintains the integrity
         * of IPv6 flows. Example:
         *
         * steer_ip6 >> log_msg("Steering an IPv6 packet")
         */

        auto steer_ip6  = mfunction("steer_ip6");

        //! Dispatch the packet across the sockets
        /*!
         * Dispatch with a randomized algorithm that maintains the integrity
         * of TCP/UDP flows. Example:
         *
         * steer_flow >> log_msg ("Steering a flow")
         */

        auto steer_flow = mfunction("steer_flow");

        //! Dispatch the packet across the sockets
        /*!
         * Dispatch with a randomized algorithm that maintains the integrity
         * of RTP/RTCP flows. Example:
         *
         * steer_rtp
         */

        auto steer_rtp  = mfunction("steer_rtp");

        //! Dispatch the packet across the sockets
        /*!
         * Dispatch with a randomized algorithm that maintains the integrity
         * of sub networks. Example:
         *
         * steer_net("192.168.0.0", 16, 24)
         */

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

        //! Dispatch the packet across the sockets
        /*!
         * Dispatch with a randomized algorithm. The function uses as /hash/ the field
         * of /size/ bits taken at /offset/ bytes from the beginning of the packet.
         *
         */

        auto steer_field = [] (int off_bytes, int size_bits) {
                                return mfunction2("steer_field", off_bytes, size_bits);
                           };
        //
        // default filters:
        //

        //! Transform the given predicate in its counterpart monadic version.
        /*!
         * Example:
         *
         * filter (is_udp) >> kernel
         *
         * is logically equivalent to:
         *
         * udp >> kernel
         */

        auto filter         = std::bind(details::polymorphic_mfunctionP(), "filter", _1);

        //! Evaluate to \c Pass SkBuff if it is an IPv4 packet, \c Drop it otherwise.

        auto ip             = mfunction("ip");

        //! Evaluate to \c Pass SkBuff if it is an IPv6 packet, \c Drop it otherwise.

        auto ip6            = mfunction("ip6");

        //! Evaluate to \c Pass SkBuff if it is an UDP packet, \c Drop it otherwise.

        auto udp            = mfunction("udp");

        //! Evaluate to \c Pass SkBuff if it is a TCP packet, \c Drop it otherwise.

        auto tcp            = mfunction("tcp");

        //! Evaluate to \c Pass SkBuff if it is an ICMP packet, \c Drop it otherwise.

        auto icmp           = mfunction("icmp");

        //! Evaluate to \c Pass SkBuff if it is an UDP packet (on top of IPv6), \c Drop it otherwise.

        auto udp6           = mfunction("udp6");

        //! Evaluate to \c Pass SkBuff if it is a TCP packet (on top of IPv6), \c Drop it otherwise.

        auto tcp6           = mfunction("tcp6");

        //! Evaluate to \c Pass SkBuff if it is an ICMP packet (on top of IPv6), \c Drop it otherwise.

        auto icmp6          = mfunction("icmp6");

        //! Evaluate to \c Pass SkBuff if it has a vlan tag, \c Drop it otherwise.

        auto vlan           = mfunction("vlan");

        //! Evaluate to \c Pass SkBuff if it is a TCP or UDP packet, \c Drop it otherwise.

        auto flow           = mfunction("flow");

        //! Evaluate to \c Pass SkBuff if it is a RTP/RTCP packet, \c Drop it otherwise.

        auto rtp            = mfunction("rtp");

        //! Evaluate to \c Pass SkBuff if it is not a fragment, \c Drop it otherwise.

        auto no_frag        = mfunction("no_frag");

        //! Evaluate to \c Pass SkBuff if it is not a fragment or if it is the first fragment, \c Drop it otherwise.

        auto no_more_frag   = mfunction("no_more_frag");

        //! Send a copy of the packet to the kernel.
        /*!
         *
         * To avoid loop, this function is ignored for packets sniffed from the kernel.
         */

        auto kernel         = mfunction("kernel");

        //! Broadcast the packet to all the sockets that have joined the group for which this computation is specified.

        auto broadcast      = mfunction("broadcast");

        //! Drop the packet. The computation evaluates to \c Drop.

        auto drop           = mfunction("drop");

        //! Unit operation implements left- and right-identity for Action monad.

        auto unit           = mfunction("unit");

        //! Unit operation implements left- and right-identity for Action monad.

        auto log_msg        = [] (std::string msg) { return mfunction1("log_msg", std::move(msg)); };

        //! Dump the payload of packet to syslog.
        /*!
         * Example:
         *
         * icmp >> log_buff
         *
         */

        auto log_buff       = mfunction("log_buff");

        //! Log the packet to syslog, with a syntax similar to tcpdump.
        /*!
         * Example:
         *
         * icmp >> log_msg ("This is an ICMP packet:") >> log_packet
         *
         */

        auto log_packet     = mfunction("log_packet");

        //! Forward the packet to the given device.
        /*!
         * This function is lazy, in that the action is logged and performed
         * when the computation is completely evaluated.
         *
         * forward ("eth1")
         */

        auto forward    = [] (std::string dev) { return mfunction1("forward", std::move(dev)); };

        //! Forward the packet to the given device.
        /*! This operation breaks the purity of the language, and it is possibly slower
         * than the lazy "forward" counterpart.
         *
         * forwardIO ("eth1")
         */

        auto forwardIO  = [] (std::string dev) { return mfunction1("forwardIO", std::move(dev)); };

        //! Forward the packet to the given device and evaluates to \c Drop.
        /*!
         * Example:
         *
         * when(is_udp, bridge ("eth1")) >> kernel
         *
         * Conditional bridge, forward the packet to eth1 if UDP, send it to the kernel otherwise.
         */

        auto bridge     = [] (std::string dev) { return mfunction1("bridge", std::move(dev)); };

        //! Forward the packet to the given device.
        /*! It evaluates to \c Pass SkBuff or \c Drop,
         * depending on the value returned by the predicate. Example:
         *
         * tee("eth1", is_udp) >> kernel
         *
         * Logically equivalent to:
         *
         * forward ("eth1") >> udp >> kernel
         *
         * Only a little bit more efficient.
         */

        auto tee_       = std::bind(details::polymorphic_mfunction1P(), "tee", _1, _2);

        //! Evaluate to \c Pass SkBuff, or forward the packet to the given device.
        /*!
         * It evaluates to \c Drop, depending on the value returned by the predicate. Example:
         *
         * tap ("eth1", is_udp) >> kernel
         *
         * Logically equivalent to:
         *
         * unless (is_udp,  forward ("eth1") >> drop ) >> kernel
         *
         * Only a little bit more efficient.
         */

        auto tap        = std::bind(details::polymorphic_mfunction1P(), "tap", _1, _2);

        //! Mark the packet with the given value.
        /*
         * Example:
         *
         * mark (42)
         */

        auto mark           = [] (unsigned long value) { return mfunction1("mark", value); };

        //! Increment the i-th counter of the current group.
        /*
         * Example:
         *
         * inc (10)
         */

        auto inc            = [] (int value) { return mfunction1("inc", value); };

        //! Decrement the i-th counter of the current group.
        /*
         * Example:
         *
         * dec (10)
         */

        auto dec            = [] (int value) { return mfunction1("dec", value); };

        //! Monadic version of \c is_l3_proto predicate.
        /*!
         * Predicates are used in conditional expressions, while monadic functions
         * are combined with Kleisli operator:
         *
         * l3_proto (0x842) >> log_msg ("Wake-on-LAN packet!")
         *
         * \see is_l3_proto
         */

        auto l3_proto       = [] (uint16_t type) { return mfunction1 ("l3_proto", type); };

        //! Monadic version of \c is_l4_proto predicate.
        /*!
         * Predicates are used in conditional expressions, while monadic functions
         * are combined with Kleisli operator:
         *
         * l4_proto(89) >> log_msg("OSFP packet!")
         *
         * \see is_l4_proto
         */

        auto l4_proto       = [] (uint8_t proto) { return mfunction1 ("l4_proto", proto); };

        //! Monadic version of \c has_port predicate.
        /*!
         * Predicates are used in conditional expressions, while monadic functions
         * are combined with Kleisli operator:
         *
         * port(80) >> log_msg ("http packet!")
         *
         * \see has_port
         */

        auto port           = [] (uint16_t p) { return mfunction1 ("port", p); };

        //! Monadic version of \c has_src_port predicate.  \see has_src_port

        auto src_port       = [] (uint16_t p) { return mfunction1 ("src_port", p); };

        //! Monadic version of \c has_dst_port predicate.  \see has_dst_port

        auto dst_port       = [] (uint16_t p) { return mfunction1 ("dst_port", p); };

        //! Monadic version of \c has_addr predicate.
        /*!
         * Predicates are used in conditional expressions, while monadic functions
         * are combined with kleisli operator:
         *
         * addr ("192.168.0.0",24) >> log_packet
         *
         * \see has_addr
         */

        auto addr = [] (const char *net, int prefix)
        {
            return mfunction2("addr", ipv4_t{net}, prefix);
        };

        //! Monadic version of \c has_src_addr predicate.  \see has_src_addr

        auto src_addr = [] (const char *net, int prefix)
        {
            return mfunction2("src_addr", ipv4_t{net}, prefix);
        };

        //! Monadic version of \c has_dst_addr predicate.  \see has_dst_addr

        auto dst_addr = [] (const char *net, int prefix)
        {
            return mfunction2("dst_addr", ipv4_t{net}, prefix);
        };

        //! Conditional execution of monadic NetFunctions.
        /*!
         * The function takes a predicate and evaluates to given the NetFunction when it evalutes to \c true,
         * otherwise does nothing.
         * Example:
         *
         * when (is_tcp, log_msg ("This is a TCP Packet"))
         *
         */

        auto when        = std::bind(details::polymorphic_mfunctionPF(), "when", _1, _2);

        //! The reverse of \c when. \see when

        auto unless      = std::bind(details::polymorphic_mfunctionPF(), "unless", _1, _2);

        //! conditional execution of monadic netfunctions.
        /*!
         * The function takes a predicate and evaluates to the first or the second expression,
         * depending on the value returned by the predicate.  Example:
         *
         * conditional (is_udp,  forward ("eth1"),  forward ("eth2"))
         *
         */

        auto conditional = std::bind(details::polymorphic_mfunctionPFF(),  "conditional", _1, _2, _3);

        //! Function that inverts a monadic NetFunction.
        /*!
         * Useful to invert filters:
         *
         * inv (ip) >> log_msg ("This is not an IPv4 Packet")
         *
         */

        auto inv         = std::bind(details::polymorphic_mfunctionF(),  "inv", _1);

        //! Function that returns the parallel of two monadic NetFunctions.
        /*!
         * Logic 'or' for manadic filters:
         *
         * par (udp, icmp) >> log_msg ("This is an UDP or ICMP Packet")
         *
         */

        auto par         = std::bind(details::polymorphic_mfunctionFF(), "par", _1, _2);

        //
        // bloom filters:
        //

        //! Predicate that evaluates to \c true when the source or the destination address
        // of the packet matches the ones specified by the bloom list.
        /*!
         * The first \c int argument specifies the size of the bloom filter.
         * Example:
         *
         * when (bloom 1024 ["192.168.0.13", "192.168.0.42"]) log_packet >> kernel
         *
         */

        auto bloom      = [] (int m, std::vector<std::string> const &ips) {
                                auto addrs = details::fmap(details::inet_addr, ips);
                                return predicate2("bloom", m, std::move(addrs));
                          };

        //! Similarly to \c bloom, evaluates to \c true when the source address
        //! of the packet matches the ones specified by the bloom list.  \see bloom

        auto bloom_src  = [] (int m, std::vector<std::string> const &ips) {
                                auto addrs = details::fmap(details::inet_addr, ips);
                                return predicate2("bloom_src", m, std::move(addrs));
                          };

        //! Similarly to \c bloom, evaluates to \c true when the destination address
        //! of the packet matches the ones specified by the bloom list.  \see bloom

        auto bloom_dst  = [] (int m, std::vector<std::string> const &ips) {
                                auto addrs = details::fmap(details::inet_addr, ips);
                                return predicate2("bloom_dst", m, std::move(addrs));
                          };

        //! Monadic counterpart of \c bloom function.  \see bloom

        auto bloom_filter      = [] (int m, std::vector<std::string> const &ips) {
                                    auto addrs = details::fmap(details::inet_addr, ips);
                                    return mfunction2("bloom_filter", m, std::move(addrs));
                                };

        //! Monadic counterpart of \c bloom_src function.  \see bloom_src

        auto bloom_src_filter  = [] (int m, std::vector<std::string> const &ips) {
                                    auto addrs = details::fmap(details::inet_addr, ips);
                                    return mfunction2("bloom_src_filter", m, std::move(addrs));
                                };

        //! Monadic counterpart of \c bloom_dst function. \see bloom_dst

        auto bloom_dst_filter  = [] (int m, std::vector<std::string> const &ips) {
                                    auto addrs = details::fmap(details::inet_addr, ips);
                                    return mfunction2("bloom_dst_filter", m, std::move(addrs));
                                };
        //
        // bloom filter, utility functions:
        //

        constexpr int bloomK = 4;

        //! Bloom filter: utility function that computes the optimal /M/, given the parameter /N/ and
        // the false-positive probability /p/.

        inline int bloom_calc_m(int n, double p)
        {
            return static_cast<int>(std::ceil( -static_cast<double>(bloomK) * n / std::log( 1.0 - std::pow(p, 1.0 / bloomK) )));
        }

        //! Bloom filter: utility function that computes the optimal /N/, given the parameter /M/ and
        // the false-positive probability /p/.

        inline int bloom_calc_n(int m, double p)
        {
            return static_cast<int>(std::ceil( -static_cast<double>(m) * std::log( 1.0 - std::pow(p, 1.0 / bloomK) ) / bloomK ));
        }

        //! Bloom filter: utility function that computes the false positive P, given /N/ and /M/ parameters.

        inline double
        bloom_calc_p(int n, int m)
        {
            return std::pow(1 - std::pow(1 - 1.0/m, n * bloomK), bloomK);
        }

    }

} // namespace lang
} // naemspace pfq
