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
#include <stdexcept>
#include <vector>
#include <string>
#include <cmath>

#include <arpa/inet.h>

namespace pfq { namespace lang { namespace experimental {

    using namespace std::placeholders;

    namespace
    {

        auto filter     = std::bind(details::polymorphic_mfunctionP(), "filter", _1);

        auto class_     = [] (int value) { return mfunction1("class", value); };
        auto deliver    = [] (int value) { return mfunction1("deliver", value); };

        auto forward    = [] (std::string dev) { return mfunction1("forward", std::move(dev)); };
        auto bridge     = [] (std::string dev) { return mfunction1("bridge", std::move(dev)); };

        auto tee        = std::bind(details::polymorphic_mfunction1P(), "tee", _1, _2);
        auto tap        = std::bind(details::polymorphic_mfunction1P(), "tap", _1, _2);

        auto dummy      = [] (int value) { return mfunction1("dummy", value); };
        auto vdummy     = [] (std::vector<int> const &vec) { return mfunction1("vdummy", vec); };
        auto hdummy     = std::bind(details::polymorphic_mfunctionP(),  "hdummy", _1);

        auto crc16      = mfunction("crc16");

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

        // bloom filter: utility functions:
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

        // experimental vlan functions...
        //

        auto vlan_id = [] (std::vector<int> const &vid) {
                                return predicate1("vlan_id", vid);
                       };

        auto vlan_id_filter = [] (std::vector<int> const &vid) {
                                    return mfunction1("vlan_id_filter", vid);
                              };

        auto steer_field = [] (int off, int size) {
                                return mfunction2("steer_field", off, size);
                           };
    }

} // namespace experimental
} // namespace lang
} // naemspace pfq
