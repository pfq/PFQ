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
#include <pfq/lang/util.hpp>

#include <functional>
#include <stdexcept>
#include <vector>
#include <string>

#include <arpa/inet.h>

namespace pfq { namespace lang { namespace experimental {

    /*! \file experimental.hpp
     *  \brief This header contains the pfq-lang eDSL experimental functions.
     */

    using namespace std::placeholders;

    //
    // experimental functions...
    //

    namespace
    {
        auto class_        = [] (int value) { return function("class", value); };
        auto deliver       = [] (int value) { return function("deliver", value); };

        auto dummy         = [] (int value) { return function("dummy", value); };
        auto dummy_ip      = [] (const char *addr) { return function("dummy_ip", ipv4_t{addr}); };
        auto dummy_vector  = [] (std::vector<int> const &vec) { return function("dummy_vector", vec); };

        auto dummy_string  = [] (std::string s) { return function("dummy_string", std::move(s)); };
        auto dummy_strings = [] (std::vector<std::string> const &vec) { return function("dummy_strings", vec); };

        auto crc16         = function("crc16");


        //! Forward the socket buffer to the list of specified devices.
        /*!
         * Unlike forward, the buffer is not forwarded to the device from which it comes from.
         *
         * link_ ({"eth1", "eth2"})
         *
         */

        auto link_ = [] (std::vector<std::string> const &devs) { return function("link", devs); };


        //! Function that returns the parallel of 3 monadic NetFunctions.
        /*!
         * Logic 'or' for manadic filters:
         *
         * par3 (udp, tcp, icmp) >> log_msg ("This is an UDP/TCP or an ICMP Packet")
         *
         */

        template <typename F0, typename F1, typename F2>
        auto par3(F0 f0, F1 f1, F2 f2)
            -> decltype(function(nullptr, f0, f1, f2))
        {
            static_assert(is_monadic_function<F0>::value, "par: argument 0: monadic function expected");
            static_assert(is_monadic_function<F1>::value, "par: argument 1: monadic function expected");
            static_assert(is_monadic_function<F2>::value, "par: argument 2: monadic function expected");

            return function("par3", f0, f1, f2);
        }

        //! Function that returns the parallel of 4 monadic NetFunctions.

        template <typename F0, typename F1, typename F2, typename F3>
        auto par4(F0 f0, F1 f1, F2 f2, F3 f3)
            -> decltype(function(nullptr, f0, f1, f2, f3))
        {
            static_assert(is_monadic_function<F0>::value, "par: argument 0: monadic function expected");
            static_assert(is_monadic_function<F1>::value, "par: argument 1: monadic function expected");
            static_assert(is_monadic_function<F2>::value, "par: argument 2: monadic function expected");
            static_assert(is_monadic_function<F3>::value, "par: argument 3: monadic function expected");

            return function("par4", f0, f1, f2, f3);
        }

        //! Function that returns the parallel of 5 monadic NetFunctions.

        template <typename F0, typename F1, typename F2, typename F3, typename F4>
        auto par5(F0 f0, F1 f1, F2 f2, F3 f3, F4 f4)
            -> decltype(function(nullptr, f0, f1, f2, f3, f4))
        {
            static_assert(is_monadic_function<F0>::value, "par: argument 0: monadic function expected");
            static_assert(is_monadic_function<F1>::value, "par: argument 1: monadic function expected");
            static_assert(is_monadic_function<F2>::value, "par: argument 2: monadic function expected");
            static_assert(is_monadic_function<F3>::value, "par: argument 3: monadic function expected");
            static_assert(is_monadic_function<F4>::value, "par: argument 4: monadic function expected");

            return function("par5", f0, f1, f2, f3, f4);
        }

        //! Function that returns the parallel of 6 monadic NetFunctions.

        template <typename F0, typename F1, typename F2, typename F3, typename F4, typename F5>
        auto par6(F0 f0, F1 f1, F2 f2, F3 f3, F4 f4, F5 f5)
            -> decltype(function(nullptr, f0, f1, f2, f3, f4, f5))
        {
            static_assert(is_monadic_function<F0>::value, "par: argument 0: monadic function expected");
            static_assert(is_monadic_function<F1>::value, "par: argument 1: monadic function expected");
            static_assert(is_monadic_function<F2>::value, "par: argument 2: monadic function expected");
            static_assert(is_monadic_function<F3>::value, "par: argument 3: monadic function expected");
            static_assert(is_monadic_function<F4>::value, "par: argument 4: monadic function expected");
            static_assert(is_monadic_function<F5>::value, "par: argument 5: monadic function expected");

            return function("par6", f0, f1, f2, f3, f4, f5);
        }

        //! Function that returns the parallel of 7 monadic NetFunctions.

        template <typename F0, typename F1, typename F2, typename F3, typename F4, typename F5, typename F6>
        auto par7(F0 f0, F1 f1, F2 f2, F3 f3, F4 f4, F5 f5, F6 f6)
            -> decltype(function(nullptr, f0, f1, f2, f3, f4, f5, f6))
        {
            static_assert(is_monadic_function<F0>::value, "par: argument 0: monadic function expected");
            static_assert(is_monadic_function<F1>::value, "par: argument 1: monadic function expected");
            static_assert(is_monadic_function<F2>::value, "par: argument 2: monadic function expected");
            static_assert(is_monadic_function<F3>::value, "par: argument 3: monadic function expected");
            static_assert(is_monadic_function<F4>::value, "par: argument 4: monadic function expected");
            static_assert(is_monadic_function<F5>::value, "par: argument 5: monadic function expected");
            static_assert(is_monadic_function<F6>::value, "par: argument 6: monadic function expected");

            return function("par7", f0, f1, f2, f3, f4, f5, f6);
        }

        //! Function that returns the parallel of 8 monadic NetFunctions.

        template <typename F0, typename F1, typename F2, typename F3, typename F4, typename F5, typename F6, typename F7>
        auto par8(F0 f0, F1 f1, F2 f2, F3 f3, F4 f4, F5 f5, F6 f6, F7 f7)
            -> decltype(function(nullptr, f0, f1, f2, f3, f4, f5, f6, f7))
        {
            static_assert(is_monadic_function<F0>::value, "par: argument 0: monadic function expected");
            static_assert(is_monadic_function<F1>::value, "par: argument 1: monadic function expected");
            static_assert(is_monadic_function<F2>::value, "par: argument 2: monadic function expected");
            static_assert(is_monadic_function<F3>::value, "par: argument 3: monadic function expected");
            static_assert(is_monadic_function<F4>::value, "par: argument 4: monadic function expected");
            static_assert(is_monadic_function<F5>::value, "par: argument 5: monadic function expected");
            static_assert(is_monadic_function<F6>::value, "par: argument 6: monadic function expected");
            static_assert(is_monadic_function<F7>::value, "par: argument 7: monadic function expected");

            return function("par8", f0, f1, f2, f3, f4, f5, f6, f7);
        }

        //! Evaluate to \c Pass SkBuff if it is a GTP packet, \c Drop it otherwise.

        auto gtp            = function("gtp");

        //! Evaluate to \c Pass SkBuff if it is a GTP Control-Plane packet, \c Drop it otherwise.

        auto gtp_cp         = function("gtp_cp");

        //! Evaluate to \c Pass SkBuff if it is a GTP User-Plane packet, \c Drop it otherwise.

        auto gtp_up         = function("gtp_up");

        //! Evaluate to \c Pass SkBuff if it is a GTP packet, \c Drop it otherwise.

        auto is_gtp         = predicate("is_gtp");

        //! Evaluate to \c Pass SkBuff if it is a GTP Control-Plane packet, \c Drop it otherwise.

        auto is_gtp_cp      = predicate("is_gtp_cp");

        //! Evaluate to \c Pass SkBuff if it is a GTP Control-Plane packet, \c Drop it otherwise.

        auto is_gtp_up      = predicate("is_gtp_up");

        //! Dispatch the packet across the sockets
        /*!
         * Dispatch with a randomized algorithm that maintains the integrity
         * of per-user flows on top of GTP tunnels (Control-Plane packets are broadcasted to
         * all sockets). Example:
         *
         * steer_gtp_usr ("10.0.0.0", 8)
         */

        auto steer_gtp_usr = [] (const char *net, int prefix)
        {
            return function("steer_gtp_usr", ipv4_t{net}, prefix);
        };

    }

} // namespace experimental
} // namespace lang
} // naemspace pfq
