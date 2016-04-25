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
    // experimental/testing functions...
    //

    namespace
    {
        auto dummy         = [] (int value) { return function("dummy", value); };
        auto dummy_ip      = [] (const char *addr) { return function("dummy_ip", ipv4_t{addr}); };
        auto dummy_cidr    = [] (CIDR data) { return function("dummy_cidr", data); };
        auto dummy_cidrs   = [] (std::vector<CIDR> const &data) { return function("dummy_cidrs", data); };

        auto dummy_vector  = [] (std::vector<int> const &vec) { return function("dummy_vector", vec); };

        auto dummy_string  = [] (std::string s) { return function("dummy_string", std::move(s)); };
        auto dummy_strings = [] (std::vector<std::string> const &vec) { return function("dummy_strings", vec); };

        auto crc16         = function("crc16");

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
