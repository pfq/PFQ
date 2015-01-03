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

#include <arpa/inet.h>

namespace pfq { namespace lang { namespace experimental {

    /*! \file experimental.hpp
     *  \brief This header contains the PFQ/lang eDSL experimental functions.
     */

    using namespace std::placeholders;

    //
    // experimental functions...
    //

    namespace
    {
        auto class_     = [] (int value) { return mfunction1("class", value); };
        auto deliver    = [] (int value) { return mfunction1("deliver", value); };

        auto dummy      = [] (int value) { return mfunction1("dummy", value); };
        auto vdummy     = [] (std::vector<int> const &vec) { return mfunction1("vdummy", vec); };
        auto hdummy     = std::bind(details::polymorphic_mfunctionP(),  "hdummy", _1);

        auto crc16      = mfunction("crc16");
    }

} // namespace experimental
} // namespace lang
} // naemspace pfq
