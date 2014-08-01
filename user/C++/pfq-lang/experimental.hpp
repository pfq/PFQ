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

namespace pfq { namespace lang { namespace experimental {

    namespace
    {
        auto filter         = std::bind(details::polymorphic_hfunction(), "filter", _1);

        auto class_         = [] (int value) { return mfunction1("class", value); };
        auto deliver        = [] (int value) { return mfunction1("deliver", value); };

        auto forward        = [] (std::string dev) { return mfunction2("forward", std::move(dev)); };
        auto bridge         = [] (std::string dev) { return mfunction2("bridge", std::move(dev)); };

        auto tee            = std::bind(details::polymorphic_mfunction4(), "tee", _1, _2);
        auto tap            = std::bind(details::polymorphic_mfunction4(), "tap", _1, _2);
    }

} // namespace experimental
} // namespace lang
} // naemspace pfq
