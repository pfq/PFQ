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

#include <system_error>

namespace pfq {

    /** pfq_error represent problems related to the PFQ system.
     *  @brief Subclass of std::system_error.
     */

    class pfq_error final : public std::system_error
    {
    public:

        pfq_error(int ev, const char * reason)
        : std::system_error(ev, std::generic_category(), reason)
        {}

        pfq_error(const char *reason)
        : std::system_error(0, std::generic_category(), reason)
        {}

        pfq_error(const pfq_error &) = default;

        virtual ~pfq_error() noexcept
        {}
    };

} // namespace pfq

