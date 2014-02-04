/***************************************************************
 *
 * (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

#include <string>
#include <deque>
#include <memory>
#include <cstring>

namespace pfq_lang
{
    // qfunction
    //

    struct qfun
    {
        std::string name;
        std::pair<std::unique_ptr<char[]>, size_t> context;
    };


    // generic function
    //

    template <typename S = std::nullptr_t>
    qfun fun(std::string n, S const &context = nullptr)
    {
        // note: is_trivially_copyable is still unimplemented in g++-4.7
        // static_assert(std::is_trivially_copyable<S>::value, "fun context must be trivially copyable");

        static_assert(std::is_pod<S>::value, "context must be a pod type");

        if (std::is_same<S, std::nullptr_t>::value) {
            return qfun { std::move(n), std::make_pair(nullptr,0) };
        }
        else {
            auto ptr = new char[sizeof(S)];
            memcpy(ptr, &context, sizeof(S));
            return qfun { std::move(n), std::make_pair(std::unique_ptr<char[]>(ptr), sizeof(S)) };
        }
    }

    // binding functions ala Haskell...
    //

    inline std::deque<qfun>
    operator>>=(qfun &&lhs, qfun &&rhs)
    {
        std::deque<qfun> ret;
        ret.push_back(std::move(lhs));
        ret.push_back(std::move(rhs));
        return ret;
    }

    inline std::deque<qfun>
    operator>>=(qfun &&lhs, std::deque<qfun> &&rhs)
    {
        rhs.push_front(std::move(lhs));
        return std::move(rhs);
    }

    inline std::deque<qfun>
    operator>>=(std::deque<qfun> &&lhs, qfun &&rhs)
    {
        lhs.push_back(std::move(rhs));
        return std::move(lhs);
    }

    // default in-kernel PFQ functions...
    //

#define PFQ_GEN_FUN(fn,name) \
    template <typename S = std::nullptr_t> \
    qfun fn(S const &context = nullptr) \
    { \
        return fun(name, context); \
    }

    PFQ_GEN_FUN(steer_mac    , "steer-mac"    )
    PFQ_GEN_FUN(steer_vlan   , "steer-vlan-id")
    PFQ_GEN_FUN(steer_ipv4   , "steer-ipv4"   )
    PFQ_GEN_FUN(steer_ipv6   , "steer-ipv6"   )
    PFQ_GEN_FUN(steer_flow   , "steer-flow"   )
    PFQ_GEN_FUN(steer_rtp    , "steer-rtp"    )

    PFQ_GEN_FUN(clone        , "clone"        )
    PFQ_GEN_FUN(broadcast    , "broadcast"    )

    PFQ_GEN_FUN(vlan         , "vlan"         )
    PFQ_GEN_FUN(ipv4         , "ipv4"         )
    PFQ_GEN_FUN(udp          , "udp"          )
    PFQ_GEN_FUN(tcp          , "tcp"          )
    PFQ_GEN_FUN(flow         , "flow"         )
    PFQ_GEN_FUN(rtp          , "rtp"          )

    PFQ_GEN_FUN(strict_vlan  , "strict-vlan"  )
    PFQ_GEN_FUN(strict_ipv4  , "strict-ipv4"  )
    PFQ_GEN_FUN(strict_udp   , "strict-udp"   )
    PFQ_GEN_FUN(strict_tcp   , "strict-tcp"   )
    PFQ_GEN_FUN(strict_flow  , "strict-flow"  )

    PFQ_GEN_FUN(neg          , "neg"          )
    PFQ_GEN_FUN(par          , "par"          )


} // namespace pfq_lang
