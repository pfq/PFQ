/***************************************************************

  Copyright (c) 2011-2013, Nicola Bonelli 
  All rights reserved. 

  Redistribution and use in source and binary forms, with or without 
  modification, are permitted provided that the following conditions are met: 

 * Redistributions of source code must retain the above copyright notice, 
 this list of conditions and the following disclaimer. 
 * Redistributions in binary form must reproduce the above copyright 
 notice, this list of conditions and the following disclaimer in the 
 documentation and/or other materials provided with the distribution. 
 * Neither the name of University of Pisa nor the names of its contributors 
 may be used to endorse or promote products derived from this software 
 without specific prior written permission. 

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
 ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
 LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
 CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
 POSSIBILITY OF SUCH DAMAGE.

 ***************************************************************/

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

    PFQ_GEN_FUN(strict_vlan  , "strict-vlan"  )  
    PFQ_GEN_FUN(strict_ipv4  , "strict-ipv4"  )  
    PFQ_GEN_FUN(strict_udp   , "strict-udp"   )  
    PFQ_GEN_FUN(strict_tcp   , "strict-tcp"   )  
    PFQ_GEN_FUN(strict_flow  , "strict-flow"  )  

    PFQ_GEN_FUN(neg          , "neg"          )  
    PFQ_GEN_FUN(par          , "par"          )  


} // namespace pfq_lang
