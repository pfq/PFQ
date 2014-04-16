--    Copyright (c) 2011-2013, Nicola Bonelli
--    All rights reserved.
--
--    Redistribution and use in source and binary forms, with or without
--    modification, are permitted provided that the following conditions are met:
--
--    * Redistributions of source code must retain the above copyright notice,
--      this list of conditions and the following disclaimer.
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--    * Neither the name of University of Pisa nor the names of its contributors
--      may be used to endorse or promote products derived from this software
--      without specific prior written permission.
--
--    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--    AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--    ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--    LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--    SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--    INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--    CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--    ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--    POSSIBILITY OF SUCH DAMAGE.
--
--

{-# LANGUAGE ImpredicativeTypes #-}

module Network.PFqDefault
    (
        -- combinators

        (.|.),
        (.&.),
        (.^.),

        -- predicates

        is_ip,
        is_udp,
        is_tcp,
        is_icmp,

        -- monadic functions

        steer_mac  ,
        steer_vlan ,
        steer_ip   ,
        steer_ipv6 ,
        steer_flow ,
        steer_rtp  ,

        ip         ,
        ipv6       ,
        udp        ,
        tcp        ,
        vlan       ,
        icmp       ,
        flow       ,
        rtp        ,

        legacy     ,
        broadcast  ,
        sink       ,
        drop'      ,

        id'        ,
        dummy      ,
        counter    ,
        class'     ,

        -- high order functions

        hdummy,
        conditional,
        when',
        unless',

    ) where


import Network.PFqLang
import Foreign.C.Types

-- Default combinators:
--

(.|.), (.&.), (.^.) :: Predicate -> Predicate -> Predicate

p1 .|. p2 = Comb (Combinator "or" ) p1 p2
p1 .&. p2 = Comb (Combinator "and") p1 p2
p1 .^. p2 = Comb (Combinator "xor") p1 p2


-- Default predicates:
--

is_ip   = Pred "is_ip"              :: Predicate
is_udp  = Pred "is_udp"             :: Predicate
is_tcp  = Pred "is_tcp"             :: Predicate
is_icmp = Pred "is_icmp"            :: Predicate

-- Predefined in-kernel computations:
--

steer_mac   = Fun "steer_mac"       :: Computation QFunction
steer_vlan  = Fun "steer_vid"       :: Computation QFunction
steer_ip    = Fun "steer_ip"        :: Computation QFunction
steer_ipv6  = Fun "steer_ipv6"      :: Computation QFunction
steer_flow  = Fun "steer_flow"      :: Computation QFunction
steer_rtp   = Fun "steer_rtp"       :: Computation QFunction

ip          = Fun "ip"              :: Computation QFunction
ipv6        = Fun "ipv6"            :: Computation QFunction
udp         = Fun "udp"             :: Computation QFunction
tcp         = Fun "tcp"             :: Computation QFunction
vlan        = Fun "vlan"            :: Computation QFunction
icmp        = Fun "icmp"            :: Computation QFunction
flow        = Fun "flow"            :: Computation QFunction
rtp         = Fun "rtp"             :: Computation QFunction

legacy      = Fun "legacy"          :: Computation QFunction
broadcast   = Fun "broadcast"       :: Computation QFunction
sink        = Fun "sink"            :: Computation QFunction
drop'       = Fun "drop"            :: Computation QFunction
id'         = Fun  "id"             :: Computation QFunction

dummy       = Fun1 "dummy"          :: CInt -> Computation QFunction
counter     = Fun1 "counter"        :: CInt -> Computation QFunction
class'      = Fun1 "class"          :: CInt -> Computation QFunction

hdummy      = HFun "hdummy"         :: Predicate -> Computation QFunction
when'       = HFun1 "when"          :: Predicate -> Computation QFunction -> Computation QFunction
unless'     = HFun1 "unless"        :: Predicate -> Computation QFunction -> Computation QFunction
conditional = HFun2 "conditional"   :: Predicate -> Computation QFunction -> Computation QFunction -> Computation QFunction

