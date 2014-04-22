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

module Network.PFq.Default
    (
        -- combinators

        (.|.),
        (.&.),
        (.^.),

        -- predicates

        is_ip,
        is_udp,
        is_tcp,
        is_udp6,
        is_tcp6,
        is_icmp,
        is_l3_proto,
        is_l4_proto,
        has_mark,

        -- monadic functions

        steer_mac  ,
        steer_vlan ,
        steer_ip   ,
        steer_ip6  ,
        steer_flow ,
        steer_rtp  ,

        ip         ,
        ip6        ,
        udp        ,
        tcp        ,
        udp6       ,
        tcp6       ,
        vlan       ,
        l3_proto   ,
        l4_proto   ,
        icmp       ,
        flow       ,
        rtp        ,

        kernel     ,
        broadcast  ,
        sink       ,
        drop'      ,

        counter    ,
        mark       ,
        id'        ,
        dummy      ,
        class'     ,

        -- high order functions

        hdummy,
        conditional,
        when',
        unless',

    ) where


import Network.PFq.Lang
import Foreign.C.Types
import Data.Int

-- Default combinators:
--

(.|.), (.&.), (.^.) :: Predicate -> Predicate -> Predicate

p1 .|. p2 = Pred2 (Combinator "or" ) p1 p2
p1 .&. p2 = Pred2 (Combinator "and") p1 p2
p1 .^. p2 = Pred2 (Combinator "xor") p1 p2


-- Default predicates:
--

is_ip    = Pred "is_ip"             :: Predicate
is_ip6   = Pred "is_ip6"            :: Predicate
is_udp   = Pred "is_udp"            :: Predicate
is_tcp   = Pred "is_tcp"            :: Predicate
is_icmp  = Pred "is_icmp"           :: Predicate
is_udp6  = Pred "is_udp6"           :: Predicate
is_tcp6  = Pred "is_tcp6"           :: Predicate
is_icmp6 = Pred "is_icmp6"          :: Predicate
is_flow  = Pred "is_flow"           :: Predicate
has_vlan = Pred "has_vlan"          :: Predicate

has_vid  = Pred1 "has_vid"          :: CInt -> Predicate
has_mark = Pred1 "has_mark"         :: CULong -> Predicate

is_l3_proto = Pred1 "is_l3_proto"   :: Int16 -> Predicate
is_l4_proto = Pred1 "is_l4_proto"   :: Int8 -> Predicate

-- Predefined in-kernel computations:
--

steer_mac   = Fun "steer_mac"       :: Computation QFunction
steer_vlan  = Fun "steer_vlan"      :: Computation QFunction
steer_ip    = Fun "steer_ip"        :: Computation QFunction
steer_ip6   = Fun "steer_ip6"       :: Computation QFunction
steer_flow  = Fun "steer_flow"      :: Computation QFunction
steer_rtp   = Fun "steer_rtp"       :: Computation QFunction

ip          = Fun "ip"              :: Computation QFunction
ip6         = Fun "ip6"             :: Computation QFunction
udp         = Fun "udp"             :: Computation QFunction
tcp         = Fun "tcp"             :: Computation QFunction
icmp        = Fun "icmp"            :: Computation QFunction
udp6        = Fun "udp6"            :: Computation QFunction
tcp6        = Fun "tcp6"            :: Computation QFunction
icmp6       = Fun "icmp6"           :: Computation QFunction
vlan        = Fun "vlan"            :: Computation QFunction
flow        = Fun "flow"            :: Computation QFunction
rtp         = Fun "rtp"             :: Computation QFunction

kernel      = Fun "kernel"          :: Computation QFunction
broadcast   = Fun "broadcast"       :: Computation QFunction
sink        = Fun "sink"            :: Computation QFunction
drop'       = Fun "drop"            :: Computation QFunction
id'         = Fun  "id"             :: Computation QFunction

counter     = Fun1 "counter"        :: CInt -> Computation QFunction
mark        = Fun1 "mark"           :: CULong -> Computation QFunction
dummy       = Fun1 "dummy"          :: CInt -> Computation QFunction
class'      = Fun1 "class"          :: CInt -> Computation QFunction

l3_proto    = Fun1 "l3_proto"        :: Int16 -> Computation QFunction
l4_proto    = Fun1 "l4_proto"        :: Int8 -> Computation QFunction

hdummy      = HFun "hdummy"         :: Predicate -> Computation QFunction
when'       = HFun1 "when"          :: Predicate -> Computation QFunction -> Computation QFunction
unless'     = HFun1 "unless"        :: Predicate -> Computation QFunction -> Computation QFunction
conditional = HFun2 "conditional"   :: Predicate -> Computation QFunction -> Computation QFunction -> Computation QFunction

