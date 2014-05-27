--    Copyright (c) 2011-14, Nicola Bonelli
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
        -- * Combinators

        (.||.),
        (.&&.),
        (.^^.),
        not',

        -- * Predicates

        (.<.),
        (.<=.),
        (.==.),
        (./=.),
        (.>.),
        (.>=.),

        any_bit,
        all_bit,

        is_ip,
        is_udp,
        is_tcp,
        is_icmp,
        is_ip6,
        is_udp6,
        is_tcp6,
        is_icmp6,
        is_flow,
        is_l3_proto,
        is_l4_proto,

        has_port,
        has_src_port,
        has_dst_port,

        has_addr,
        has_src_addr,
        has_dst_addr,

        has_vlan,
        has_vid,
        has_mark,

        -- * Properties

        ip_tos      ,
        ip_tot_len  ,
        ip_id       ,
        ip_frag     ,
        ip_ttl      ,
        get_mark    ,

        tcp_source  ,
        tcp_dest    ,
        tcp_hdrlen  ,

        udp_source  ,
        udp_dest    ,
        udp_len     ,

        icmp_type   ,
        icmp_code   ,

        -- * Filters

        ip         ,
        ip6        ,
        udp        ,
        tcp        ,
        icmp       ,
        udp6       ,
        tcp6       ,
        icmp6      ,
        vlan       ,
        l3_proto   ,
        l4_proto   ,
        flow       ,
        rtp        ,

        port       ,
        src_port   ,
        dst_port   ,

        addr       ,
        src_addr   ,
        dst_addr   ,

        -- * Steering functions

        steer_link ,
        steer_vlan ,
        steer_ip   ,
        steer_ip6  ,
        steer_flow ,
        steer_net  ,
        steer_rtp  ,

        -- * Forwarders

        kernel     ,
        broadcast  ,
        sink       ,
        drop'      ,
        forward    ,
        forward_kernel ,

        -- * Conditionals

        conditional ,
        when'       ,
        unless'     ,

        -- * Miscellaneous

        unit       ,
        inc        ,
        dec        ,
        mark       ,
        class'     ,
        deliver    ,
        crc16      ,
        log_packet ,
        dummy      ,
        hdummy     ,

    ) where


import Network.PFq.Lang
import Foreign.C.Types
import Data.Int

import Data.Word
import Network.Socket
import System.IO.Unsafe

import Foreign.Storable.Tuple()

-- import Data.Bits
-- import Data.Endian


-- prefix2mask :: Int -> Word32
-- prefix2mask p =  toBigEndian $ fromIntegral $ complement (shiftL (1 :: Word64) (32 - p) - 1)

-- mkNetAddr :: String -> Int -> Word64
-- mkNetAddr net p = let a = unsafePerformIO (inet_addr net)
--                       b = prefix2mask p
--                   in  shiftL (fromIntegral a :: Word64) 32 .|. (fromIntegral b :: Word64)


mkNetAddr :: String -> Int -> (Word32, CInt)
mkNetAddr net p = let a = unsafePerformIO (inet_addr net)
                  in (fromIntegral a, fromIntegral p)

mkSuperNetAddr :: String -> Int -> Int -> (Word32, CInt, CInt)
mkSuperNetAddr net p sub = let a = unsafePerformIO (inet_addr net)
                           in (fromIntegral a, fromIntegral p, fromIntegral sub)

-- Default combinators

(.||.), (.&&.), (.^^.) :: Predicate -> Predicate -> Predicate
not' :: Predicate -> Predicate

p1 .||. p2 = Pred2 (Combinator "or" ) p1 p2
p1 .&&. p2 = Pred2 (Combinator "and") p1 p2
p1 .^^. p2 = Pred2 (Combinator "xor") p1 p2
not' p     = Pred2 (Combinator "not") p  p


infixl 7 .&&.
infixl 6 .^^.
infixl 5 .||.

-- Default comparators

(.<.), (.<=.), (.==.), (./=.), (.>.), (.>=.) :: Property -> Word64 -> Predicate
p .<.  x = Pred4 "less" p x
p .<=. x = Pred4 "less_eq" p x
p .==. x = Pred4 "equal" p x
p ./=. x = Pred4 "not_equal" p x
p .>.  x = Pred4 "greater" p x
p .>=. x = Pred4 "greater_eq" p x

infix 4 .<.
infix 4 .<=.
infix 4 .>.
infix 4 .>=.
infix 4 .==.
infix 4 ./=.


any_bit, all_bit :: Property -> Word64 -> Predicate
p `any_bit` x = Pred4 "any_bit" p x
p `all_bit` x = Pred4 "all_bit" p x


-- Default predicates

is_ip       = Pred "is_ip"              :: Predicate
is_ip6      = Pred "is_ip6"             :: Predicate
is_udp      = Pred "is_udp"             :: Predicate
is_tcp      = Pred "is_tcp"             :: Predicate
is_icmp     = Pred "is_icmp"            :: Predicate
is_udp6     = Pred "is_udp6"            :: Predicate
is_tcp6     = Pred "is_tcp6"            :: Predicate
is_icmp6    = Pred "is_icmp6"           :: Predicate
is_flow     = Pred "is_flow"            :: Predicate
has_vlan    = Pred "has_vlan"           :: Predicate

has_vid     = Pred1 "has_vid"           :: CInt -> Predicate
has_mark    = Pred1 "has_mark"          :: CULong -> Predicate

is_l3_proto = Pred1 "is_l3_proto"       :: Int16 -> Predicate
is_l4_proto = Pred1 "is_l4_proto"       :: Int8 -> Predicate

has_port     = Pred1 "has_port"         :: Int16 -> Predicate
has_src_port = Pred1 "has_src_port"     :: Int16 -> Predicate
has_dst_port = Pred1 "has_dst_port"     :: Int16 -> Predicate

has_addr, has_src_addr, has_dst_addr    :: String -> Int -> Predicate

has_addr net p     = Pred1 "has_addr"     (mkNetAddr net p)
has_src_addr net p = Pred1 "has_src_addr" (mkNetAddr net p)
has_dst_addr net p = Pred1 "has_dst_addr" (mkNetAddr net p)

-- Default properties

get_mark    = Prop "get_mark"

ip_tos      = Prop "ip_tos"
ip_tot_len  = Prop "ip_tot_len"
ip_id       = Prop "ip_id"
ip_frag     = Prop "ip_frag"
ip_ttl      = Prop "ip_ttl"

tcp_source  = Prop "tcp_source"
tcp_dest    = Prop "tcp_dest"
tcp_hdrlen  = Prop "tcp_hdrlen"

udp_source  = Prop "udp_source"
udp_dest    = Prop "udp_dest"
udp_len     = Prop "udp_len"

icmp_type   = Prop "icmp_type"
icmp_code   = Prop "icmp_code"

-- Predefined in-kernel computations

steer_link  = Fun "steer_link"      :: NetFunction (SkBuff -> Action SkBuff)
steer_vlan  = Fun "steer_vlan"      :: NetFunction (SkBuff -> Action SkBuff)
steer_ip    = Fun "steer_ip"        :: NetFunction (SkBuff -> Action SkBuff)
steer_ip6   = Fun "steer_ip6"       :: NetFunction (SkBuff -> Action SkBuff)
steer_flow  = Fun "steer_flow"      :: NetFunction (SkBuff -> Action SkBuff)
steer_rtp   = Fun "steer_rtp"       :: NetFunction (SkBuff -> Action SkBuff)

steer_net :: String -> Int -> Int -> NetFunction (SkBuff -> Action SkBuff)
steer_net net p sub = (Fun1 "steer_net" (mkSuperNetAddr net p sub))

ip          = Fun "ip"              :: NetFunction (SkBuff -> Action SkBuff)
ip6         = Fun "ip6"             :: NetFunction (SkBuff -> Action SkBuff)
udp         = Fun "udp"             :: NetFunction (SkBuff -> Action SkBuff)
tcp         = Fun "tcp"             :: NetFunction (SkBuff -> Action SkBuff)
icmp        = Fun "icmp"            :: NetFunction (SkBuff -> Action SkBuff)
udp6        = Fun "udp6"            :: NetFunction (SkBuff -> Action SkBuff)
tcp6        = Fun "tcp6"            :: NetFunction (SkBuff -> Action SkBuff)
icmp6       = Fun "icmp6"           :: NetFunction (SkBuff -> Action SkBuff)
vlan        = Fun "vlan"            :: NetFunction (SkBuff -> Action SkBuff)
flow        = Fun "flow"            :: NetFunction (SkBuff -> Action SkBuff)
rtp         = Fun "rtp"             :: NetFunction (SkBuff -> Action SkBuff)

forward_kernel = Fun "forward_kernel" :: NetFunction (SkBuff -> Action SkBuff)
kernel      = Fun "kernel"          :: NetFunction (SkBuff -> Action SkBuff)
broadcast   = Fun "broadcast"       :: NetFunction (SkBuff -> Action SkBuff)
sink        = Fun "sink"            :: NetFunction (SkBuff -> Action SkBuff)
drop'       = Fun "drop"            :: NetFunction (SkBuff -> Action SkBuff)
unit        = Fun "unit"            :: NetFunction (SkBuff -> Action SkBuff)
log_packet  = Fun "log_packet"      :: NetFunction (SkBuff -> Action SkBuff)

crc16       = Fun "crc16"           :: NetFunction (SkBuff -> Action SkBuff)
inc         = Fun1 "inc"            :: CInt -> NetFunction (SkBuff -> Action SkBuff)
dec         = Fun1 "dec"            :: CInt -> NetFunction (SkBuff -> Action SkBuff)
mark        = Fun1 "mark"           :: CULong -> NetFunction (SkBuff -> Action SkBuff)
forward     = Fun1 "forward"        :: CInt -> NetFunction (SkBuff -> Action SkBuff)
dummy       = Fun1 "dummy"          :: CInt -> NetFunction (SkBuff -> Action SkBuff)
class'      = Fun1 "class"          :: CInt -> NetFunction (SkBuff -> Action SkBuff)
deliver     = Fun1 "deliver"        :: CInt -> NetFunction (SkBuff -> Action SkBuff)

l3_proto    = Fun1 "l3_proto"       :: Int16 -> NetFunction (SkBuff -> Action SkBuff)
l4_proto    = Fun1 "l4_proto"       :: Int8 -> NetFunction (SkBuff -> Action SkBuff)

port        = Fun1 "port"           :: Int16 -> NetFunction (SkBuff -> Action SkBuff)
src_port    = Fun1 "src_port"       :: Int16 -> NetFunction (SkBuff -> Action SkBuff)
dst_port    = Fun1 "dst_port"       :: Int16 -> NetFunction (SkBuff -> Action SkBuff)


addr, src_addr, dst_addr :: String -> Int -> NetFunction (SkBuff -> Action SkBuff)

addr net p     = Fun1 "addr"     (mkNetAddr net p)
src_addr net p = Fun1 "src_addr" (mkNetAddr net p)
dst_addr net p = Fun1 "dst_addr" (mkNetAddr net p)

hdummy      = HFun "hdummy"         :: Predicate -> NetFunction (SkBuff -> Action SkBuff)
when'       = HFun1 "when"          :: Predicate -> NetFunction (SkBuff -> Action SkBuff) -> NetFunction (SkBuff -> Action SkBuff)
unless'     = HFun1 "unless"        :: Predicate -> NetFunction (SkBuff -> Action SkBuff) -> NetFunction (SkBuff -> Action SkBuff)
conditional = HFun2 "conditional"   :: Predicate -> NetFunction (SkBuff -> Action SkBuff) -> NetFunction (SkBuff -> Action SkBuff) -> NetFunction (SkBuff -> Action SkBuff)

