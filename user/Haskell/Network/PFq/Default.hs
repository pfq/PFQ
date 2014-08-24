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

        is_frag,
        is_first_frag,
        is_more_frag,

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

        no_frag    ,
        no_more_frag,

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
        steer_rtp  ,
        steer_net  ,

        -- * Forwarders

        forwardIO  ,
        kernel     ,
        broadcast  ,
        drop'      ,

        -- * Conditionals

        conditional ,
        when'       ,
        unless'     ,

        -- * Miscellaneous

        unit       ,
        inc        ,
        dec        ,
        inv        ,
        par'       ,
        mark       ,
        log_msg    ,
        log_buff   ,
        log_packet ,

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

(.||.), (.&&.), (.^^.) :: NetPredicate -> NetPredicate -> NetPredicate
not' :: NetPredicate -> NetPredicate

not' p     = Combinator1 "not"  p
p1 .||. p2 = Combinator2 "or"   p1 p2
p1 .&&. p2 = Combinator2 "and"  p1 p2
p1 .^^. p2 = Combinator2 "xor"  p1 p2

infixl 7 .&&.
infixl 6 .^^.
infixl 5 .||.

-- Default comparators

(.<.), (.<=.), (.==.), (./=.), (.>.), (.>=.) :: NetProperty -> Word64 -> NetPredicate
p .<.  x = PredicateR1 "less" p x
p .<=. x = PredicateR1 "less_eq" p x
p .==. x = PredicateR1 "equal" p x
p ./=. x = PredicateR1 "not_equal" p x
p .>.  x = PredicateR1 "greater" p x
p .>=. x = PredicateR1 "greater_eq" p x

infix 4 .<.
infix 4 .<=.
infix 4 .>.
infix 4 .>=.
infix 4 .==.
infix 4 ./=.


any_bit, all_bit :: NetProperty -> Word64 -> NetPredicate
p `any_bit` x = PredicateR1 "any_bit" p x
p `all_bit` x = PredicateR1 "all_bit" p x


-- Default predicates
is_ip         = Predicate "is_ip"
is_ip6        = Predicate "is_ip6"
is_udp        = Predicate "is_udp"
is_tcp        = Predicate "is_tcp"
is_icmp       = Predicate "is_icmp"
is_udp6       = Predicate "is_udp6"
is_tcp6       = Predicate "is_tcp6"
is_icmp6      = Predicate "is_icmp6"
is_flow       = Predicate "is_flow"
has_vlan      = Predicate "has_vlan"
is_frag       = Predicate "is_frag"
is_first_frag = Predicate "is_first_frag"
is_more_frag  = Predicate "is_more_frag"

has_vid       = Predicate1 "has_vid"        :: CInt -> NetPredicate
has_mark      = Predicate1 "has_mark"       :: CULong -> NetPredicate

is_l3_proto   = Predicate1 "is_l3_proto"    :: Int16 -> NetPredicate
is_l4_proto   = Predicate1 "is_l4_proto"    :: Int8 -> NetPredicate

has_port      = Predicate1 "has_port"       :: Int16 -> NetPredicate
has_src_port  = Predicate1 "has_src_port"   :: Int16 -> NetPredicate
has_dst_port  = Predicate1 "has_dst_port"   :: Int16 -> NetPredicate

has_addr, has_src_addr, has_dst_addr        :: String -> Int -> NetPredicate

has_addr net p     = Predicate1 "has_addr" (mkNetAddr net p)
has_src_addr net p = Predicate1 "has_src_addr" (mkNetAddr net p)
has_dst_addr net p = Predicate1 "has_dst_addr" (mkNetAddr net p)

-- Default properties

get_mark    = Property "get_mark"

ip_tos      = Property "ip_tos"
ip_tot_len  = Property "ip_tot_len"
ip_id       = Property "ip_id"
ip_frag     = Property "ip_frag"
ip_ttl      = Property "ip_ttl"

tcp_source  = Property "tcp_source"
tcp_dest    = Property "tcp_dest"
tcp_hdrlen  = Property "tcp_hdrlen"

udp_source  = Property "udp_source"
udp_dest    = Property "udp_dest"
udp_len     = Property "udp_len"

icmp_type   = Property "icmp_type"
icmp_code   = Property "icmp_code"


-- Predefined in-kernel computations

steer_link      = MFunction "steer_link"    :: NetFunction
steer_vlan      = MFunction "steer_vlan"    :: NetFunction
steer_ip        = MFunction "steer_ip"      :: NetFunction
steer_ip6       = MFunction "steer_ip6"     :: NetFunction
steer_flow      = MFunction "steer_flow"    :: NetFunction
steer_rtp       = MFunction "steer_rtp"     :: NetFunction

steer_net :: String -> Int -> Int -> NetFunction
steer_net net p sub = MFunction1 "steer_net" (mkSuperNetAddr net p sub)

ip              = MFunction "ip"            :: NetFunction
ip6             = MFunction "ip6"           :: NetFunction
udp             = MFunction "udp"           :: NetFunction
tcp             = MFunction "tcp"           :: NetFunction
icmp            = MFunction "icmp"          :: NetFunction
udp6            = MFunction "udp6"          :: NetFunction
tcp6            = MFunction "tcp6"          :: NetFunction
icmp6           = MFunction "icmp6"         :: NetFunction
vlan            = MFunction "vlan"          :: NetFunction
flow            = MFunction "flow"          :: NetFunction
rtp             = MFunction "rtp"           :: NetFunction

no_frag         = MFunction "no_frag"       :: NetFunction
no_more_frag    = MFunction "no_more_frag"  :: NetFunction

-- Forwarder...

forwardIO       = MFunction1 "forwardIO"     :: String -> NetFunction
log_msg         = MFunction1 "log_msg"       :: String -> NetFunction

kernel          = MFunction "kernel"         :: NetFunction
broadcast       = MFunction "broadcast"      :: NetFunction
drop'           = MFunction "drop"           :: NetFunction
unit            = MFunction "unit"           :: NetFunction
log_buff        = MFunction "log_buff"       :: NetFunction
log_packet      = MFunction "log_packet"     :: NetFunction

inc             = MFunction1 "inc"       :: CInt     -> NetFunction
dec             = MFunction1 "dec"       :: CInt     -> NetFunction
mark            = MFunction1 "mark"      :: CULong   -> NetFunction

l3_proto        = MFunction1 "l3_proto"  :: Int16    -> NetFunction
l4_proto        = MFunction1 "l4_proto"  :: Int8     -> NetFunction

port            = MFunction1 "port"      :: Int16    -> NetFunction
src_port        = MFunction1 "src_port"  :: Int16    -> NetFunction
dst_port        = MFunction1 "dst_port"  :: Int16    -> NetFunction


addr, src_addr, dst_addr :: String -> Int -> NetFunction

addr net p      = MFunction1 "addr"     (mkNetAddr net p)
src_addr net p  = MFunction1 "src_addr" (mkNetAddr net p)
dst_addr net p  = MFunction1 "dst_addr" (mkNetAddr net p)

when'           = MFunctionPF "when"          :: NetPredicate -> NetFunction  -> NetFunction
unless'         = MFunctionPF "unless"        :: NetPredicate -> NetFunction  -> NetFunction
conditional     = MFunctionPFF "conditional"  :: NetPredicate -> NetFunction  -> NetFunction  -> NetFunction

inv             = MFunctionF "inv"            :: NetFunction  -> NetFunction
par'            = MFunctionFF "par"           :: NetFunction  -> NetFunction -> NetFunction


