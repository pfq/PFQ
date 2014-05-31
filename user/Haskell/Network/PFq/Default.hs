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
        -- * Combinators :: Predicate -> Predicate -> Predicate

        (.||.),
        (.&&.),
        (.^^.),
        not',

        -- * Predicates :: SkBuff -> Bool

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

        -- * Properties :: SkBuff -> Word64

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
        steer_net  ,
        steer_rtp  ,

        -- * Forwarders

        kernel     ,
        broadcast  ,
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

(.||.), (.&&.), (.^^.) :: NetPredicate -> NetPredicate -> NetPredicate
not' :: NetPredicate -> NetPredicate

not' p     = Combinator1 ("not", typeOf'(not')) p
p1 .||. p2 = Combinator2 ("or",  typeOf'((.||.))) p1 p2
p1 .&&. p2 = Combinator2 ("and", typeOf'((.&&.))) p1 p2
p1 .^^. p2 = Combinator2 ("xor", typeOf'((.^^.))) p1 p2

infixl 7 .&&.
infixl 6 .^^.
infixl 5 .||.

-- Default comparators

(.<.), (.<=.), (.==.), (./=.), (.>.), (.>=.) :: NetProperty -> Word64 -> NetPredicate
p .<.  x = Predicate2 ("less", typeOf'((.<.))) p x
p .<=. x = Predicate2 ("less_eq", typeOf'((.<=.))) p x
p .==. x = Predicate2 ("equal", typeOf'((.==.))) p x
p ./=. x = Predicate2 ("not_equal", typeOf'((./=.))) p x
p .>.  x = Predicate2 ("greater", typeOf'((.>.))) p x
p .>=. x = Predicate2 ("greater_eq", typeOf'((.>=.))) p x

infix 4 .<.
infix 4 .<=.
infix 4 .>.
infix 4 .>=.
infix 4 .==.
infix 4 ./=.


any_bit, all_bit :: NetProperty -> Word64 -> NetPredicate
p `any_bit` x = Predicate2 ("any_bit", typeOf'(any_bit)) p x
p `all_bit` x = Predicate2 ("all_bit", typeOf'(all_bit)) p x


-- Default predicates

is_ip         = Predicate ("is_ip",typeOf'(is_ip))
is_ip6        = Predicate ("is_ip6",typeOf'(is_ip6))
is_udp        = Predicate ("is_udp",typeOf'(is_udp))
is_tcp        = Predicate ("is_tcp",typeOf'(is_tcp))
is_icmp       = Predicate ("is_icmp",typeOf'(is_icmp))
is_udp6       = Predicate ("is_udp6",typeOf'(is_udp6))
is_tcp6       = Predicate ("is_tcp6",typeOf'(is_tcp6))
is_icmp6      = Predicate ("is_icmp6",typeOf'(is_icmp6))
is_flow       = Predicate ("is_flow",typeOf'(is_flow))
has_vlan      = Predicate ("has_vlan",typeOf'(has_vlan))
is_frag       = Predicate ("is_frag",typeOf'(is_frag))
is_first_frag = Predicate ("is_first_frag",typeOf'(is_first_frag))
is_more_frag  = Predicate ("is_more_frag",typeOf'(is_more_frag))

has_vid       = Predicate1 ("has_vid",typeOf'(has_vid))             :: CInt -> NetPredicate
has_mark      = Predicate1 ("has_mark",typeOf'(has_mark))           :: CULong -> NetPredicate

is_l3_proto   = Predicate1 ("is_l3_proto",typeOf'(is_l3_proto))     :: Int16 -> NetPredicate
is_l4_proto   = Predicate1 ("is_l4_proto",typeOf'(is_l4_proto))     :: Int8 -> NetPredicate

has_port      = Predicate1 ("has_port",typeOf'(has_port))           :: Int16 -> NetPredicate
has_src_port  = Predicate1 ("has_src_port",typeOf'(has_src_port))   :: Int16 -> NetPredicate
has_dst_port  = Predicate1 ("has_dst_port",typeOf'(has_dst_port))   :: Int16 -> NetPredicate

has_addr, has_src_addr, has_dst_addr        :: String -> Int -> NetPredicate

has_addr net p     = Predicate1 ("has_addr",typeOf'(has_addr))     (mkNetAddr net p)
has_src_addr net p = Predicate1 ("has_src_addr",typeOf'(has_src_addr)) (mkNetAddr net p)
has_dst_addr net p = Predicate1 ("has_dst_addr",typeOf'(has_dst_addr)) (mkNetAddr net p)

-- Default properties

get_mark    = Property ("get_mark",typeOf'(get_mark))

ip_tos      = Property ("ip_tos",typeOf'(ip_tos))
ip_tot_len  = Property ("ip_tot_len",typeOf'(ip_tot_len))
ip_id       = Property ("ip_id",typeOf'(ip_id))
ip_frag     = Property ("ip_frag",typeOf'(ip_frag))
ip_ttl      = Property ("ip_ttl",typeOf'(ip_ttl))

tcp_source  = Property ("tcp_source",typeOf'(tcp_source))
tcp_dest    = Property ("tcp_dest",typeOf'(tcp_dest))
tcp_hdrlen  = Property ("tcp_hdrlen",typeOf'(tcp_hdrlen))

udp_source  = Property ("udp_source",typeOf'(udp_source))
udp_dest    = Property ("udp_dest",typeOf'(udp_dest))
udp_len     = Property ("udp_len",typeOf'())

icmp_type   = Property ("icmp_type",typeOf'(icmp_type))
icmp_code   = Property ("icmp_code",typeOf'(icmp_code))


-- Predefined in-kernel computations

steer_net :: String -> Int -> Int -> NetFunction
steer_net net p sub = (MFunction1 ("steer_net",typeOf'(steer_net)) (mkSuperNetAddr net p sub))

steer_link      = MFunction ("steer_link",typeOf'(steer_link))          :: NetFunction
steer_vlan      = MFunction ("steer_vlan",typeOf'(steer_vlan))          :: NetFunction
steer_ip        = MFunction ("steer_ip",typeOf'(steer_ip))              :: NetFunction
steer_ip6       = MFunction ("steer_ip6",typeOf'(steer_ip6))            :: NetFunction
steer_flow      = MFunction ("steer_flow",typeOf'(steer_flow))          :: NetFunction
steer_rtp       = MFunction ("steer_rtp",typeOf'(steer_rtp))            :: NetFunction

ip              = MFunction ("ip",typeOf'(ip))                  :: NetFunction
ip6             = MFunction ("ip6",typeOf'(ip6))                :: NetFunction
udp             = MFunction ("udp",typeOf'(udp))                :: NetFunction
tcp             = MFunction ("tcp",typeOf'(tcp))                :: NetFunction
icmp            = MFunction ("icmp",typeOf'(icmp))              :: NetFunction
udp6            = MFunction ("udp6",typeOf'(udp6))              :: NetFunction
tcp6            = MFunction ("tcp6",typeOf'(tcp6))              :: NetFunction
icmp6           = MFunction ("icmp6",typeOf'(icmp6))            :: NetFunction
vlan            = MFunction ("vlan",typeOf'(vlan))              :: NetFunction
flow            = MFunction ("flow",typeOf'(flow))              :: NetFunction
rtp             = MFunction ("rtp",typeOf'(rtp))                :: NetFunction

no_frag         = MFunction ("no_frag",typeOf'(no_frag))           :: NetFunction
no_more_frag    = MFunction ("no_more_frag",typeOf'(no_more_frag)) :: NetFunction

forward_kernel = MFunction ("forward_kernel",typeOf'(forward_kernel))   :: NetFunction
kernel         = MFunction ("kernel",typeOf'(kernel))                   :: NetFunction
broadcast      = MFunction ("broadcast",typeOf'(broadcast))             :: NetFunction
drop'          = MFunction ("drop",typeOf'(drop'))                      :: NetFunction
unit           = MFunction ("unit",typeOf'(unit))                       :: NetFunction
log_packet     = MFunction ("log_packet",typeOf'())                     :: NetFunction
crc16          = MFunction ("crc16",typeOf'(crc16))                     :: NetFunction

inc            = MFunction1 ("inc",typeOf'(inc))               :: CInt     -> NetFunction
dec            = MFunction1 ("dec",typeOf'(dec))               :: CInt     -> NetFunction
mark           = MFunction1 ("mark",typeOf'(mark))             :: CULong   -> NetFunction
forward        = MFunction1 ("forward",typeOf'(forward))       :: CInt     -> NetFunction
dummy          = MFunction1 ("dummy",typeOf'(dummy))           :: CInt     -> NetFunction
class'         = MFunction1 ("class",typeOf'(class'))          :: CInt     -> NetFunction
deliver        = MFunction1 ("deliver",typeOf'(deliver))       :: CInt     -> NetFunction

l3_proto       = MFunction1 ("l3_proto",typeOf'(l3_proto))     :: Int16    -> NetFunction
l4_proto       = MFunction1 ("l4_proto",typeOf'(l4_proto))     :: Int8     -> NetFunction

port           = MFunction1 ("port",typeOf'(port))             :: Int16    -> NetFunction
src_port       = MFunction1 ("src_port",typeOf'(src_port))     :: Int16    -> NetFunction
dst_port       = MFunction1 ("dst_port",typeOf'(dst_port))     :: Int16    -> NetFunction


addr, src_addr, dst_addr :: String -> Int -> NetFunction

addr net p     = MFunction1 ("addr",typeOf'(addr))     (mkNetAddr net p)
src_addr net p = MFunction1 ("src_addr",typeOf'(src_addr)) (mkNetAddr net p)
dst_addr net p = MFunction1 ("dst_addr",typeOf'(dst_addr)) (mkNetAddr net p)

hdummy         = HFunction ("hdummy",typeOf'(hdummy))            :: NetPredicate -> NetFunction
when'          = HFunction1 ("when",typeOf'(when'))              :: NetPredicate -> NetFunction  -> NetFunction
unless'        = HFunction1 ("unless",typeOf'(unless'))          :: NetPredicate -> NetFunction  -> NetFunction
conditional    = HFunction2 ("conditional",typeOf'(conditional)) :: NetPredicate -> NetFunction  -> NetFunction  -> NetFunction

