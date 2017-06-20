--    Copyright (c) 2011-16, Nicola Bonelli
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
{-# LANGUAGE OverloadedStrings  #-}

module Network.PFQ.Lang.Default
    (
      -- * Predicates
      -- | Collection of predicates used in conditional expressions.

      is_ip
    , is_udp
    , is_tcp
    , is_icmp
    , is_flow
    , is_l3_proto
    , is_l4_proto

    , is_frag
    , is_first_frag
    , is_more_frag

    , is_rtp
    , is_rtcp
    , is_sip
    , is_voip

    , has_port
    , has_src_port
    , has_dst_port

    , has_addr
    , has_src_addr
    , has_dst_addr

    , has_state
    , has_mark
    , has_vlan
    , has_vid
    , vlan_id

      -- * Properties

    , ip_tos
    , ip_tot_len
    , ip_id
    , ip_frag
    , ip_ttl
    , get_mark
    , get_state
    , tcp_source
    , tcp_dest
    , tcp_hdrlen
    , udp_source
    , udp_dest
    , udp_len
    , icmp_type
    , icmp_code

      -- * Combinators

    , (.||.)
    , (.&&.)
    , (.^^.)
    , Network.PFQ.Lang.Default.not
    , inv
    , par
    , par3
    , par4
    , par5
    , par6
    , par7
    , par8

        -- * Comparators
        -- | Take a NetProperty, a value, and return a predicate that compares the values.

    , (.<)
    , (.<=)
    , (.==)
    , (./=)
    , (.>)
    , (.>=)
    , (<.)
    , (<=.)
    , (==.)
    , (/=.)
    , (>.)
    , (>=.)
    , any_bit
    , all_bit

        -- * Conditionals

    , conditional
    , when
    , unless

        -- * Filters
        -- | A collection of monadic NetFunctions.

    , Network.PFQ.Lang.Default.filter
    , ip
    , udp
    , tcp
    , icmp
    , vlan
    , l3_proto
    , l4_proto
    , flow
    , rtp
    , rtcp
    , sip
    , voip
    , vlan_id_filter
    , no_frag
    , no_more_frag
    , port
    , src_port
    , dst_port
    , addr
    , src_addr
    , dst_addr

        -- * Steering functions
        -- | Monadic functions used to dispatch packets across sockets.
        -- They evaluate to /Steer Hash Qbuff/, if the packet has a certain property, /Drop/ otherwise.

    , steer_rrobin
    , steer_rss
    , steer_to
    , steer_link
    , steer_local_link
    , double_steer_mac
    , steer_vlan
    , steer_p2p
    , double_steer_ip
    , steer_local_ip
    , steer_flow
    , steer_local_net
    , steer_field
    , double_steer_field
    , steer_field_symmetric
    , steer_rtp
    , steer_voip

        -- * Forwarders
    , kernel
    , detour
    , broadcast
    , Network.PFQ.Lang.Default.drop
    , classify
    , forward
    , forwardIO
    , link
    , bridge
    , tee
    , tap

        -- * Logging
    , log_msg
    , log_buff
    , log_packet

        -- * Bloom Filters

    , bloom
    , bloom_src
    , bloom_dst
    , bloom_filter
    , bloom_src_filter
    , bloom_dst_filter
    , bloomCalcN
    , bloomCalcM
    , bloomCalcP

        -- * Miscellaneous

    , unit
    , inc
    , dec
    , mark
    , put_state

    ) where


import           Network.PFQ.Lang

import           Data.Word

import           Network.Socket
import           System.IO.Unsafe

import           Foreign.Storable.Tuple ()

-- Default combinators

-- | Combine two predicate expressions with a specific boolean /or/ operation.
(.||.) :: NetPredicate -> NetPredicate -> NetPredicate

-- | Combine two predicate expressions with a specific boolean /and/ operation.
(.&&.) :: NetPredicate -> NetPredicate -> NetPredicate

-- | Combine two predicate expressions with a specific boolean /xor/ operation.
(.^^.) :: NetPredicate -> NetPredicate -> NetPredicate

-- | Return a new predicate that evaluates to /True/, when the given one evaluates to
-- false, and vice versa.
not :: NetPredicate -> NetPredicate

not = Combinator1 "not"
p1 .||. p2 = Combinator2 "or"   p1 p2
p1 .&&. p2 = Combinator2 "and"  p1 p2
p1 .^^. p2 = Combinator2 "xor"  p1 p2

infixl 7 .&&.
infixl 6 .^^.
infixl 5 .||.

-- | Return a predicate that evaluates to /True/, if the property is less than
-- the given value. Example:
--
-- >  when (ip_ttl .< 64) drop

(.<)  :: NetProperty -> Word64 -> NetPredicate
(.<=) :: NetProperty -> Word64 -> NetPredicate
(.==) :: NetProperty -> Word64 -> NetPredicate
(./=) :: NetProperty -> Word64 -> NetPredicate
(.>)  :: NetProperty -> Word64 -> NetPredicate
(.>=) :: NetProperty -> Word64 -> NetPredicate

(<.)  :: Word64 -> NetProperty ->  NetPredicate
(<=.) :: Word64 -> NetProperty ->  NetPredicate
(==.) :: Word64 -> NetProperty ->  NetPredicate
(/=.) :: Word64 -> NetProperty ->  NetPredicate
(>.)  :: Word64 -> NetProperty ->  NetPredicate
(>=.) :: Word64 -> NetProperty ->  NetPredicate

p .<  x = Predicate "less" p x () () () () () ()
p .<= x = Predicate "less_eq" p x () () () () () ()
p .== x = Predicate "equal" p x () () () () () ()
p ./= x = Predicate "not_equal" p x () () () () () ()
p .>  x = Predicate "greater" p x () () () () () ()
p .>= x = Predicate "greater_eq" p x () () () () () ()

x >.  p = Predicate "less" p x () () () () () ()
x >=. p = Predicate "less_eq" p x () () () () () ()
x ==. p = Predicate "equal" p x () () () () () ()
x /=. p = Predicate "not_equal" p x () () () () () ()
x <.  p = Predicate "greater" p x () () () () () ()
x <=. p = Predicate "greater_eq" p x () () () () () ()

infix 4 .<
infix 4 .<=
infix 4 .>
infix 4 .>=
infix 4 .==
infix 4 ./=

infix 4 <.
infix 4 <=.
infix 4 >.
infix 4 >=.
infix 4 ==.
infix 4 /=.

-- | Return a predicate that evaluates to /True/, if the property has at least
-- one bit set among those specified by the given mask.
any_bit :: NetProperty
        -> Word64       -- ^ comparison mask
        -> NetPredicate

-- | Return a predicate that evaluates to /True/, if the property has all bits
-- set among those specified in the given mask.
all_bit :: NetProperty
        -> Word64       -- ^ comparison mask
        -> NetPredicate

p `any_bit` x = Predicate "any_bit" p x () () () () () ()
p `all_bit` x = Predicate "all_bit" p x () () () () () ()


-- | Evaluate to /True/ if the Qbuff is an IPv4 packet.
is_ip = Predicate "is_ip" () () () () () () () ()

-- | Evaluate to /True/ if the Qbuff is an UDP packet.
is_udp = Predicate "is_udp" () () () () () () () ()

-- | Evaluate to /True/ if the Qbuff is a TCP packet.
is_tcp = Predicate "is_tcp" () () () () () () () ()

-- | Evaluate to /True/ if the Qbuff is an ICMP packet.
is_icmp = Predicate "is_icmp" () () () () () () () ()

-- | Evaluate to /True/ if the Qbuff is an UDP or TCP packet.
is_flow = Predicate "is_flow" () () () () () () () ()

-- | Evaluate to /True/ if the Qbuff has a vlan tag.
has_vlan = Predicate "has_vlan" () () () () () () () ()

-- | Evaluate to /True/ if the Qbuff is a TCP fragment.
is_frag = Predicate "is_frag" () () () () () () () ()

-- | Evaluate to /True/ if the Qbuff is the first TCP fragment.
is_first_frag = Predicate "is_first_frag" () () () () () () () ()

-- | Evaluate to /True/ if the Qbuff is a TCP fragment, but the first.
is_more_frag = Predicate "is_more_frag" () () () () () () () ()

-- | Evaluate to /True/ if the Qbuff has the given vlan id.
--
-- > has_vid 42
has_vid :: Int -> NetPredicate
has_vid x = Predicate "has_vid" x () () () () () () ()

-- | Evaluate to /True/ if the Qbuff has the given mark, set by 'mark' function.
--
-- > has_mark 11
has_mark :: Word32 -> NetPredicate
has_mark x = Predicate "has_mark" x () () () () () () ()

-- | Evaluate to /True/ if the state of the computation is set to the given value, possibly by 'put_state' function.
--
-- > has_state 11
has_state :: Word32 -> NetPredicate
has_state x = Predicate "has_state" x () () () () () () ()


-- | Evaluate to /True/ if the Qbuff has the given Layer3 protocol.
is_l3_proto :: Word16 -> NetPredicate
is_l3_proto x = Predicate "is_l3_proto" x () () () () () () ()

-- | Evaluate to /True/ if the Qbuff has the given Layer4 protocol.
is_l4_proto :: Word8 -> NetPredicate
is_l4_proto x = Predicate "is_l4_proto" x () () () () () () ()

is_rtp, is_rtcp, is_sip, is_voip :: NetPredicate

-- | Evaluate to /True/ if the Qbuff is a RTP packet.
is_rtp = Predicate "is_rtp" () () () () () () () ()

-- | Evaluate to /True/ if the Qbuff is a RTCP packet.
is_rtcp = Predicate "is_rtcp" () () () () () () () ()

-- | Evaluate to /True/ if the Qbuff is a SIP packet.
is_sip = Predicate "is_sip" () () () () () () () ()

-- | Evaluate to /True/ if the Qbuff is a VoIP packet (RTP|RTCP|SIP).
is_voip = Predicate "is_voip" () () () () () () () ()


has_port, has_src_port, has_dst_port :: Word16 -> NetPredicate

-- | Evaluate to /True/ if the Qbuff has the given source or destination port.
--
-- If the transport protocol is not present or has no port, the predicate evaluates to False.
--
-- > has_port 80
has_port x = Predicate "has_port" x () () () () () () ()

-- | Evaluate to /True/ if the Qbuff has the given source port.
--
-- If the transport protocol is not present or has no port, the predicate evaluates to False.
has_src_port x = Predicate "has_src_port" x () () () () () () ()

-- | Evaluate to /True/ if the Qbuff has the given destination port.
--
-- If the transport protocol is not present or has no port, the predicate evaluates to False.
has_dst_port x = Predicate "has_dst_port" x () () () () () () ()

-- | Evaluate to /True/ if the source or destination IP address matches the given network address. I.e.,
--
-- > has_addr "192.168.0.0/24"
-- > has_addr (CIDR ("192.168.0.0", 24))

has_addr :: CIDR -> NetPredicate

-- | Evaluate to /True/ if the source IP address matches the given network address.
has_src_addr :: CIDR -> NetPredicate

-- | Evaluate to /True/ if the destination IP address matches the given network address.
has_dst_addr :: CIDR -> NetPredicate

has_addr a       = Predicate "has_addr"     a () () () () () () ()
has_src_addr a   = Predicate "has_src_addr" a () () () () () () ()
has_dst_addr a   = Predicate "has_dst_addr" a () () () () () () ()

-- | Evaluate to the mark set by 'mark' function. By default packets are marked with 0.
get_mark = Property "get_mark" () () () () () () () ()

-- | Evaluate to the state of the computation (possibly set by 'state' function).
get_state = Property "get_state" () () () () () () () ()


-- | Evaluate to the /tos/ field of the IP header.
ip_tos = Property "ip_tos" () () () () () () () ()

-- | Evaluate to the /tot_len/ field of the IP header.
ip_tot_len = Property "ip_tot_len" () () () () () () () ()

-- | Evaluate to the /ip_id/ field of the IP header.
ip_id = Property "ip_id" () () () () () () () ()

-- | Evaluate to the /frag/ field of the IP header.
ip_frag = Property "ip_frag" () () () () () () () ()

-- | Evaluate to the /TTL/ field of the IP header.
ip_ttl = Property "ip_ttl" () () () () () () () ()

-- | Evaluate to the /source port/ of the TCP header.
tcp_source = Property "tcp_source" () () () () () () () ()

-- | Evaluate to the /destination port/ of the TCP header.
tcp_dest = Property "tcp_dest" () () () () () () () ()

-- | Evaluate to the /length/ field of the TCP header.
tcp_hdrlen = Property "tcp_hdrlen" () () () () () () () ()

-- | Evaluate to the /source port/ of the UDP header.
udp_source = Property "udp_source" () () () () () () () ()

-- | Evaluate to the /destination port/ of the UDP header.
udp_dest = Property "udp_dest" () () () () () () () ()

-- | Evaluate to the /length/ field of the UDP header.
udp_len = Property "udp_len" () () () () () () () ()

-- | Evaluate to the /type/ field of the ICMP header.
icmp_type = Property "icmp_type" () () () () () () () ()

-- | Evaluate to the /code/ field of the ICMP header.
icmp_code = Property "icmp_code" () () () () () () () ()


-- Predefined in-kernel computations:

-- | Dispatch the packet across the sockets in Round-Robin fashion.
--
-- > ip >-> steer_rrobin
steer_rrobin = Function "steer_rrobin" () () () () () () () () :: NetFunction


-- | Dispatch the packet across the sockets using the RSS hash.
--
-- > ip >-> steer_rss
steer_rss = Function "steer_rss" () () () () () () () () :: NetFunction

-- | Dispatch the packet to a given socket with id.
--
-- > ip >-> steer_to 1
steer_to :: Int -> NetFunction
steer_to idx = Function "steer_to" idx () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that guarantees
-- physical links consistency.
--
-- > steer_link
steer_link = Function "steer_link" () () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that guarantees
-- local mac addresses consistency.
--
-- > steer_local_link "4c:60:de:86:55:46"
steer_local_link :: String -> NetFunction
steer_local_link mac_gw = Function "steer_local_link" mac_gw () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that guarantees both
-- mac addresses consistency. This alter the total amount of traffic, as the
-- packets can be steered to at most two sockets.
--
-- > double_steer_mac
double_steer_mac = Function "double_steer_mac" () () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that guarantees
-- vlan links consistency.
--
-- > steer_vlan
steer_vlan = Function "steer_vlan" () () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that guarantees
-- IP flows consistency.
--
-- > steer_p2p
steer_p2p = Function "steer_p2p" () () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that guarantees both
-- IP addresses consistency.
-- This alter the total amount of traffic (see 'double_steer_mac').
--
-- > double_steer_ip
double_steer_ip = Function "double_steer_ip" () () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that guarantees local
-- IP addresses consistency.
-- This alter the total amount of traffic (see 'double_steer_mac').
--
-- > steer_local_ip "192.168.1.0/24"
steer_local_ip :: CIDR -> NetFunction
steer_local_ip d = Function "steer_local_ip" d () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that guarantees
-- TCP/UDP flows consistency.
--
-- > steer_flow >-> log_msg "Steering a flow"
steer_flow = Function "steer_flow" () () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that guarantees
-- RTP/RTCP flows consistency.
--
-- > steer_rtp
steer_rtp = Function "steer_rtp" () () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that guarantees
-- RTP/RTCP flows consistency; SIP packets are broadcasted.
--
-- > steer_voip
steer_voip = Function "steer_voip" () () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that guarantees
-- sub networks consistency.
--
-- > steer_local_net "192.168.0.0" 16 24
steer_local_net :: IPv4 -> Int -> Int -> NetFunction
steer_local_net net p sub = Function "steer_local_net" net p sub () () () () ()

-- | Dispatch the packet across the sockets
-- with a randomized algorithm. The function uses as /hash/ the field
-- of /size/ bytes taken at /offset/ bytes from the beginning of the packet.
steer_field :: Int -- ^ offset from the beginning of the packet, in bytes
            -> Int -- ^ sizeof field in bytes (max 4)
            -> NetFunction
steer_field offset size = Function "steer_field" offset size () () () () () ()

-- | Dispatch the packet across the sockets
-- with a randomized algorithm. The function uses as /hash/ the xor operation
-- of the fields of /size/ bytes taken at /offset1/ and /offset2/ bytes from the
-- beginning of the packet.
-- This alter the total amount of traffic (see 'double_steer_mac').
double_steer_field :: Int -- ^ offset1 from the beginning of the packet, in bytes
                   -> Int -- ^ offset2 from the beginning of the packet, in bytes
                   -> Int -- ^ sizeof field in bytes (max 4)
                   -> NetFunction
double_steer_field offset1 offset2 size = Function "double_steer_field" offset1 offset2 size () () () () ()

-- | Dispatch the packet across the sockets
-- with a randomized algorithm. The function uses as /hash/ the xor operation
-- of the fields of /size/ bytes taken at /offset1/ and /offset2/ bytes from the
-- beginning of the packet.
steer_field_symmetric :: Int -- ^ offset1 from the beginning of the packet, in bytes
                      -> Int -- ^ offset2 from the beginning of the packet, in bytes
                      -> Int -- ^ sizeof field in bytes (max 4)
                      -> NetFunction
steer_field_symmetric offset1 offset2 size = Function "steer_field_symmetric" offset1 offset2 size () () () () ()

-- Predefined filters:

-- | Transform the given predicate in its counterpart monadic version.
-- Example:
--
-- > filter is_udp >-> kernel
--
-- is logically equivalent to:
--
-- > udp >-> kernel
filter :: NetPredicate -> NetFunction
filter p = Function "filter" p () () () () () () ()

-- | Evaluate to /Pass Qbuff/ if it is an IPv4 packet, /Drop/ it otherwise.
ip = Function "ip" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass Qbuff/ if it is an UDP packet, /Drop/ it otherwise.
udp = Function "udp" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass Qbuff/ if it is a TCP packet, /Drop/ it otherwise.
tcp = Function "tcp" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass Qbuff/ if it is an ICMP packet, /Drop/ it otherwise.
icmp = Function "icmp" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass Qbuff/ if it has a vlan tag, /Drop/ it otherwise.
vlan = Function "vlan" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass Qbuff/ if it is a TCP or UDP packet, /Drop/ it otherwise.
flow = Function "flow" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass Qbuff/ if it is a RTP packet, /Drop/ it otherwise.
rtp = Function "rtp" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass Qbuff/ if it is a RTCP packet, /Drop/ it otherwise.
rtcp = Function "rtcp" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass Qbuff/ if it is a SIP packet, /Drop/ it otherwise.
sip = Function "sip" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass Qbuff/ if it is a VoIP packet (RTP|RTCP|SIP), /Drop/ it otherwise.
voip = Function "voip" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass Qbuff/ if it is not a fragment, /Drop/ it otherwise.
no_frag = Function "no_frag" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass Qbuff/ if it is not a fragment or if it's the first fragment, /Drop/ it otherwise.
no_more_frag = Function "no_more_frag" () () () () () () () () :: NetFunction

-- | Forward the packet to the given device.
-- This function is lazy, in that the action is logged and performed
-- when the computation is completely evaluated.
--
-- > forward "eth1"
forward :: String -> NetFunction
forward d = Function "forward" d () () () () () () ()

-- | Forward the packet to the given device and evaluates to /Drop/. Example:
--
-- > when is_udp (bridge "eth1") >-> kernel
--
-- Conditional bridge, forward the packet to eth1 if UDP, send it to the kernel
-- otherwise.
bridge :: String -> NetFunction
bridge d = Function "bridge" d () () () () () () ()

-- | Forward the packet to the given device and, evaluates to /Pass Qbuff/ or /Drop/,
-- depending on the value returned by the predicate. Example:
--
-- > tee "eth1" is_udp >-> kernel
--
-- Logically equivalent to:
--
-- > forward "eth1" >-> udp >-> kernel
--
-- Only a little bit more efficient.
tee :: String -> NetPredicate -> NetFunction
tee d p = Function "tee" d p () () () () () ()

-- | Evaluate to /Pass Qbuff/, or forward the packet to the given device and evaluate to /Drop/,
-- depending on the value returned by the predicate. Example:
--
-- > tap "eth1" is_udp >-> kernel
--
-- Logically equivalent to:
--
-- > unless is_udp (forward "eth1" >-> drop) >-> kernel
--
-- Only a little bit more efficient.
tap :: String -> NetPredicate -> NetFunction
tap d p = Function "tap" d p () () () () () ()

-- | Forward the packet to the given device. This operation breaks the purity of the language,
-- and it is possibly slower than the lazy "forward" counterpart.
--
-- > forwardIO "eth1"
forwardIO :: String -> NetFunction
forwardIO d = Function "forwardIO" d () () () () () () ()

-- | Forward the socket buffer to the list of specified devices.
--  Unlike forward, the buffer is not forwarded to the device from which it comes from.
--
-- > link ["eth1", "eth2"]

link :: [String] -> NetFunction
link ds = Function "forward" ds () () () () () () ()


-- | Send a copy of the packet to the kernel and evaluates to /Pass/.
-- To avoid loop, this function is ignored for packets sniffed by the kernel.
kernel = Function "kernel" () () () () () () () () :: NetFunction

-- | Send a copy of the packet to the kernel and evaluates to /Drop/.
-- To avoid loop, this function is ignored for packets sniffed by the kernel.
detour = Function "detour" () () () () () () () () :: NetFunction


-- | Broadcast the packet to all the sockets that have joined the group.
broadcast = Function "broadcast" () () () () () () () () :: NetFunction

-- | Drop the packet. The computation evaluates to /Drop/.
drop= Function "drop" () () () () () () () () :: NetFunction

-- | Specify the class for the given packet. The computation evaluates to /Pass/.
classify :: Int -> NetFunction
classify n = Function "classify" n () () () () () () ()

-- | Unit operation implements left- and right-identity for Action monad.
unit = Function "unit" () () () () () () () () :: NetFunction

-- | Log a message to syslog.
--
-- > udp >-> log_msg "This is an UDP packet"
log_msg :: String -> NetFunction
log_msg msg = Function "log_msg" msg () () () () () () ()

-- | Dump the payload of packet to syslog.
--
-- > icmp >-> log_buff
log_buff = Function "log_buff" () () () () () () () () :: NetFunction

-- | Log the packet to syslog, with a syntax similar to tcpdump.
--
-- > icmp >-> log_msg "This is an ICMP packet:" >-> log_packet
log_packet = Function "log_packet" () () () () () () () () :: NetFunction

-- | Increment the i-th counter of the current group.
--
-- > inc 10
inc :: Int -> NetFunction
inc n = Function "inc" n () () () () () () ()

-- | Decrement the i-th counter of the current group.
--
-- > dec 10
dec :: Int -> NetFunction
dec n = Function "dec" n () () () () () () ()

-- | Mark the packet with the given value.
-- This function is unsafe in that it breaks the pure functional paradigm.
-- Consider using `put_state` instead.
--
-- > mark 42
mark :: Word32 -> NetFunction
mark n = Function "mark" n () () () () () () ()

-- | Set the state of the computation to the given value.
--
-- > put_state 42
put_state :: Word32 -> NetFunction
put_state n = Function "put_state" n () () () () () () ()

-- | Monadic version of 'is_l3_proto' predicate.
--
-- Predicates are used in conditional expressions, while monadic functions
-- are combined with Kleisli operator:
--
-- > l3_proto 0x842 >-> log_msg "Wake-on-LAN packet!"
l3_proto :: Word16 -> NetFunction
l3_proto p = Function "l3_proto" p () () () () () () ()

-- | Monadic version of 'is_l4_proto' predicate.
--
-- Predicates are used in conditional expressions, while monadic functions
-- are combined with Kleisli operator:
--
-- > l4_proto 89 >-> log_msg "OSFP packet!"
l4_proto :: Word8 -> NetFunction
l4_proto p = Function "l4_proto" p () () () () () () ()

-- | Monadic version of 'has_port' predicate.
--
-- Predicates are used in conditional expressions, while monadic functions
-- are combined with Kleisli operator:
--
-- > port 80 >-> log_msg "http packet!"
port :: Word16 -> NetFunction
port p = Function "port" p () () () () () () ()

-- | Monadic version of 'has_src_port' predicate.
src_port :: Word16 -> NetFunction
src_port p = Function "src_port" p () () () () () () ()

-- | Monadic version of 'has_dst_port' predicate.
dst_port :: Word16 -> NetFunction
dst_port a = Function "dst_port" a () () () () () () ()

-- | Monadic version of 'has_addr' predicate.
--
-- predicates are used in conditional expressions, while monadic functions
-- are combined with kleisli operator:
--
-- > addr "192.168.0.0/24" >-> log_packet
-- > addr (CIDR ("192.168.0.0",24)) >-> log_packet
addr :: CIDR -> NetFunction

-- | Monadic version of 'has_src_addr' predicate.
src_addr :: CIDR -> NetFunction

-- | Monadic version of 'has_src_addr' predicate.
dst_addr :: CIDR -> NetFunction

addr net   = Function "addr" net () () () () () () ()
src_addr net = Function "src_addr" net () () () () () () ()
dst_addr net = Function "dst_addr" net () () () () () () ()

-- | Conditional execution of monadic NetFunctions.
--
-- The function takes a predicate and evaluates to given the NetFunction when it evalutes to /True/,
-- otherwise does nothing.
-- Example:
--
-- > when is_tcp (log_msg "This is a TCP Packet")
when :: NetPredicate -> NetFunction -> NetFunction
when p c = Function "when" p c () () () () () ()

-- | The reverse of "when"
unless :: NetPredicate -> NetFunction -> NetFunction
unless p c = Function "unless" p c () () () () () ()

-- | conditional execution of monadic netfunctions.
--
-- the function takes a predicate and evaluates to the first or the second expression, depending on the
-- value returned by the predicate. Example:
--
-- > conditional is_udp (forward "eth1") (forward "eth2")
conditional :: NetPredicate -> NetFunction -> NetFunction -> NetFunction
conditional p c1 c2 = Function "conditional" p c1 c2 () () () () ()

-- | Function that inverts a monadic NetFunction. Useful to invert filters:
--
-- > inv ip >-> log_msg "This is not an IPv4 Packet"
inv :: NetFunction -> NetFunction
inv x = Function "inv" x () () () () () () ()

-- | Function that returns the parallel of two monadic NetFunctions.
--
-- Logic 'or' for monadic filters:
--
-- > par udp icmp >-> log_msg "This is an UDP or ICMP Packet"
par :: NetFunction -> NetFunction -> NetFunction
par a b = Function "par" a b () () () () () ()

-- | Function that returns the parallel of n- monadic NetFunctions.

par3 :: NetFunction -> NetFunction -> NetFunction -> NetFunction
par3 a b c = Function "par3" a b c () () () () ()

par4 :: NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction
par4 a b c d = Function "par4" a b c d () () () ()

par5 :: NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction
par5 a b c d e = Function "par5" a b c d e () () ()

par6 :: NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction
par6 a b c d e f = Function "par6" a b c d e f () ()

par7 :: NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction
par7 a b c d e f g = Function "par7" a b c d e f g ()

par8 :: NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction
par8 = Function "par8"


-- | Predicate which evaluates to /True/ when the packet has one of the
-- vlan id specified by the list. Example:
--
-- > when (vland_id [1,13,42,43]) (log_msg "Got a packet!")
vlan_id :: [Int] -> NetPredicate
vlan_id ids = Predicate "vlan_id" ids () () () () () () ()

-- | Monadic function, counterpart of 'vlan_id' function.
vlan_id_filter :: [Int] -> NetFunction
vlan_id_filter ids = Function "vlan_id_filter" ids () () () () () () ()

-- | Predicate that evaluates to /True/ when the source or the destination address
-- of the packet matches the ones specified by the bloom list.
--
-- The first 'Int' argument specifies the size of the bloom filter. Example:
--
-- > when (bloom 1024 ["192.168.0.13", "192.168.0.42"] 32) log_packet >-> kernel
{-# NOINLINE bloom #-}
bloom ::  Int         -- ^ Hint: size of bloom filter (M)
      ->  [HostName]  -- ^ List of Host/Network address to match
      ->  Int         -- ^ Network prefix
      ->  NetPredicate

-- | Similarly to 'bloom', evaluates to /True/ when the source address
-- of the packet matches the ones specified by the bloom list.
{-# NOINLINE bloom_src #-}
bloom_src :: Int -> [HostName] -> Int -> NetPredicate

-- | Similarly to 'bloom', evaluates to /True/ when the destination address
-- of the packet matches the ones specified by the bloom list.
{-# NOINLINE bloom_dst #-}
bloom_dst :: Int -> [HostName] -> Int -> NetPredicate

-- | Monadic counterpart of 'bloom' function.
{-# NOINLINE bloom_filter #-}
bloom_filter :: Int -> [HostName] -> Int -> NetFunction

-- | Monadic counterpart of 'bloom_src' function.
{-# NOINLINE bloom_src_filter #-}
bloom_src_filter :: Int -> [HostName] -> Int -> NetFunction

-- | Monadic counterpart of 'bloom_dst' function.
{-# NOINLINE bloom_dst_filter #-}
bloom_dst_filter :: Int -> [HostName] -> Int -> NetFunction

bloom m hs p     = let ips = unsafePerformIO (mapM inet_addr hs) in Predicate "bloom" m ips p () () () () ()
bloom_src m hs p = let ips = unsafePerformIO (mapM inet_addr hs) in Predicate "bloom_src" m ips p () () () () ()
bloom_dst m hs p = let ips = unsafePerformIO (mapM inet_addr hs) in Predicate "bloom_dst" m ips p () () () () ()

bloom_filter m hs p     = let ips = unsafePerformIO (mapM inet_addr hs) in Function "bloom_filter" m ips p () () () () ()
bloom_src_filter m hs p = let ips = unsafePerformIO (mapM inet_addr hs) in Function "bloom_src_filter" m ips p () () () () ()
bloom_dst_filter m hs p = let ips = unsafePerformIO (mapM inet_addr hs) in Function "bloom_dst_filter" m ips p () () () () ()

-- bloom filter, utility functions:

bloomK = 4

-- | Bloom filter: utility function that computes the optimal /M/, given the parameter /N/ and
-- the false-positive probability /p/.
bloomCalcM :: Int -> Double -> Int
bloomCalcM n p = ceiling $ fromIntegral(-bloomK * n) / log(1 - p ** (1 / fromIntegral bloomK))

-- | Bloom filter: utility function that computes the optimal /N/, given the parameter /M/ and
-- the false-positive probability /p/.
bloomCalcN :: Int -> Double -> Int
bloomCalcN m p = ceiling $ fromIntegral (-m) * log(1 - p ** (1 / fromIntegral bloomK )) / fromIntegral bloomK

-- | Bloom filter: utility function that computes the false positive P, given /N/ and /M/ parameters.
bloomCalcP :: Int -> Int -> Double
bloomCalcP n m = (1 - (1 - 1 / fromIntegral m) ** fromIntegral (n * bloomK))^bloomK

