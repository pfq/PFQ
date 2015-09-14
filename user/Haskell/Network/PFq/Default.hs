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
{-# LANGUAGE OverloadedStrings  #-}

module Network.PFq.Default
    (
        -- * Predicates
        -- | Collection of predicates used in conditional expressions.

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

        has_state,
        has_mark,
        has_vlan,
        has_vid,
        vlan_id,

        -- * Properties

        ip_tos      ,
        ip_tot_len  ,
        ip_id       ,
        ip_frag     ,
        ip_ttl      ,
        get_mark    ,
        get_state   ,

        tcp_source  ,
        tcp_dest    ,
        tcp_hdrlen  ,

        udp_source  ,
        udp_dest    ,
        udp_len     ,

        icmp_type   ,
        icmp_code   ,

        -- * Combinators

        (.||.),
        (.&&.),
        (.^^.),
        not',
        inv ,
        par',

        -- * Comparators
        -- | Take a NetProperty, a value, and return a predicate that compares the values.

        (.<.),
        (.<=.),
        (.==.),
        (./=.),
        (.>.),
        (.>=.),
        any_bit,
        all_bit,

        -- * Conditionals

        conditional ,
        when'       ,
        unless'     ,

        -- * Filters
        -- | A collection of monadic NetFunctions.

        filter'    ,
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

        vlan_id_filter,

        no_frag    ,
        no_more_frag,

        port       ,
        src_port   ,
        dst_port   ,

        addr       ,
        src_addr   ,
        dst_addr   ,

        -- * Steering functions
        -- | Monadic functions used to dispatch packets across sockets.
        -- They evaluate to /Steer Hash Skbuff/, if the packet has a certain property, /Drop/ otherwise.

        steer_link ,
        steer_vlan ,
        steer_ip   ,
        steer_ip6  ,
        steer_flow ,
        steer_rtp  ,
        steer_net  ,
        steer_field,

        -- * Forwarders

        kernel     ,
        broadcast  ,
        drop'      ,

        forward    ,
        forwardIO  ,

        bridge     ,
        tee        ,
        tap        ,

        -- * Logging

        log_msg    ,
        log_buff   ,
        log_packet ,

        -- * Bloom Filters

        bloom       ,
        bloom_src   ,
        bloom_dst   ,

        bloom_filter,
        bloom_src_filter,
        bloom_dst_filter,

        bloomCalcN  ,
        bloomCalcM  ,
        bloomCalcP  ,

        -- * Miscellaneous

        unit       ,
        inc        ,
        dec        ,
        mark       ,
        put_state  ,

    ) where


import           Data.Int
import           Network.PFq.Lang

import           Data.Word

import           Network.Socket
import           System.IO.Unsafe

import           Foreign.C.Types
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
not' :: NetPredicate -> NetPredicate

not' p     = Combinator1 "not"  p
p1 .||. p2 = Combinator2 "or"   p1 p2
p1 .&&. p2 = Combinator2 "and"  p1 p2
p1 .^^. p2 = Combinator2 "xor"  p1 p2

infixl 7 .&&.
infixl 6 .^^.
infixl 5 .||.

-- | Return a predicate that evaluates to /True/, if the property is less than
-- the given value. Example:
--
-- >  when' (ip_ttl .<. 64) drop'

(.<.)  :: NetProperty -> Word64 -> NetPredicate
(.<=.) :: NetProperty -> Word64 -> NetPredicate
(.==.) :: NetProperty -> Word64 -> NetPredicate
(./=.) :: NetProperty -> Word64 -> NetPredicate
(.>.)  :: NetProperty -> Word64 -> NetPredicate
(.>=.) :: NetProperty -> Word64 -> NetPredicate

p .<.  x = Predicate "less" p x () () () () () ()
p .<=. x = Predicate "less_eq" p x () () () () () ()
p .==. x = Predicate "equal" p x () () () () () ()
p ./=. x = Predicate "not_equal" p x () () () () () ()
p .>.  x = Predicate "greater" p x () () () () () ()
p .>=. x = Predicate "greater_eq" p x () () () () () ()

infix 4 .<.
infix 4 .<=.
infix 4 .>.
infix 4 .>=.
infix 4 .==.
infix 4 ./=.


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


-- | Evaluate to /True/ if the SkBuff is an IPv4 packet.
is_ip = Predicate "is_ip" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff is an IPv6 packet.
is_ip6 = Predicate "is_ip6" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff is an UDP packet.
is_udp = Predicate "is_udp" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff is a TCP packet.
is_tcp = Predicate "is_tcp" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff is an ICMP packet.
is_icmp = Predicate "is_icmp" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff is an UDP packet, on top of IPv6.
is_udp6 = Predicate "is_udp6" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff is a TCP packet, on top of IPv6.
is_tcp6 = Predicate "is_tcp6" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff is an ICMP packet, on top of IPv6.
is_icmp6 = Predicate "is_icmp6" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff is an UDP or TCP packet.
is_flow = Predicate "is_flow" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff has a vlan tag.
has_vlan = Predicate "has_vlan" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff is a TCP fragment.
is_frag = Predicate "is_frag" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff is the first TCP fragment.
is_first_frag = Predicate "is_first_frag" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff is a TCP fragment, but the first.
is_more_frag = Predicate "is_more_frag" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff has the given vlan id.
--
-- > has_vid 42
has_vid :: CInt -> NetPredicate
has_vid x = Predicate "has_vid" x () () () () () () ()

-- | Evaluate to /True/ if the SkBuff has the given mark, set by 'mark' function.
--
-- > has_mark 11
has_mark :: Word32 -> NetPredicate
has_mark x = Predicate "has_mark" x () () () () () () ()

-- | Evaluate to /True/ if the state of the computation is set to the given value, possibly by 'put_state' function.
--
-- > has_state 11
has_state :: Word32 -> NetPredicate
has_state x = Predicate "has_state" x () () () () () () ()


-- | Evaluate to /True/ if the SkBuff has the given Layer3 protocol.
is_l3_proto :: Int16 -> NetPredicate
is_l3_proto x = Predicate "is_l3_proto" x () () () () () () ()

-- | Evaluate to /True/ if the SkBuff has the given Layer4 protocol.
is_l4_proto :: Int8 -> NetPredicate
is_l4_proto x = Predicate "is_l4_proto" x () () () () () () ()


has_port, has_src_port, has_dst_port :: Int16 -> NetPredicate

-- | Evaluate to /True/ if the SkBuff has the given source or destination port.
--
-- If the transport protocol is not present or has no port, the predicate evaluates to False.
--
-- > has_port 80
has_port x = Predicate "has_port" x () () () () () () ()

-- | Evaluate to /True/ if the SkBuff has the given source port.
--
-- If the transport protocol is not present or has no port, the predicate evaluates to False.
has_src_port x = Predicate "has_src_port" x () () () () () () ()

-- | Evaluate to /True/ if the SkBuff has the given destination port.
--
-- If the transport protocol is not present or has no port, the predicate evaluates to False.
has_dst_port x = Predicate "has_dst_port" x () () () () () () ()

-- | Evaluate to /True/ if the source or destination IP address matches the given network address. I.e.,
--
-- > has_addr "192.168.0.0" 24
has_addr :: IPv4 -> CInt -> NetPredicate

-- | Evaluate to /True/ if the source IP address matches the given network address.
has_src_addr :: IPv4 -> CInt -> NetPredicate

-- | Evaluate to /True/ if the destination IP address matches the given network address.
has_dst_addr :: IPv4 -> CInt -> NetPredicate

has_addr a p     = Predicate "has_addr"     a p () () () () () ()
has_src_addr a p = Predicate "has_src_addr" a p () () () () () ()
has_dst_addr a p = Predicate "has_dst_addr" a p () () () () () ()

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

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that maintains the integrity of
-- physical links.
--
-- > ip >-> steer_link
steer_link = MFunction "steer_link" () () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that maintains the integrity of
-- vlan links.
--
-- > steer_vlan
steer_vlan = MFunction "steer_vlan" () () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that maintains the integrity of
-- IP flows.
--
-- > steer_ip
steer_ip = MFunction "steer_ip" () () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that maintains the integrity of
-- IPv6 flows.
--
-- > steer_ip6 >-> log_msg "Steering an IPv6 packet"
steer_ip6 = MFunction "steer_ip6" () () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that maintains the integrity of
-- TCP/UDP flows.
--
-- > steer_flow >-> log_msg "Steering a flow"
steer_flow = MFunction "steer_flow" () () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that maintains the integrity of
-- RTP/RTCP flows.
--
-- > steer_rtp
steer_rtp = MFunction "steer_rtp" () () () () () () () () :: NetFunction

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that maintains the integrity of
-- sub networks.
--
-- > steer_net "192.168.0.0" 16 24
steer_net :: IPv4 -> CInt -> CInt -> NetFunction
steer_net net p sub = MFunction "steer_net" net p sub () () () () ()

-- | Dispatch the packet across the sockets
-- with a randomized algorithm. The function uses as /hash/ the field
-- of /size/ bits taken at /offset/ bytes from the beginning of the packet.
steer_field :: CInt -- ^ offset from the beginning of the packet, in bytes
            -> CInt -- ^ sizeof field in bits
            -> NetFunction
steer_field off size = MFunction "steer_field" off size () () () () () ()

-- Predefined filters:

-- | Transform the given predicate in its counterpart monadic version.
-- Example:
--
-- > filter' is_udp >-> kernel
--
-- is logically equivalent to:
--
-- > udp >-> kernel
filter' :: NetPredicate -> NetFunction
filter' p = MFunction "filter" p () () () () () () ()

-- | Evaluate to /Pass SkBuff/ if it is an IPv4 packet, /Drop/ it otherwise.
ip = MFunction "ip" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if it is an IPv6 packet, /Drop/ it otherwise.
ip6 = MFunction "ip6" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if it is an UDP packet, /Drop/ it otherwise.
udp = MFunction "udp" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if it is a TCP packet, /Drop/ it otherwise.
tcp = MFunction "tcp" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if it is an ICMP packet, /Drop/ it otherwise.
icmp = MFunction "icmp" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if it is an UDP packet (on top of IPv6), /Drop/ it otherwise.
udp6 = MFunction "udp6" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if it is a TCP packet (on top of IPv6), /Drop/ it otherwise.
tcp6 = MFunction "tcp6" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if it is an ICMP packet (on top of IPv6), /Drop/ it otherwise.
icmp6 = MFunction "icmp6" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if it has a vlan tag, /Drop/ it otherwise.
vlan = MFunction "vlan" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if it is a TCP or UDP packet, /Drop/ it otherwise.
flow = MFunction "flow" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if it is a RTP/RTCP packet, /Drop/ it otherwise.
rtp = MFunction "rtp" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if it is not a fragment, /Drop/ it otherwise.
no_frag = MFunction "no_frag" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if it is not a fragment or if it's the first fragment, /Drop/ it otherwise.
no_more_frag = MFunction "no_more_frag" () () () () () () () () :: NetFunction

-- | Forward the packet to the given device.
-- This function is lazy, in that the action is logged and performed
-- when the computation is completely evaluated.
--
-- > forward "eth1"
forward :: String -> NetFunction
forward d = MFunction "forward" d () () () () () () ()

-- | Forward the packet to the given device and evaluates to /Drop/. Example:
--
-- > when' is_udp (bridge "eth1") >-> kernel
--
-- Conditional bridge, forward the packet to eth1 if UDP, send it to the kernel
-- otherwise.
bridge :: String -> NetFunction
bridge d = MFunction "bridge" d () () () () () () ()

-- | Forward the packet to the given device and, evaluates to /Pass SkBuff/ or /Drop/,
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
tee d p = MFunction "tee" d p () () () () () ()

-- | Evaluate to /Pass SkBuff/, or forward the packet to the given device and evaluate to /Drop/,
-- depending on the value returned by the predicate. Example:
--
-- > tap "eth1" is_udp >-> kernel
--
-- Logically equivalent to:
--
-- > unless' is_udp (forward "eth1" >-> drop') >-> kernel
--
-- Only a little bit more efficient.
tap :: String -> NetPredicate -> NetFunction
tap d p = MFunction "tap" d p () () () () () ()

-- | Forward the packet to the given device. This operation breaks the purity of the language,
-- and it is possibly slower than the lazy "forward" counterpart.
--
-- > forwardIO "eth1"
forwardIO :: String -> NetFunction
forwardIO d = MFunction "forwardIO" d () () () () () () ()

-- | Send a copy of the packet to the kernel (the sk_buff may have been captured directly
-- by PFQ).
--
-- To avoid loop, this function is ignored for packets sniffed from the kernel.
kernel = MFunction "kernel" () () () () () () () () :: NetFunction

-- | Broadcast the packet to all the sockets that have joined the group for which this computation
-- is specified.
broadcast = MFunction "broadcast" () () () () () () () () :: NetFunction

-- | Drop the packet. The computation evaluates to /Drop/.
drop'= MFunction "drop" () () () () () () () () :: NetFunction

-- | Unit operation implements left- and right-identity for Action monad.
unit = MFunction "unit" () () () () () () () () :: NetFunction

-- | Log a message to syslog.
--
-- > udp >-> log_msg "This is an UDP packet"
log_msg :: String -> NetFunction
log_msg msg = MFunction "log_msg" msg () () () () () () ()

-- | Dump the payload of packet to syslog.
--
-- > icmp >-> log_buff
log_buff = MFunction "log_buff" () () () () () () () () :: NetFunction

-- | Log the packet to syslog, with a syntax similar to tcpdump.
--
-- > icmp >-> log_msg "This is an ICMP packet:" >-> log_packet
log_packet = MFunction "log_packet" () () () () () () () () :: NetFunction

-- | Increment the i-th counter of the current group.
--
-- > inc 10
inc :: CInt -> NetFunction
inc n = MFunction "inc" n () () () () () () ()

-- | Decrement the i-th counter of the current group.
--
-- > dec 10
dec :: CInt -> NetFunction
dec n = MFunction "dec" n () () () () () () ()

-- | Mark the packet with the given value.
-- This function is unsafe in that it breaks the pure functional paradigm.
-- Consider using `put_state` instead.
--
-- > mark 42
mark :: Word32 -> NetFunction
mark n = MFunction "mark" n () () () () () () ()

-- | Set the state of the computation to the given value.
--
-- > state 42
put_state :: Word32 -> NetFunction
put_state n = MFunction "put_state" n () () () () () () ()


-- | Monadic version of 'is_l3_proto' predicate.
--
-- Predicates are used in conditional expressions, while monadic functions
-- are combined with Kleisli operator:
--
-- > l3_proto 0x842 >-> log_msg "Wake-on-LAN packet!"
l3_proto :: Int16 -> NetFunction
l3_proto p = MFunction "l3_proto" p () () () () () () ()

-- | Monadic version of 'is_l4_proto' predicate.
--
-- Predicates are used in conditional expressions, while monadic functions
-- are combined with Kleisli operator:
--
-- > l4_proto 89 >-> log_msg "OSFP packet!"
l4_proto :: Int8 -> NetFunction
l4_proto p = MFunction "l4_proto" p () () () () () () ()

-- | Monadic version of 'has_port' predicate.
--
-- Predicates are used in conditional expressions, while monadic functions
-- are combined with Kleisli operator:
--
-- > port 80 >-> log_msg "http packet!"
port :: Int16 -> NetFunction
port p = MFunction "port" p () () () () () () ()

-- | Monadic version of 'has_src_port' predicate.
src_port :: Int16 -> NetFunction
src_port p = MFunction "src_port" p () () () () () () ()

-- | Monadic version of 'has_dst_port' predicate.
dst_port :: Int16 -> NetFunction
dst_port a = MFunction "dst_port" a () () () () () () ()

-- | Monadic version of 'has_addr' predicate.
--
-- predicates are used in conditional expressions, while monadic functions
-- are combined with kleisli operator:
--
-- > addr "192.168.0.0" 24 >-> log_packet
addr :: IPv4 -> CInt -> NetFunction

-- | Monadic version of 'has_src_addr' predicate.
src_addr :: IPv4 -> CInt -> NetFunction

-- | Monadic version of 'has_src_addr' predicate.
dst_addr :: IPv4 -> CInt -> NetFunction

addr net p = MFunction "addr" net p () () () () () ()
src_addr net p = MFunction "src_addr" net p () () () () () ()
dst_addr net p = MFunction "dst_addr" net p () () () () () ()

-- | Conditional execution of monadic NetFunctions.
--
-- The function takes a predicate and evaluates to given the NetFunction when it evalutes to /True/,
-- otherwise does nothing.
-- Example:
--
-- > when' is_tcp (log_msg "This is a TCP Packet")
when' :: NetPredicate -> NetFunction -> NetFunction
when' p c = MFunction "when" p c () () () () () ()

-- | The reverse of "when'"
unless' :: NetPredicate -> NetFunction -> NetFunction
unless' p c = MFunction "unless" p c () () () () () ()

-- | conditional execution of monadic netfunctions.
--
-- the function takes a predicate and evaluates to the first or the second expression, depending on the
-- value returned by the predicate. Example:
--
-- > conditional is_udp (forward "eth1") (forward "eth2")
conditional :: NetPredicate -> NetFunction -> NetFunction -> NetFunction
conditional p c1 c2 = MFunction "conditional" p c1 c2 () () () () ()

-- | Function that inverts a monadic NetFunction. Useful to invert filters:
--
-- > inv ip >-> log_msg "This is not an IPv4 Packet"
inv :: NetFunction -> NetFunction
inv x = MFunction "inv" x () () () () () () ()

-- | Function that returns the parallel of two monadic NetFunctions.
--
-- Logic 'or' for monadic filters:
--
-- > par' udp icmp >-> log_msg "This is an UDP or ICMP Packet"
par' :: NetFunction -> NetFunction -> NetFunction
par' a b = MFunction "par" a b () () () () () ()

-- | Predicate which evaluates to /True/ when the packet has one of the
-- vlan id specified by the list. Example:
--
-- > when' (vland_id [1,13,42,43]) (msg_log "Got a packet!")
vlan_id :: [CInt] -> NetPredicate
vlan_id ids = Predicate "vlan_id" ids () () () () () () ()

-- | Monadic function, counterpart of 'vlan_id' function.
vlan_id_filter :: [CInt] -> NetFunction
vlan_id_filter ids = MFunction "vlan_id_filter" ids () () () () () () ()

-- | Predicate that evaluates to /True/ when the source or the destination address
-- of the packet matches the ones specified by the bloom list.
--
-- The first 'CInt' argument specifies the size of the bloom filter. Example:
--
-- > when' (bloom 1024 ["192.168.0.13", "192.168.0.42"] 32) log_packet >-> kernel
{-# NOINLINE bloom #-}
bloom ::  CInt        -- ^ Hint: size of bloom filter (M)
      ->  [HostName]  -- ^ List of Host/Network address to match
      ->  CInt        -- ^ Network prefix
      ->  NetPredicate

-- | Similarly to 'bloom', evaluates to /True/ when the source address
-- of the packet matches the ones specified by the bloom list.
{-# NOINLINE bloom_src #-}
bloom_src :: CInt -> [HostName] -> CInt -> NetPredicate

-- | Similarly to 'bloom', evaluates to /True/ when the destination address
-- of the packet matches the ones specified by the bloom list.
{-# NOINLINE bloom_dst #-}
bloom_dst :: CInt -> [HostName] -> CInt -> NetPredicate

-- | Monadic counterpart of 'bloom' function.
{-# NOINLINE bloom_filter #-}
bloom_filter :: CInt -> [HostName] -> CInt -> NetFunction

-- | Monadic counterpart of 'bloom_src' function.
{-# NOINLINE bloom_src_filter #-}
bloom_src_filter :: CInt -> [HostName] -> CInt -> NetFunction

-- | Monadic counterpart of 'bloom_dst' function.
{-# NOINLINE bloom_dst_filter #-}
bloom_dst_filter :: CInt -> [HostName] -> CInt -> NetFunction

bloom m hs p     = let ips = unsafePerformIO (mapM inet_addr hs) in Predicate "bloom" m ips p () () () () ()
bloom_src m hs p = let ips = unsafePerformIO (mapM inet_addr hs) in Predicate "bloom_src" m ips p () () () () ()
bloom_dst m hs p = let ips = unsafePerformIO (mapM inet_addr hs) in Predicate "bloom_dst" m ips p () () () () ()

bloom_filter m hs p     = let ips = unsafePerformIO (mapM inet_addr hs) in MFunction "bloom_filter" m ips p () () () () ()
bloom_src_filter m hs p = let ips = unsafePerformIO (mapM inet_addr hs) in MFunction "bloom_src_filter" m ips p () () () () ()
bloom_dst_filter m hs p = let ips = unsafePerformIO (mapM inet_addr hs) in MFunction "bloom_dst_filter" m ips p () () () () ()

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

