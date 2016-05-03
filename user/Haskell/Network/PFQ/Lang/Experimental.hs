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

module Network.PFQ.Lang.Experimental
    (
        -- * Experimental Functions
        -- | This set of experimental functions may be subject to changes in future releases

      dummy
    , dummy_ip
    , dummy_vector
    , dummy_string
    , dummy_strings
    , dummy_cidr
    , dummy_cidrs

    , crc16

    , steer_gtp_usr

    , gtp
    , gtp_cp
    , gtp_up
    , is_gtp
    , is_gtp_cp
    , is_gtp_up
    , shift
    , src
    , dst
    , trace

    , kernel_if
    , detour_if

    , is_broadcast
    , is_multicast
    , is_ip_broadcast
    , is_ip_multicast
    , is_ip_host
    , is_incoming_host

    , mac_broadcast
    , mac_multicast
    , incoming_host
    , ip_broadcast
    , ip_multicast
    , ip_host

    ) where


import Network.PFQ.Lang

-- Experimental/Testing in-kernel computations

dummy :: Int -> NetFunction
dummy n = Function "dummy" n () () () () () () ()

dummy_ip  :: IPv4 -> NetFunction
dummy_ip xs  = Function "dummy_ip" xs () () () () () () ()

dummy_vector  :: [Int] -> NetFunction
dummy_vector xs  = Function "dummy_vector" xs () () () () () () ()

dummy_string :: String -> NetFunction
dummy_string xs  = Function "dummy_string" xs () () () () () () ()

dummy_strings :: [String] -> NetFunction
dummy_strings xs  = Function "dummy_strings" xs () () () () () () ()

dummy_cidr  :: CIDR -> NetFunction
dummy_cidr x  = Function "dummy_cidr" x () () () () () () ()

dummy_cidrs  :: [CIDR] -> NetFunction
dummy_cidrs xs  = Function "dummy_cidrs" xs () () () () () () ()


crc16 :: NetFunction
crc16 = Function "crc16" () () () () () () () ()

-- | Dispatch the packet across the sockets
-- with a randomized algorithm that maintains the integrity of
-- per-user flows on top of GTP tunnel protocol (Control-Plane packets
-- are broadcasted to all sockets).
--
-- > (steer_gtp_usr "192.168.0.0" 16)

steer_gtp_usr :: IPv4 -> Int -> NetFunction
steer_gtp_usr net prefix = Function "steer_gtp_usr" net prefix () () () () () () :: NetFunction


-- | Evaluate to /Pass SkBuff/ in case of GTP packet, /Drop/ it otherwise.
gtp    = Function "gtp" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ in case of GTP Control-Plane packet, /Drop/ it otherwise.
gtp_cp  = Function "gtp_cp" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ in case of GTP User-Plane packet, /Drop/ it otherwise.
gtp_up  = Function "gtp_up" () () () () () () () () :: NetFunction

-- | Evaluate to /True/ if the SkBuff is a GTP packet.
is_gtp = Predicate "is_gtp" () () () () () () () () :: NetPredicate

-- | Evaluate to /True/ if the SkBuff is a GTP Control-Plane packet.
is_gtp_cp = Predicate "is_gtp_cp" () () () () () () () () :: NetPredicate

-- | Evaluate to /True/ if the SkBuff is a GTP User-Plane packet.
is_gtp_up = Predicate "is_gtp_up" () () () () () () () () :: NetPredicate


-- The function shift an action...
--
-- > shift steer_flow
shift :: NetFunction -> NetFunction
shift f = Function "shift" f () () () () () () ()

-- This function creates a 'source' context...
--
-- > src $ ...
src :: NetFunction -> NetFunction
src f = Function "src" f () () () () () () ()


-- The function creates a 'destination' context...
--
-- > dst  $ ...
dst :: NetFunction -> NetFunction
dst f = Function "dst" f () () () () () () ()


-- | Log monadic/state information to syslog.
--
-- > udp >-> log_msg "This is an UDP packet"
--
trace :: NetFunction
trace = Function "trace" () () () () () () () ()


-- | Conditional forwarder to kernel. Evaluate to /Pass SkBuff/.
--
-- > kernel_if is_tcp

kernel_if :: NetPredicate -> NetFunction
kernel_if p = Function "kernel_if" p () () () () () () ()

-- | Conditional forwarder to kernel. Evaluate to /Drop/ if
-- predicate evaluates to True, /Pass/ otherwise.
--
-- > detour_if is_tcp

detour_if :: NetPredicate -> NetFunction
detour_if p = Function "detour_if" p () () () () () () ()

-- | Evaluate to /True/ if the SkBuff is broadcast frame.
is_broadcast = Predicate "is_broadcast" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff is multicast frame.
is_multicast = Predicate "is_multicast" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff is broadcast IP packet.
is_ip_broadcast = Predicate "is_ip_broadcast" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff is multicast IP packet.
is_ip_multicast = Predicate "is_ip_multicast" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff IP address matches that of the incoming interface,
-- /False/ otherwise.
is_ip_host = Predicate "is_ip_host" () () () () () () () ()

-- | Evaluate to /True/ if the SkBuff IP address matches that of the incoming interface,
--   is a broadcast or a multicast frame.
is_incoming_host = Predicate "is_incoming_host" () () () () () () () ()


-- | Evaluate to /Pass SkBuff/ if it is a broadcast frame, /Drop/ it otherwise.
mac_broadcast = Function "mac_broadcast" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if it is a multicast frame, /Drop/ it otherwise.
mac_multicast = Function "mac_multicast" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if it is a broadcast IP packet, /Drop/ it otherwise.
ip_broadcast = Function "ip_broadcast" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if it is a multicast IP packet, /Drop/ it otherwise.
ip_multicast = Function "ip_multicast" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if the IP address matches that of the incoming interface, /Drop/ it otherwise.
ip_host = Function "ip_host" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ if the IP address matches that of the incoming interface,
-- is a broadcast or a multicast frame, /Drop/ it otherwise.
incoming_host = Function "incoming_host" () () () () () () () () :: NetFunction


