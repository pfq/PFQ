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

module Network.PFq.Experimental
    (
        -- * Experimental Functions
        -- | This set of experimental functions may be subject to changes in future releases

        dummy        ,
        dummy_ip     ,
        dummy_vector ,
        dummy_string ,
        dummy_strings,

        crc16      ,

        class'     ,
        deliver    ,

        par3,
        par4,
        par5,
        par6,
        par7,
        par8,

        steer_gtp_usr,
        gtp,
        gtp_cp,
        gtp_up,
        is_gtp,
        is_gtp_cp,
        is_gtp_up,

        link

    ) where


import Network.PFq.Lang
import Foreign.C.Types

-- Experimental in-kernel computations

-- | Specify the class mask for the given packet.

class'  :: CInt -> NetFunction
class'  n = MFunction "class" n () () () () () () ()

deliver :: CInt -> NetFunction
deliver n = MFunction "deliver" n () () () () () () ()

dummy :: CInt -> NetFunction
dummy n = MFunction "dummy" n () () () () () () ()

dummy_ip  :: IPv4 -> NetFunction
dummy_ip xs  = MFunction "dummy_ip" xs () () () () () () ()

dummy_vector  :: [CInt] -> NetFunction
dummy_vector xs  = MFunction "dummy_vector" xs () () () () () () ()

dummy_string :: String -> NetFunction
dummy_string xs  = MFunction "dummy_string" xs () () () () () () ()

dummy_strings :: [String] -> NetFunction
dummy_strings xs  = MFunction "dummy_strings" xs () () () () () () ()

crc16 :: NetFunction
crc16 = MFunction "crc16" () () () () () () () ()


-- | Forward the socket buffer to the list of specified devices.
--  Unlike forward, the buffer is not forwarded to the device from which it comes from.
--
-- > link ["eth1", "eth2"]

link :: [String] -> NetFunction
link ds = MFunction "forward" ds () () () () () () ()


-- | Function that returns the parallel of 3 monadic NetFunctions.

par3 :: NetFunction -> NetFunction -> NetFunction -> NetFunction
par3 a b c = MFunction "par3" a b c () () () () ()

par4 :: NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction
par4 a b c d = MFunction "par4" a b c d () () () ()

par5 :: NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction
par5 a b c d e = MFunction "par5" a b c d e () () ()

par6 :: NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction
par6 a b c d e f = MFunction "par6" a b c d e f () ()

par7 :: NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction
par7 a b c d e f g = MFunction "par7" a b c d e f g ()

par8 :: NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction -> NetFunction
par8 a b c d e f g h = MFunction "par8" a b c d e f g h


-- | Dispatch the packet across the sockets
-- with a randomized algorithm that maintains the integrity of
-- per-user flows on top of GTP tunnel protocol (Control-Plane packets
-- are broadcasted to all sockets).
--
-- > (steer_gtp_usr "192.168.0.0" 16)

steer_gtp_usr :: IPv4 -> CInt -> NetFunction
steer_gtp_usr net prefix = MFunction "steer_gtp_usr" net prefix () () () () () () :: NetFunction


-- | Evaluate to /Pass SkBuff/ in case of GTP packet, /Drop/ it otherwise.
gtp    = MFunction "gtp" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ in case of GTP Control-Plane packet, /Drop/ it otherwise.
gtp_cp  = MFunction "gtp_cp" () () () () () () () () :: NetFunction

-- | Evaluate to /Pass SkBuff/ in case of GTP User-Plane packet, /Drop/ it otherwise.
gtp_up  = MFunction "gtp_up" () () () () () () () () :: NetFunction

-- | Evaluate to /True/ if the SkBuff is a GTP packet.
is_gtp = Predicate "is_gtp" () () () () () () () () :: NetPredicate

-- | Evaluate to /True/ if the SkBuff is a GTP Control-Plane packet.
is_gtp_cp = Predicate "is_gtp_cp" () () () () () () () () :: NetPredicate

-- | Evaluate to /True/ if the SkBuff is a GTP User-Plane packet.
is_gtp_up = Predicate "is_gtp_up" () () () () () () () () :: NetPredicate


