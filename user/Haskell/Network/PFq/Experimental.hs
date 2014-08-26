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

        bridge     ,
        tee        ,
        tap        ,
        class'     ,
        deliver    ,
        forward    ,
        filter'    ,
        dummy      ,
        hdummy     ,
        vdummy     ,
        crc16      ,

        bloom       ,
        bloom_src   ,
        bloom_dst   ,

        bloom_filter,
        bloom_src_filter,
        bloom_dst_filter,

        bloomCalcN  ,
        bloomCalcM  ,
        bloomCalcP  ,

        vlan_id       ,
        vlan_id_filter,

        steer_field   ,
    ) where


import Network.PFq.Lang
import Foreign.C.Types

import System.IO.Unsafe
import Network.Socket

-- Experimental in-kernel computations

filter'     = MFunctionP  "filter"        :: NetPredicate -> NetFunction

class'      = MFunction1 "class"         :: CInt    -> NetFunction
deliver     = MFunction1 "deliver"       :: CInt    -> NetFunction
forward     = MFunction1 "forward"       :: String  -> NetFunction
bridge      = MFunction1 "bridge"        :: String  -> NetFunction
tee         = MFunction1P "tee"          :: String  -> NetPredicate -> NetFunction
tap         = MFunction1P "tap"          :: String  -> NetPredicate -> NetFunction

dummy       = MFunction1 "dummy"         :: CInt -> NetFunction
hdummy      = MFunctionP  "hdummy"       :: NetPredicate -> NetFunction

vdummy      :: [CInt] -> NetFunction
vdummy  xs  = MFunction1 "vdummy" (Vector xs)

crc16       = MFunction "crc16" :: NetFunction

bloom         :: CInt -> [HostName] -> NetPredicate
bloom_src     :: CInt -> [HostName] -> NetPredicate
bloom_dst     :: CInt -> [HostName] -> NetPredicate

bloom_filter      :: CInt -> [HostName] -> NetFunction
bloom_src_filter  :: CInt -> [HostName] -> NetFunction
bloom_dst_filter  :: CInt -> [HostName] -> NetFunction

bloom            m hs = let ips = unsafePerformIO (mapM inet_addr hs) in Predicate2 "bloom"     m (Vector ips)
bloom_src        m hs = let ips = unsafePerformIO (mapM inet_addr hs) in Predicate2 "bloom_src" m (Vector ips)
bloom_dst        m hs = let ips = unsafePerformIO (mapM inet_addr hs) in Predicate2 "bloom_dst" m (Vector ips)

bloom_filter     m hs = let ips = unsafePerformIO (mapM inet_addr hs) in MFunction2 "bloom_filter"     m (Vector ips)
bloom_src_filter m hs = let ips = unsafePerformIO (mapM inet_addr hs) in MFunction2 "bloom_src_filter" m (Vector ips)
bloom_dst_filter m hs = let ips = unsafePerformIO (mapM inet_addr hs) in MFunction2 "bloom_dst_filter" m (Vector ips)


-- bloom filter: utility functions:

bloomK = 4

bloomCalcM  :: Int -> Double -> Int
bloomCalcM  n p =  ceiling $ fromIntegral(-bloomK * n) / log(1 - p ** (1 / fromIntegral bloomK))

bloomCalcN  :: Int -> Double -> Int
bloomCalcN  m p =  ceiling $ fromIntegral (-m) * log(1 - p ** (1 / fromIntegral bloomK )) / fromIntegral bloomK

bloomCalcP  :: Int -> Int -> Double
bloomCalcP  n m =  (1 - (1 - 1 / fromIntegral m) ** fromIntegral (n * bloomK))^bloomK

-- vlan

vlan_id  :: [CInt] -> NetPredicate
vlan_id ids = Predicate1 "vlan_id" (Vector ids)

vlan_id_filter  :: [CInt] -> NetFunction
vlan_id_filter ids = MFunction1 "vlan_id_filter" (Vector ids)


steer_field :: CInt -> CInt -> NetFunction
steer_field off size = MFunction2 "steer_field" off size



