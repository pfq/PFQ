--
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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Network.PFqLang
    (
        Computation(..),
        FunType,
        (>->),
        --
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
        clone      ,
        broadcast  ,
        sink       ,
        drop       ,

        ident      ,
        dummy
    ) where

import Control.Monad.Identity
import Foreign.Storable

-- Functional computation
--

-- Computation is a phantom type: f is signature of the in-kernel monadic functions.

type FunType    = forall a. (Storable a) => a -> SkBuff -> Action SkBuff
type Action     = Identity
newtype SkBuff  = SkBuff ()

-- Computation:

data Computation f where
        Fun  :: String -> Computation f
        FunC :: forall a f. (Storable a) => String -> a -> Computation f
        Comp :: Computation f -> Computation f -> Computation f

instance Show (Computation f) where
    show (Fun name)  = name
    show (FunC name ctx) = name ++ " (size:" ++ show (sizeOf ctx) ++ ")"
    show (Comp c1 c2) = show c1 ++ " >-> " ++ show c2

-- operator: >->

(>->) :: Computation FunType -> Computation FunType -> Computation FunType
(Fun  n)     >-> (Fun n')     = Comp (Fun n)      (Fun n')
(FunC n x)   >-> (Fun n')     = Comp (FunC n x)   (Fun n')
(FunC n x)   >-> (FunC n' x') = Comp (FunC n x)   (FunC n' x')
(Fun  n)     >-> (FunC n' x') = Comp (Fun n)      (FunC n' x')
(Comp c1 c2) >-> (Fun n)      = Comp (Comp c1 c2) (Fun n)
(Comp c1 c2) >-> (FunC n c)   = Comp (Comp c1 c2) (FunC n c)

(Fun  n)     >-> (Comp c1 c2) = Comp (Fun n) (Comp c1 c2)
(FunC n x)   >-> (Comp c1 c2) = Comp (FunC n x) (Comp c1 c2)
(Comp c1 c2) >-> (Comp c3 c4) = Comp (Comp c1 c2) (Comp c3 c4)

-- Predefined in-kernel computations
--

steer_mac   = Fun "steer-mac"       :: Computation FunType
steer_vlan  = Fun "steer-vlan-id"   :: Computation FunType
steer_ip    = Fun "steer-ip"        :: Computation FunType
steer_ipv6  = Fun "steer-ipv6"      :: Computation FunType
steer_flow  = Fun "steer-flow"      :: Computation FunType
steer_rtp   = Fun "steer-rtp"       :: Computation FunType

ip          = Fun "ip"              :: Computation FunType
ipv6        = Fun "ipv6"            :: Computation FunType
udp         = Fun "udp"             :: Computation FunType
tcp         = Fun "tcp"             :: Computation FunType
vlan        = Fun "vlan"            :: Computation FunType
icmp        = Fun "icmp"            :: Computation FunType
flow        = Fun "flow"            :: Computation FunType
rtp         = Fun "rtp"             :: Computation FunType

legacy      = Fun "legacy"          :: Computation FunType
clone       = Fun "clone"           :: Computation FunType
broadcast   = Fun "broadcast"       :: Computation FunType
sink        = Fun "sink"            :: Computation FunType
drop'       = Fun "drop"            :: Computation FunType

ident       = Fun  "id"             :: Computation FunType
dummy       = FunC "dummy"          :: Int -> Computation FunType

