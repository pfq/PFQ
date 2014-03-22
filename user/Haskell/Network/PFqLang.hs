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
        StorableContext(..),
        QFun,
        QMetaFun,

        (>->),
        eval,

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

data StorableContext = forall a. (Show a, Storable a) => StorableContext a

instance Show StorableContext where
        show (StorableContext c) = show c


-- Computation is a phantom type: f is signature of the in-kernel monadic functions.

type QFun       = forall ctx. (Show ctx, Storable ctx) => ctx -> SkBuff -> Action SkBuff
type QMetaFun   = (String, Maybe StorableContext)

type Action     = Identity
newtype SkBuff  = SkBuff ()


-- Computation:

data Computation f where
        Fun  :: String -> Computation f
        FunC :: forall a f. (Show a, Storable a) => String -> a -> Computation f
        Comp :: Computation f -> Computation f -> Computation f

instance Show (Computation f) where
    show (Fun name)  = name
    show (FunC name ctx) = name ++ " (" ++ show ctx ++ ")"
    show (Comp c1 c2) = show c1 ++ " >-> " ++ show c2

-- operator: >->

(>->) :: Computation QFun -> Computation QFun -> Computation QFun
(Fun  n)     >-> (Fun n')     = Comp (Fun n)      (Fun n')
(FunC n x)   >-> (Fun n')     = Comp (FunC n x)   (Fun n')
(FunC n x)   >-> (FunC n' x') = Comp (FunC n x)   (FunC n' x')
(Fun  n)     >-> (FunC n' x') = Comp (Fun n)      (FunC n' x')
(Comp c1 c2) >-> (Fun n)      = Comp (Comp c1 c2) (Fun n)
(Comp c1 c2) >-> (FunC n c)   = Comp (Comp c1 c2) (FunC n c)

(Fun  n)     >-> (Comp c1 c2) = Comp (Fun n) (Comp c1 c2)
(FunC n x)   >-> (Comp c1 c2) = Comp (FunC n x) (Comp c1 c2)
(Comp c1 c2) >-> (Comp c3 c4) = Comp (Comp c1 c2) (Comp c3 c4)

-- eval: convert a computation in a list of QMetaFun data.

eval :: Computation QFun -> [QMetaFun]
eval (Fun n)      = [(n, Nothing)]
eval (FunC n x)   = [(n, Just (StorableContext x))]
eval (Comp c1 c2) = eval c1 ++ eval c2


-- Predefined in-kernel computations:
--

steer_mac   = Fun "steer-mac"       :: Computation QFun
steer_vlan  = Fun "steer-vlan-id"   :: Computation QFun
steer_ip    = Fun "steer-ip"        :: Computation QFun
steer_ipv6  = Fun "steer-ipv6"      :: Computation QFun
steer_flow  = Fun "steer-flow"      :: Computation QFun
steer_rtp   = Fun "steer-rtp"       :: Computation QFun

ip          = Fun "ip"              :: Computation QFun
ipv6        = Fun "ipv6"            :: Computation QFun
udp         = Fun "udp"             :: Computation QFun
tcp         = Fun "tcp"             :: Computation QFun
vlan        = Fun "vlan"            :: Computation QFun
icmp        = Fun "icmp"            :: Computation QFun
flow        = Fun "flow"            :: Computation QFun
rtp         = Fun "rtp"             :: Computation QFun

legacy      = Fun "legacy"          :: Computation QFun
clone       = Fun "clone"           :: Computation QFun
broadcast   = Fun "broadcast"       :: Computation QFun
sink        = Fun "sink"            :: Computation QFun
drop'       = Fun "drop"            :: Computation QFun

ident       = Fun  "id"             :: Computation QFun
dummy       = FunC "dummy"          :: Int -> Computation QFun

