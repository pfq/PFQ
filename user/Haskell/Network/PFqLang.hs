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
        StorableContext(..),
        Computation(),
        QFun,
        QMetaFun,

        (>->),
        eval,
        qfun,
        qfunWith,

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
        dummy      ,
        counter    ,
        class'
    ) where

import Control.Monad.Identity
import Foreign.Storable

-- Functional computation

data StorableContext = forall a. (Show a, Storable a) => StorableContext a

instance Show StorableContext where
        show (StorableContext c) = show c

-- Computation (phantom type)

data Computation f where
        Fun  :: String -> Computation f
        FunWith :: forall a f. (Show a, Storable a) => String -> a -> Computation f
        Comp :: Computation f -> Computation f -> Computation f

instance Show (Computation f) where
    show (Fun name)  = name
    show (FunWith name ctx) = name ++ " (" ++ show ctx ++ ")"
    show (Comp c1 c2) = show c1 ++ " >-> " ++ show c2

-- QFun: signature of the in-kernel monadic functions.

type QFun       = forall ctx. (Show ctx, Storable ctx) => ctx -> SkBuff -> Action SkBuff
type QMetaFun   = (String, Maybe StorableContext)

type Action     = Identity
newtype SkBuff  = SkBuff ()

-- operator: >->

(>->) :: Computation QFun -> Computation QFun -> Computation QFun
(Fun  n)     >-> (Fun n')     = Comp (Fun n)      (Fun n')
(FunWith n x)   >-> (Fun n')     = Comp (FunWith n x)   (Fun n')
(FunWith n x)   >-> (FunWith n' x') = Comp (FunWith n x)   (FunWith n' x')
(Fun  n)     >-> (FunWith n' x') = Comp (Fun n)      (FunWith n' x')
(Comp c1 c2) >-> (Fun n)      = Comp (Comp c1 c2) (Fun n)
(Comp c1 c2) >-> (FunWith n c)   = Comp (Comp c1 c2) (FunWith n c)

(Fun  n)     >-> (Comp c1 c2) = Comp (Fun n) (Comp c1 c2)
(FunWith n x)   >-> (Comp c1 c2) = Comp (FunWith n x) (Comp c1 c2)
(Comp c1 c2) >-> (Comp c3 c4) = Comp (Comp c1 c2) (Comp c3 c4)


-- eval: convert a computation of QFun to a list of QMetaFun.

eval :: Computation QFun -> [QMetaFun]
eval (Fun n)      = [(n, Nothing)]
eval (FunWith n x)   = [(n, Just (StorableContext x))]
eval (Comp c1 c2) = eval c1 ++ eval c2


-- qfun, qfunWith: Computation QFun constructors

qfun :: String -> Computation QFun
qfun = Fun

qfunWith :: (Show a, Storable a) => String -> a -> Computation QFun
qfunWith name = FunWith name

-- Predefined in-kernel computations:
--

steer_mac   = qfun "steer-mac"
steer_vlan  = qfun "steer-vlan-id"
steer_ip    = qfun "steer-ip"
steer_ipv6  = qfun "steer-ipv6"
steer_flow  = qfun "steer-flow"
steer_rtp   = qfun "steer-rtp"

ip          = qfun "ip"
ipv6        = qfun "ipv6"
udp         = qfun "udp"
tcp         = qfun "tcp"
vlan        = qfun "vlan"
icmp        = qfun "icmp"
flow        = qfun "flow"
rtp         = qfun "rtp"

legacy      = qfun "legacy"
clone       = qfun "clone"
broadcast   = qfun "broadcast"
sink        = qfun "sink"
drop'       = qfun "drop"

ident       = qfun  "id"
dummy       = qfunWith "dummy"    :: Int -> Computation QFun
counter     = qfunWith "counter"  :: Int -> Computation QFun
class'      = qfunWith "class"    :: Int -> Computation QFun

