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
        Predicate(),
        Computation(),
        Serializable,

        Fun,
        FunDescr,
        serialize,

        (>->),

        qfun,
        qfun1,
        hfun,
        hfun1,
        hfun2,

        -- combinators

        (.|.),
        (.&.),
        (.^.),

        -- predicates

        is_ip,
        is_udp,
        is_tcp,

        -- monadic functions

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
        broadcast  ,
        sink       ,
        drop'      ,

        id'        ,
        dummy      ,
        counter    ,
        class'     ,

        -- high order functions

        hdummy,
        conditional,
        when',
        unless',

    ) where


import Control.Monad.Identity
import Foreign.Storable

-- StorableContext

data StorableContext = forall a. (Show a, Storable a) => StorableContext a

instance Show StorableContext where
        show (StorableContext c) = show c


data Argument = Empty | Predicate Int | Arg StorableContext
                    deriving Show

-- Functional descriptor

data FunType = MonadicFun | HighOrderFun | PredicateFun | CombinatorFun
                deriving (Show, Enum)

data FunDescr = FunDescr
                {
                    functionalType  :: FunType,
                    functionalSymb  :: String,
                    functionalArg   :: Argument,
                    functionalLeft  :: Int,
                    functionalRight :: Int
                }
                deriving (Show)


relinkFunDescr :: Int -> Int -> FunDescr -> FunDescr
relinkFunDescr n1 n2 (FunDescr t name arg l r) =
        FunDescr t name arg (update n1 n2 l) (update n1 n2 r)
            where update n1 n2 x = if x == n1 then n2 else x

-- Serializable class

class Serializable a where
    serialize :: Int -> a -> ([FunDescr], Int)


-- Predicates and combinators

newtype Combinator = Combinator String

instance Show Combinator where
        show (Combinator "or")  = "|"
        show (Combinator "and") = "&"
        show (Combinator "xor") = "^"
        show (Combinator _)     = undefined

instance Serializable Combinator where
        serialize n (Combinator name) = ([FunDescr { functionalType  = CombinatorFun,
                                                     functionalSymb  = name,
                                                     functionalArg   = Empty,
                                                     functionalLeft  = -1,
                                                     functionalRight = -1 }], n+1)

data Predicate where
        Pred  :: String -> Predicate
        Pred1 :: forall a. (Show a, Storable a) => String -> a -> Predicate
        Comb  :: Combinator -> Predicate -> Predicate -> Predicate


instance Show Predicate where
        show (Pred name)        = name
        show (Pred1 name a1)    = "(" ++ name ++ " " ++ show a1 ++ ")"
        show (Comb comb p1 p2)  = "(" ++ show p1 ++ " " ++ show comb ++ " " ++ show p2 ++ ")"

instance Serializable Predicate where
        serialize n (Pred name)   = ([FunDescr { functionalType  = PredicateFun,
                                                 functionalSymb  = name,
                                                 functionalArg   = Empty,
                                                 functionalLeft  = -1,
                                                 functionalRight = -1 }], n+1)

        serialize n (Pred1 name x) = ([FunDescr { functionalType  = PredicateFun,
                                                  functionalSymb  = name,
                                                  functionalArg   = Arg $ StorableContext x,
                                                  functionalLeft  = -1,
                                                  functionalRight = -1 }], n+1)

        serialize n (Comb comb p1 p2) = let ([g'], n')   = serialize n comb
                                            (f'',  n'')  = serialize n' p1
                                            (f''', n''') = serialize n'' p2
                                        in ( [g'{ functionalLeft = n', functionalRight = n''}] ++ f'' ++ f''', n''')

-- Computation

data Computation f where
        Fun  :: String -> Computation f
        Fun1 :: forall a f. (Show a, Storable a) => String -> a -> Computation f
        HFun  :: String -> Predicate -> Computation f
        HFun1 :: String -> Predicate -> Computation f -> Computation f
        HFun2 :: String -> Predicate -> Computation f -> Computation f -> Computation f
        Comp :: Computation f -> Computation f -> Computation f


instance Show (Computation f) where
        show (Fun name) = name
        show (Fun1 name a) = "(" ++ name ++ " " ++ show a ++ ")"
        show (HFun  name pred) = "(" ++ name ++ " " ++ show pred ++ ")"
        show (HFun1 name pred a) = "(" ++ name ++ " " ++ show pred ++ " (" ++ show a ++ "))"
        show (HFun2 name pred a1 a2)  = "(" ++ name ++ " " ++ show pred ++ " (" ++ show a1 ++ ") (" ++ show a2 ++ "))"
        show (Comp c1 c2) = show c1 ++ " >-> " ++ show c2



instance Serializable (Computation f) where
        serialize n (Fun name) = ([FunDescr { functionalType  = MonadicFun,
                                              functionalSymb  = name,
                                              functionalArg   = Empty,
                                              functionalLeft  = n+1,
                                              functionalRight = n+1 }], n+1)

        serialize n (Fun1 name x) = ([FunDescr { functionalType  = MonadicFun,
                                                 functionalSymb  = name,
                                                 functionalArg   = Arg $ StorableContext x,
                                                 functionalLeft  = n+1,
                                                 functionalRight = n+1 }], n+1)

        serialize n (HFun name p) = let (s', n') = ([FunDescr { functionalType  = HighOrderFun,
                                                                functionalSymb  = name,
                                                                functionalArg   = Predicate n',
                                                                functionalLeft  = n'',
                                                                functionalRight = n'' }], n+1)
                                        (p', n'') = serialize n' p
                                    in (s' ++ p', n'')

        serialize n (HFun1 name p c) = let (f', n') = (FunDescr { functionalType  = HighOrderFun,
                                                                  functionalSymb  = name,
                                                                  functionalArg   = Predicate n',
                                                                  functionalLeft  = -1,
                                                                  functionalRight = -1 }, n+1)
                                           (p', n'') = serialize n' p
                                           (c', n''') = serialize n'' c
                                        in ( [f'{ functionalLeft = n''', functionalRight = n''}] ++ p' ++ c', n''')

        serialize n (HFun2 name p c1 c2) = let (f', n') = (FunDescr { functionalType  = HighOrderFun,
                                                                      functionalSymb  = name,
                                                                      functionalArg   = Predicate n',
                                                                      functionalLeft  = -1,
                                                                      functionalRight = -1 }, n+1)
                                               (p',  n'') = serialize n' p
                                               (c1', n''') = serialize n'' c1
                                               (c2', n'''') = serialize n''' c2
                                            in ( [f'{ functionalLeft = n''', functionalRight = n''}] ++ p' ++ (map (relinkFunDescr n''' n'''') c1') ++ c2', n'''')

        serialize n (Comp c1 c2) = let (s1, n') = serialize n c1
                                       (s2, n'') = serialize n' c2
                                   in (s1 ++ s2, n'')

-- Fun: signature of the in-kernel monadic functions.

type Action     = Identity
newtype SkBuff  = SkBuff ()

type Fun = forall a. (Show a, Storable a) => a -> SkBuff -> Action SkBuff

type QMetaFun = (String, Maybe StorableContext)

-- operator: >->

(>->) :: Computation Fun -> Computation Fun -> Computation Fun
f1 >-> f2 = Comp f1 f2


qfun :: String -> Computation Fun
qfun = Fun

qfun1 :: (Show a, Storable a) => String -> a -> Computation Fun
qfun1 = Fun1


hfun :: String -> Predicate -> Computation Fun
hfun = HFun

hfun1 :: String -> Predicate -> Computation Fun -> Computation Fun
hfun1 = HFun1

hfun2 :: String -> Predicate -> Computation Fun -> Computation Fun -> Computation Fun
hfun2 = HFun2


-- Predefined predicates:

is_ip  = Pred "is_ip"
is_udp = Pred "is_udp"
is_tcp = Pred "is_tcp"

-- Predefined combinators:

(.|.), (.&.), (.^.) :: Predicate -> Predicate -> Predicate

p1 .|. p2 = Comb (Combinator "or" ) p1 p2
p1 .&. p2 = Comb (Combinator "and") p1 p2
p1 .^. p2 = Comb (Combinator "xor") p1 p2


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
broadcast   = qfun "broadcast"
sink        = qfun "sink"
drop'       = qfun "drop"

id'         = qfun  "id"

dummy       = qfun1 "dummy"    :: Int -> Computation Fun
counter     = qfun1 "counter"  :: Int -> Computation Fun
class'      = qfun1 "class"    :: Int -> Computation Fun

hdummy      = hfun "hdummy"       :: Predicate -> Computation Fun
when'       = hfun1 "when"        :: Predicate -> Computation Fun -> Computation Fun
unless'     = hfun1 "unless"      :: Predicate -> Computation Fun -> Computation Fun
conditional = hfun2 "conditional" :: Predicate -> Computation Fun -> Computation Fun -> Computation Fun

