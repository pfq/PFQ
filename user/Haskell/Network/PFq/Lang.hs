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

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Network.PFq.Lang
    (
        StorableArgument(..),
        Argument(..),
        Function(..),
        Serializable(..),
        Expr(..),
        Action,
        SkBuff,
        NetFunction,
        NetPredicate,
        NetProperty,
        (>->),

    ) where


import Control.Monad.Identity
import Foreign.Storable
import Data.Word

-- Basic types...

newtype SkBuff   = SkBuff ()
newtype Action a = Identity a

type Symbol      = String
type Signature   = String


-- StorableArgument

data StorableArgument = forall a. (Show a, Storable a) => StorableArgument a

instance Show StorableArgument where
        show (StorableArgument c) = show c


-- AST Expression:

data Argument = ArgData StorableArgument | ArgFun Expr
                    deriving Show

data Expr = Nil | Expr Symbol Signature [Argument] Expr
                deriving (Show)


-- Serializable class

class Serializable a where
    serialize :: Int -> a -> (Expr, Int)


-- NetFunction

type NetFunction  = Function (SkBuff -> Action SkBuff)
type NetPredicate = Function (SkBuff -> Bool)
type NetProperty  = Function (SkBuff -> Word64)

data Function f where
    MFunction  :: (Symbol,Signature) -> NetFunction
    MFunction1 :: forall a. (Show a, Storable a) => (Symbol,Signature) -> a -> NetFunction

    HFunction  :: (Symbol,Signature) -> NetPredicate -> NetFunction
    HFunction1 :: (Symbol,Signature) -> NetPredicate -> NetFunction -> NetFunction
    HFunction2 :: (Symbol,Signature) -> NetPredicate -> NetFunction -> NetFunction -> NetFunction

    Predicate  :: (Symbol,Signature) -> NetPredicate
    Predicate1 :: forall a. (Show a, Storable a) => (Symbol,Signature) -> a -> NetPredicate
    Predicate2 :: forall a. (Show a, Storable a) => (Symbol,Signature) -> NetProperty -> a -> NetPredicate

    Property   :: (Symbol,Signature) -> NetProperty
    Property1  :: forall a. (Show a, Storable a) => (Symbol,Signature) -> a -> NetProperty

    Combinator1 :: (Symbol,Signature) -> NetPredicate -> NetPredicate
    Combinator2 :: (Symbol,Signature) -> NetPredicate -> NetPredicate -> NetPredicate

    Compound   :: forall f1 f2 f. Function f1 -> Function f2 -> Function f

-- Kleisli operator: >->

(>->) :: Function (a -> m b) -> Function (b -> m c) -> Function (a -> m c)
f1 >-> f2 = Compound f1 f2


-- Show instance:

instance Show (Function f) where
        show (MFunction (symb,_))           = symb
        show (MFunction1 (symb,_) a)        = "(" ++ symb ++ " " ++ show a ++ ")"

        show (HFunction (symb,_) p)         = "(" ++ symb ++ " " ++ show p  ++ ")"
        show (HFunction1 (symb,_) p n1)     = "(" ++ symb ++ " " ++ show p  ++ " (" ++ show n1 ++ "))"
        show (HFunction2 (symb,_) p n1 n2)  = "(" ++ symb ++ " " ++ show p  ++ " (" ++ show n1 ++ ") (" ++ show n2 ++ "))"

        show (Predicate  (symb,_))          = symb
        show (Predicate1 (symb,_) a)        = "(" ++ symb ++ " " ++ show a ++ ")"
        show (Predicate2 (symb,_) p a)      = "(" ++ symb ++ " " ++ show p ++ " " ++ show a ++ ")"

        show (Property (symb,_))            = symb
        show (Property1 (symb,_) a)         = "(" ++ symb ++ " " ++ show a ++ ")"

        show (Combinator1 ("not",_) p)      = "(not " ++ show p ++ ")"
        show (Combinator2 ("and",_) p1 p2)  = "(" ++ show p1 ++" && " ++ show p2 ++ ")"
        show (Combinator2 ("or" ,_) p1 p2)  = "(" ++ show p1 ++" || " ++ show p2 ++ ")"
        show (Combinator2 ("xor",_) p1 p2)  = "(" ++ show p1 ++" ^^ " ++ show p2 ++ ")"
        show (Combinator1 (_,_) _)          = undefined
        show (Combinator2 (_,_) _ _)        = undefined

        show (Compound a b) = show a ++ " >-> " ++ show b


-- relinkFunDescr :: Int -> Int -> FunDescr -> FunDescr
-- relinkFunDescr n1 n2 (FunDescr t name nargs arg l r) =
--         FunDescr t name nargs arg (update n1 n2 l) (update n1 n2 r)
--             where update n1 n2 x = if x == n1 then n2 else x


-- instance Serializable Combinator where
--         serialize n (Combinator name) = ([FunDescr { functionalType  = CombinatorFun,
--                                                      functionalSymb  = name,
--                                                      functionalNargs = 2,
--                                                      functionalArg   = Empty,
--                                                      functionalLeft  = -1,
--                                                      functionalRight = -1 }], n+1)
--
-- instance Serializable Property where
--         serialize n (Prop name)   = ([FunDescr { functionalType  = PropertyFun,
--                                                  functionalSymb  = name,
--                                                  functionalNargs = 0,
--                                                  functionalArg   = Empty,
--                                                  functionalLeft  = -1,
--                                                  functionalRight = -1 }], n+1)
--
--         serialize n (Prop1 name x) = ([FunDescr { functionalType  = PropertyFun,
--                                                   functionalSymb  = name,
--                                                   functionalNargs = 1,
--                                                   functionalArg   = ArgData $ StorableArgument x,
--                                                   functionalLeft  = -1,
--                                                   functionalRight = -1 }], n+1)
--
--
-- instance Serializable Predicate where
--         serialize n (Pred name)   = ([FunDescr { functionalType  = PredicateFun,
--                                                  functionalSymb  = name,
--                                                  functionalNargs = 0,
--                                                  functionalArg   = Empty,
--                                                  functionalLeft  = -1,
--                                                  functionalRight = -1 }], n+1)
--
--         serialize n (Pred1 name x) = ([FunDescr { functionalType  = PredicateFun,
--                                                   functionalSymb  = name,
--                                                   functionalNargs = 1,
--                                                   functionalArg   = ArgData $ StorableArgument x,
--                                                   functionalLeft  = -1,
--                                                   functionalRight = -1 }], n+1)
--
--         serialize n (Pred2 comb p1 p2) = let ([g'], n')   = serialize n comb
--                                              (f'',  n'')  = serialize n' p1
--                                              (f''', n''') = serialize n'' p2
--                                          in ( [g'{ functionalLeft = n', functionalRight = n''}] ++ f'' ++ f''', n''')
--
--         serialize n (Pred3 name p ) = let (f', n') = ([FunDescr { functionalType  = PredicateFun,
--                                                  functionalSymb  = name,
--                                                  functionalNargs = 1,
--                                                  functionalArg   = ArgFun (n+1),
--                                                  functionalLeft  = -1,
--                                                  functionalRight = -1 }], n+1)
--                                           (f'', n'') = serialize n' p
--                                       in (f' ++ f'', n'')
--
--         serialize n (Pred4 name p x) = let (f', n') = ([FunDescr { functionalType  = PredicateFun,
--                                                  functionalSymb  = name,
--                                                  functionalNargs = 2,
--                                                  functionalArg   = ArgDataFun (StorableArgument x) (n+1),
--                                                  functionalLeft  = -1,
--                                                  functionalRight = -1 }], n+1)
--                                            (f'', n'') = serialize n' p
--                                        in (f' ++ f'', n'')
--
--
-- instance Serializable (NetFunction f) where
--         serialize n (Fun name) = ([FunDescr { functionalType  = MonadicFun,
--                                               functionalSymb  = name,
--                                               functionalNargs = 0,
--                                               functionalArg   = Empty,
--                                               functionalLeft  = n+1,
--                                               functionalRight = n+1 }], n+1)
--
--         serialize n (Fun1 name x) = ([FunDescr { functionalType  = MonadicFun,
--                                                  functionalSymb  = name,
--                                                  functionalNargs = 1,
--                                                  functionalArg   = ArgData $ StorableArgument x,
--                                                  functionalLeft  = n+1,
--                                                  functionalRight = n+1 }], n+1)
--
--         serialize n (HFun name p) = let (s', n') = ([FunDescr { functionalType  = HighOrderFun,
--                                                                 functionalSymb  = name,
--                                                                 functionalNargs = 1,
--                                                                 functionalArg   = ArgFun n',
--                                                                 functionalLeft  = n'',
--                                                                 functionalRight = n'' }], n+1)
--                                         (p', n'') = serialize n' p
--                                     in (s' ++ p', n'')
--
--         serialize n (HFun1 name p c) = let (f', n') = (FunDescr { functionalType  = HighOrderFun,
--                                                                   functionalSymb  = name,
--                                                                   functionalNargs = 2,
--                                                                   functionalArg   = ArgFun n',
--                                                                   functionalLeft  = -1,
--                                                                   functionalRight = -1 }, n+1)
--                                            (p', n'') = serialize n' p
--                                            (c', n''') = serialize n'' c
--                                         in ( [f'{ functionalLeft = n''', functionalRight = n''}] ++ p' ++ c', n''')
--
--         serialize n (HFun2 name p c1 c2) = let (f', n') = (FunDescr { functionalType  = HighOrderFun,
--                                                                       functionalSymb  = name,
--                                                                       functionalNargs = 3,
--                                                                       functionalArg   = ArgFun n',
--                                                                       functionalLeft  = -1,
--                                                                       functionalRight = -1 }, n+1)
--                                                (p',  n'') = serialize n' p
--                                                (c1', n''') = serialize n'' c1
--                                                (c2', n'''') = serialize n''' c2
--                                             in ( [f'{ functionalLeft = n''', functionalRight = n''}] ++ p' ++ map (relinkFunDescr n''' n'''') c1' ++ c2', n'''')
--
--         serialize n (Comp c1 c2) = let (s1, n') = serialize n c1
--                                        (s2, n'') = serialize n' c2
--                                    in (s1 ++ s2, n'')
--
--

