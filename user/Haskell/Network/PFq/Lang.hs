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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Network.PFq.Lang
    (
        -- * Basic types

        NetDevice(..),
        Argument(..),
        Pretty(..),
        Function(..),
        Serializable(..),
        Vector(..),
        FunctionDescr(..),
        Action,
        SkBuff,

        -- * Function types

        NetFunction,
        NetPredicate,
        NetProperty,
        (>->),
    ) where


-- import Control.Monad.Identity

import Foreign.Storable

import Data.Word

-- Basic types...

-- |Symbol is a value of type 'String' and it is used to represent the name of
-- a function.

type Symbol = String

-- |SkBuff is a placeholder type, used to model the kernel sk_buff data structure.

newtype SkBuff = SkBuff ()

-- |Vector data type

newtype Vector a = Vector [a]

-- |Function data type

newtype Fun = Fun Int

-- |Action is a monad modelled after the Identity monad and implemented at kernel level.

newtype Action a = Identity a

-- |NetDevice data type

data NetDevice = Dev String | DevQueue String Int
                    deriving (Eq, Show, Read)

data Argument = forall a. (Show a, Storable a) => ArgData a     |
                forall a. (Show a, Storable a) => ArgVector [a] |
                ArgString String                                |
                ArgFun Int                                      |
                ArgNull

instance Show Argument where
    show (ArgNull)      = "()"
    show (ArgFun n)     = "f(" ++ show n ++ ")"
    show (ArgString xs) = xs
    show (ArgData x)    = show x
    show (ArgVector xs) = show xs


instance (Show a) => Show (Vector a) where
    show (Vector xs) = show xs


class Argumentable a where
    mkArgument :: a -> Argument

instance Argumentable String where
    mkArgument = ArgString

instance (Show a, Storable a) => Argumentable a where
    mkArgument = ArgData

instance (Show a, Storable a) => Argumentable (Vector a) where
    mkArgument (Vector xs) = ArgVector xs

instance Argumentable Fun where
    mkArgument (Fun n) = ArgFun n


data FunctionDescr = FunctionDescr Symbol [Argument] Int
                        deriving (Show)


-- |Function that takes a SkBuff and returns an Action SkBuff.

type NetFunction  = Function (SkBuff -> Action SkBuff)

-- |Function that takes a SkBuff and returns a Bool.

type NetPredicate = Function (SkBuff -> Bool)

-- |Function that takes a SkBuff and returns a generic Word64.
type NetProperty  = Function (SkBuff -> Word64)


data Function f where {

        MFunction    :: Symbol -> NetFunction;
        MFunction1   :: forall a. (Show a, Argumentable a) => Symbol -> a -> NetFunction;
        MFunction2   :: forall a b. (Show a, Argumentable a, Show b, Argumentable b) => Symbol -> a -> b -> NetFunction;
        MFunction1P  :: forall a. (Show a, Argumentable a) => Symbol -> a -> NetPredicate -> NetFunction;
        MFunctionP   :: Symbol -> NetPredicate -> NetFunction;
        MFunctionPF  :: Symbol -> NetPredicate -> NetFunction -> NetFunction;
        MFunctionPFF :: Symbol -> NetPredicate -> NetFunction -> NetFunction -> NetFunction;
        MFunctionF   :: Symbol -> NetFunction  -> NetFunction;
        MFunctionFF  :: Symbol -> NetFunction  -> NetFunction -> NetFunction;

        Predicate   :: Symbol -> NetPredicate;
        Predicate1  :: forall a. (Show a, Argumentable a) => Symbol -> a -> NetPredicate;
        Predicate2  :: forall a b. (Show a, Argumentable a, Show b, Argumentable b) => Symbol -> a -> b -> NetPredicate;
        PredicateR  :: Symbol -> NetProperty -> NetPredicate;
        PredicateR1 :: forall a. (Show a, Argumentable a) => Symbol -> NetProperty -> a -> NetPredicate;

        Property    :: Symbol -> NetProperty;
        Property1   :: forall a. (Show a, Argumentable a) => Symbol -> a -> NetProperty;

        Combinator1 :: Symbol -> NetPredicate -> NetPredicate;
        Combinator2 :: Symbol -> NetPredicate -> NetPredicate -> NetPredicate;

        Composition :: forall f1 f2 f. (Serializable (Function f1), Serializable (Function f2)) => Function f1 -> Function f2 -> Function f;

    }


-- |Kleisli left-to-right operator, for monadic composition of DLS NetFunction

(>->) :: Function (a -> m b) -> Function (b -> m c) -> Function (a -> m c)
f1 >-> f2 = Composition f1 f2


-- Show instance:

instance Show (Function f) where

        show (MFunction  symb)           = "(Function " ++ symb ++ ")"
        show (MFunction1 symb a)         = "(Function " ++ symb ++ " " ++ show a ++ ")"
        show (MFunction2 symb a b)       = "(Function " ++ symb ++ " " ++ show a ++ " " ++ show b ++ ")"
        show (MFunction1P symb a p)      = "(Function " ++ symb ++ " " ++ show a ++ " " ++ show p ++ ")"

        show (MFunctionP  symb p)        = "(Function " ++ symb ++ " " ++ show p  ++ ")"
        show (MFunctionPF symb p n1)     = "(Function " ++ symb ++ " " ++ show p  ++ " " ++ show n1 ++ ")"
        show (MFunctionPFF symb p n1 n2) = "(Function " ++ symb ++ " " ++ show p  ++ " " ++ show n1 ++ " " ++ show n2 ++ ")"
        show (MFunctionF symb f)         = "(Function " ++ symb ++ " " ++ show f  ++ ")"
        show (MFunctionFF symb f g)      = "(Function " ++ symb ++ " " ++ show f  ++ " " ++ show g ++ ")"

        show (Predicate  symb)           = "(Predicate " ++ symb ++  ")"
        show (Predicate1 symb a)         = "(Predicate " ++ symb ++ " " ++ show a ++ ")"
        show (Predicate2 symb a b)       = "(Predicate " ++ symb ++ " " ++ show a ++ " " ++ show b ++ ")"
        show (PredicateR symb p)         = "(Predicate " ++ symb ++ " " ++ show p ++ ")"
        show (PredicateR1 symb p a)      = "(Predicate " ++ symb ++ " " ++ show p ++ " " ++ show a ++ ")"

        show (Property  symb)            = "(Property " ++ symb ++ ")"
        show (Property1 symb a)          = "(Property " ++ symb ++ " " ++ show a ++ ")"

        show (Combinator1 "not" p)       = "(Combinator not " ++ show p  ++ ")"
        show (Combinator2 "and" p1 p2)   = "(Combinator and " ++ show p1 ++" " ++ show p2 ++ ")"
        show (Combinator2 "or"  p1 p2)   = "(Combinator or  " ++ show p1 ++" " ++ show p2 ++ ")"
        show (Combinator2 "xor" p1 p2)   = "(Combinator xor " ++ show p1 ++" " ++ show p2 ++ ")"
        show (Combinator1 {})            = undefined
        show (Combinator2 {})            = undefined

        show (Composition a b)           = "(Composition " ++ show a ++ " " ++ show b ++ ")"


-- | Pretty class:

class Pretty x where
        pretty :: x -> String

instance Pretty (Function f) where

        pretty (MFunction symb)            = symb
        pretty (MFunction1 symb a)         = "(" ++ symb ++ " " ++ show a ++ ")"
        pretty (MFunction2 symb a b)       = "(" ++ symb ++ " " ++ show a ++ " " ++ show b ++ " )"
        pretty (MFunction1P symb a p)      = "(" ++ symb ++ " " ++ show a ++ " " ++ pretty p ++ " )"

        pretty (MFunctionP symb p)         = "(" ++ symb ++ " " ++ pretty p  ++ ")"
        pretty (MFunctionPF symb p n1)     = "(" ++ symb ++ " " ++ pretty p  ++ " " ++ pretty n1 ++ ")"
        pretty (MFunctionPFF symb p n1 n2) = "(" ++ symb ++ " " ++ pretty p  ++ " " ++ pretty n1 ++ " " ++ pretty n2 ++ ")"
        pretty (MFunctionF symb f)         = "(" ++ symb ++ " " ++ pretty f  ++ ")"
        pretty (MFunctionFF symb f g)      = "(" ++ symb ++ " " ++ pretty f  ++ " " ++ pretty g ++ ")"

        pretty (Predicate  symb)           = symb
        pretty (Predicate1 symb a)         = "(" ++ symb ++ " " ++ show a ++ ")"
        pretty (Predicate2 symb a b)       = "(" ++ symb ++ " " ++ show a ++ " " ++ show b ++ ")"
        pretty (PredicateR symb p)         = "(" ++ symb ++ " " ++ pretty p ++ ")"
        pretty (PredicateR1 symb p a)      = "(" ++ symb ++ " " ++ pretty p ++ " " ++ show a ++ ")"

        pretty (Property symb)             = symb
        pretty (Property1 symb a)          = "(" ++ symb ++ " " ++ show a ++ ")"

        pretty (Combinator1 "not" p)       = "(not " ++ pretty p ++ ")"
        pretty (Combinator2 "and" p1 p2)   = "(" ++ pretty p1 ++" && " ++ pretty p2 ++ ")"
        pretty (Combinator2 "or"  p1 p2)   = "(" ++ pretty p1 ++" || " ++ pretty p2 ++ ")"
        pretty (Combinator2 "xor" p1 p2)   = "(" ++ pretty p1 ++" ^^ " ++ pretty p2 ++ ")"
        pretty (Combinator1{} )            = undefined
        pretty (Combinator2{})             = undefined
        pretty (Composition a b)           = pretty a ++ " >-> " ++ pretty b


-- Serializable class:


class Serializable a where
    serialize :: a -> Int -> ([FunctionDescr], Int)


instance Serializable (Function (a -> m b)) where

    serialize (MFunction  symb)    n   = ([FunctionDescr symb [] (n+1) ], n+1)
    serialize (MFunction1 symb x)  n   = ([FunctionDescr symb [mkArgument x] (n+1) ], n+1)
    serialize (MFunction2 symb x y) n  = ([FunctionDescr symb [mkArgument x, mkArgument y] (n+1) ], n+1)
    serialize (MFunction1P symb x p) n = let (s1, n1) = ([FunctionDescr symb [mkArgument x, mkArgument (Fun n1)] n2 ], n+1)
                                             (s2, n2) =  serialize p n1
                                         in (s1 ++ s2, n2)

    serialize (MFunctionP  symb p)  n = let (s1, n1) = ([FunctionDescr symb [mkArgument (Fun n1)] n2 ], n+1)
                                            (s2, n2) =  serialize p n1
                                        in (s1 ++ s2, n2)

    serialize (MFunctionPF symb p c) n = let (s1, n1) = ([FunctionDescr symb [mkArgument (Fun n1), mkArgument (Fun n2)] n3 ], n+1)
                                             (s2, n2) =  serialize p n1
                                             (s3, n3) =  serialize c n2
                                         in (s1 ++ s2 ++ termComp n2 s3, n3)

    serialize (MFunctionPFF symb p c1 c2) n = let (s1, n1) = ([FunctionDescr symb [mkArgument (Fun n1), mkArgument (Fun n2), mkArgument (Fun n3)] n4 ], n+1)
                                                  (s2, n2) =  serialize p  n1
                                                  (s3, n3) =  serialize c1 n2
                                                  (s4, n4) =  serialize c2 n3
                                              in (s1 ++ s2 ++ termComp n2 s3 ++ termComp n3 s4, n4)

    serialize (MFunctionF  symb f)  n = let (s1, n1) = ([FunctionDescr symb [mkArgument (Fun n1)] n2 ], n+1)
                                            (s2, n2) =  serialize f n1
                                        in (s1 ++ termComp n1 s2, n2)

    serialize (MFunctionFF  symb f g) n = let (s1, n1) = ([FunctionDescr symb [mkArgument (Fun n1), mkArgument (Fun n2)] n3 ], n+1)
                                              (s2, n2) =  serialize f n1
                                              (s3, n3) =  serialize g n2
                                         in (s1 ++ termComp n1 s2 ++ termComp n2 s3, n3)

    serialize (Composition a b) n = let (s1, n1) = serialize a n
                                        (s2, n2) = serialize b n1
                                    in (s1 ++ s2, n2)

    serialize _ _ = undefined


instance Serializable NetPredicate where
    serialize (Predicate  symb)     n = ([FunctionDescr symb [] (-1) ], n+1)
    serialize (Predicate1 symb x)   n = ([FunctionDescr symb [mkArgument x] (-1) ], n+1)
    serialize (Predicate2 symb x y) n = ([FunctionDescr symb [mkArgument x, mkArgument y] (-1) ], n+1)
    serialize (PredicateR symb p)   n = let (s1, n1) = ([FunctionDescr symb [mkArgument (Fun n1)] (-1) ], n+1)
                                            (s2, n2) = serialize p n1
                                        in (s1 ++ s2, n2)

    serialize (PredicateR1 symb p x) n = let (s1, n1) = ([FunctionDescr symb [mkArgument (Fun n1), mkArgument x] (-1) ], n+1)
                                             (s2, n2) = serialize p n1
                                         in (s1 ++ s2, n2)

    serialize (Combinator1 symb p) n = let (s1, n1) = ([FunctionDescr symb [mkArgument (Fun n1)] (-1) ], n+1)
                                           (s2, n2) = serialize p n1
                                       in (s1 ++ s2, n2)

    serialize (Combinator2 symb p1 p2) n = let (s1, n1) = ([FunctionDescr symb [mkArgument (Fun n1), mkArgument (Fun n2)] (-1) ], n+1)
                                               (s2, n2) = serialize p1 n1
                                               (s3, n3) = serialize p2 n2
                                           in (s1 ++ s2 ++ s3, n3)
    serialize _ _ = undefined


instance Serializable NetProperty where
    serialize (Property  symb)    n = ([FunctionDescr symb [] (-1) ], n+1)
    serialize (Property1 symb x)  n = ([FunctionDescr symb [mkArgument x] (-1) ], n+1)

    serialize _ _ = undefined


termComp :: Int -> [FunctionDescr] -> [FunctionDescr]
termComp n xs = map (\(FunctionDescr sym as next) -> FunctionDescr sym as (cut next)) xs
                where
                    cut x = if x == (n + length xs) then (-1) else x

