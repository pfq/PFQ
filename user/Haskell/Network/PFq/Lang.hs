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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Network.PFq.Lang
    (
        StorableArgument(..),
        Pretty(..),
        Argument(..),
        Function(..),
        Serializable(..),
        FunctionDescr(..),
        Action,
        SkBuff,
        NetFunction,
        NetPredicate,
        NetProperty,
        (>->),
        typeOf',
    ) where


import Control.Monad.Identity
import Foreign.Storable
import Data.Word
import Data.Typeable
import Data.List.Split

-- Basic types...

newtype SkBuff = SkBuff ()
                   deriving Typeable

newtype Action a = Identity a
                    deriving Typeable

type Symbol      = String
type Signature   = String


-- Expressions:

data StorableArgument = forall a. (Show a, Storable a) => StorableArgument a

instance Show StorableArgument where
        show (StorableArgument c) = show c


data Argument = ArgData StorableArgument | ArgFun Int
                    deriving (Show)


data FunctionDescr = FunctionDescr Symbol Signature [Argument] (Int, Int)
                        deriving (Show)

relink :: Int -> Int -> FunctionDescr -> FunctionDescr
relink n1 n2 (FunctionDescr sym sig args (l,r) ) =
        FunctionDescr sym sig args (update n1 n2 l, update n1 n2 r)
            where update n1 n2 x = if x == n1 then n2 else x


-- DLS NetFunction

type NetFunction  = Function (SkBuff -> Action SkBuff)
type NetPredicate = Function (SkBuff -> Bool)
type NetProperty  = Function (SkBuff -> Word64)

data Function f where {

        MFunction  :: (Symbol,Signature) -> NetFunction;
        MFunction1 :: forall a. (Show a, Storable a) => (Symbol,Signature) -> a -> NetFunction;

        HFunction  :: (Symbol,Signature) -> NetPredicate -> NetFunction;
        HFunction1 :: (Symbol,Signature) -> NetPredicate -> NetFunction -> NetFunction;
        HFunction2 :: (Symbol,Signature) -> NetPredicate -> NetFunction -> NetFunction -> NetFunction;

        Predicate  :: (Symbol,Signature) -> NetPredicate;
        Predicate1 :: forall a. (Show a, Storable a) => (Symbol,Signature) -> a -> NetPredicate;
        Predicate2 :: (Symbol,Signature) -> NetProperty -> NetPredicate;
        Predicate3 :: forall a. (Show a, Storable a) => (Symbol,Signature) -> NetProperty -> a -> NetPredicate;

        Property   :: (Symbol,Signature) -> NetProperty;
        Property1  :: forall a. (Show a, Storable a) => (Symbol,Signature) -> a -> NetProperty;

        Combinator1 :: (Symbol,Signature) -> NetPredicate -> NetPredicate;
        Combinator2 :: (Symbol,Signature) -> NetPredicate -> NetPredicate -> NetPredicate;

        Composition :: forall f1 f2 f. (Serializable (Function f1), Serializable (Function f2)) => Function f1 -> Function f2 -> Function f;

    } deriving (Typeable)


-- DLS Kleisli operator: >->

(>->) :: Function (a -> m b) -> Function (b -> m c) -> Function (a -> m c)
f1 >-> f2 = Composition f1 f2


-- typeOf' utility function:

typeOf' :: (Typeable a) => a -> String
typeOf' f = unwords . (splitOn "Function ") $ show $ typeOf f


-- Show instance:

instance Show (Function f) where
        show (MFunction  (symb,sig))         = "(" ++ symb ++ " :: " ++ sig ++ ")"
        show (MFunction1 (symb,sig) a)       = "(" ++ symb ++ " " ++ show a ++ " :: " ++ sig ++ ")"

        show (HFunction  (symb,sig) p)       = "(" ++ symb ++ " " ++ show p  ++ " :: " ++ sig  ++ ")"
        show (HFunction1 (symb,sig) p n1)    = "(" ++ symb ++ " " ++ show p  ++ " (" ++ show n1 ++ ") :: " ++  sig  ++ ")"
        show (HFunction2 (symb,sig) p n1 n2) = "(" ++ symb ++ " " ++ show p  ++ " (" ++ show n1 ++ ") (" ++ show n2 ++ ") :: " ++ sig ++ ")"

        show (Predicate  (symb,sig))         = "(" ++ symb ++ " :: " ++ sig ++ ")"
        show (Predicate1 (symb,sig) a)       = "(" ++ symb ++ " " ++ show a ++ " :: " ++ sig ++ ")"
        show (Predicate2 (symb,sig) p)       = "(" ++ symb ++ " " ++ show p ++ " :: " ++ sig ++ ")"
        show (Predicate3 (symb,sig) p a)     = "(" ++ symb ++ " " ++ show p ++ " " ++ show a ++ " :: " ++ sig ++ ")"

        show (Property  (symb,sig))          = "(" ++ symb ++ " :: " ++ sig ++ ")"
        show (Property1 (symb,sig) a)        = "(" ++ symb ++ " " ++ show a ++ " :: " ++ sig ++ ")"

        show (Combinator1 ("not",sig) p)     = "(not " ++ show p ++ " :: " ++ sig ++ ")"
        show (Combinator2 ("and",sig) p1 p2) = "(" ++ show p1 ++" && " ++ show p2 ++ ") :: " ++ sig ++ ")"
        show (Combinator2 ("or" ,sig) p1 p2) = "(" ++ show p1 ++" || " ++ show p2 ++ ") :: " ++ sig ++ ")"
        show (Combinator2 ("xor",sig) p1 p2) = "(" ++ show p1 ++" ^^ " ++ show p2 ++ ") :: " ++ sig ++ ")"
        show (Combinator1 (_,_) _)           = undefined
        show (Combinator2 (_,_) _ _)         = undefined

        show (Composition a b)               = show a ++ " >-> " ++ show b

-- Pretty class:

class Pretty x where
        prettyPrint :: x -> String

instance Pretty (Function f) where
        prettyPrint (MFunction (symb,_))           = symb
        prettyPrint (MFunction1 (symb,_) a)        = "(" ++ symb ++ " " ++ show a ++ ")"
        prettyPrint (HFunction (symb,_) p)         = "(" ++ symb ++ " " ++ prettyPrint p  ++ ")"
        prettyPrint (HFunction1 (symb,_) p n1)     = "(" ++ symb ++ " " ++ prettyPrint p  ++ " (" ++ prettyPrint n1 ++ "))"
        prettyPrint (HFunction2 (symb,_) p n1 n2)  = "(" ++ symb ++ " " ++ prettyPrint p  ++ " (" ++ prettyPrint n1 ++ ") (" ++ prettyPrint n2 ++ "))"

        prettyPrint (Predicate  (symb,_))          = symb
        prettyPrint (Predicate1 (symb,_) a)        = "(" ++ symb ++ " " ++ show a ++ ")"
        prettyPrint (Predicate2 (symb,_) p)        = "(" ++ symb ++ " " ++ prettyPrint p ++ ")"
        prettyPrint (Predicate3 (symb,_) p a)      = "(" ++ symb ++ " " ++ prettyPrint p ++ " " ++ show a ++ ")"

        prettyPrint (Property (symb,_))            = symb
        prettyPrint (Property1 (symb,_) a)         = "(" ++ symb ++ " " ++ show a ++ ")"

        prettyPrint (Combinator1 ("not",_) p)      = "(not " ++ prettyPrint p ++ ")"
        prettyPrint (Combinator2 ("and",_) p1 p2)  = "(" ++ prettyPrint p1 ++" && " ++ prettyPrint p2 ++ ")"
        prettyPrint (Combinator2 ("or" ,_) p1 p2)  = "(" ++ prettyPrint p1 ++" || " ++ prettyPrint p2 ++ ")"
        prettyPrint (Combinator2 ("xor",_) p1 p2)  = "(" ++ prettyPrint p1 ++" ^^ " ++ prettyPrint p2 ++ ")"
        prettyPrint (Combinator1 (_,_) _)          = undefined
        prettyPrint (Combinator2 (_,_) _ _)        = undefined

        prettyPrint (Composition a b)              = prettyPrint a ++ " >-> " ++ prettyPrint b


-- Serializable class:


class Serializable a where
    serialize :: Int -> a -> ([FunctionDescr], Int)


instance Serializable (Function (a -> m b)) where

    serialize n (MFunction  (symb, sig))    = ([FunctionDescr symb sig [] (n+1, n+1)], n+1)
    serialize n (MFunction1 (symb, sig) x)  = ([FunctionDescr symb sig [ArgData $ StorableArgument x] (n+1,n+1) ], n+1)
    serialize n (HFunction  (symb, sig) p)  = let (s1, n1) = ([FunctionDescr symb sig [] (n2, n2) ], n+1)
                                                  (s2, n2) =  serialize n1 p
                                              in (s1 ++ s2, n2)

    serialize n (HFunction1 (symb, sig) p c) = let (s1, n1) = ([FunctionDescr symb sig [ArgFun n1] (n3, n2) ], n+1)
                                                   (s2, n2) =  serialize n1 p
                                                   (s3, n3) =  serialize n2 c
                                               in (s1 ++ s2 ++ s3, n3)

    serialize n (HFunction2 (symb, sig) p c1 c2) = let (s1, n1) = ([FunctionDescr symb sig [ArgFun n1] (n3, n2) ], n+1)
                                                       (s2, n2) =  serialize n1 p
                                                       (s3, n3) =  serialize n2 c1
                                                       (s4, n4) =  serialize n3 c2
                                                   in (s1 ++ s2 ++ (map (relink n3 n4) s3) ++ s4, n4)

    serialize n (Composition a b) = let (s1, n1) = serialize n  a
                                        (s2, n2) = serialize n1 b
                                    in (s1 ++ s2, n2)


instance Serializable NetPredicate where
    serialize n (Predicate  (symb, sig))    = ([FunctionDescr symb sig [] (-1,-1) ], n+1)
    serialize n (Predicate1 (symb, sig) x)  = ([FunctionDescr symb sig [ArgData $ StorableArgument x] (-1,-1) ], n+1)
    serialize n (Predicate2 (symb, sig) p)  = let (s1, n1) = ([FunctionDescr symb sig [ArgFun $ n+1] (-1,-1) ], n+1)
                                                  (s2, n2) = serialize n1 p
                                              in (s1 ++ s2, n2)

    serialize n (Predicate3 (symb, sig) p x)= let (s1, n1) = ([FunctionDescr symb sig [ArgFun $ n+1, ArgData $ StorableArgument x] (-1,-1) ], n+1)
                                                  (s2, n2) = serialize n1 p
                                              in (s1 ++ s2, n2)

    serialize n (Combinator1 (symb, sig) p) = let (s1, n1) = ([FunctionDescr symb sig [ArgFun $ n+1] (-1,-1) ], n+1)
                                                  (s2, n2) = serialize n1 p
                                              in (s1 ++ s2, n2)

    serialize n (Combinator2 (symb, sig) p1 p2) = let (s1, n1) = ([FunctionDescr symb sig [ArgFun $ n+1, ArgFun $ n+2] (-1,-1) ], n+1)
                                                      (s2, n2) = serialize n1 p1
                                                      (s3, n3) = serialize n2 p2
                                              in (s1 ++ s2 ++ s3, n3)


instance Serializable NetProperty where
    serialize n (Property  (symb, sig))    = ([FunctionDescr symb sig [] (-1, -1) ], n+1)
    serialize n (Property1 (symb, sig) x)  = ([FunctionDescr symb sig [ArgData $ StorableArgument x] (-1, -1) ], n+1)

