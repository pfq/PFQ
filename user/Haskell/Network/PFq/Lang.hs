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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
    ) where


-- import Control.Monad.Identity

import Foreign.Storable
import Data.Word

-- Basic types...

newtype SkBuff   = SkBuff ()
newtype Action a = Identity a
type Symbol      = String


-- Expressions:

data StorableArgument = forall a. (Show a, Storable a) => StorableArgument a

instance Show StorableArgument where
        show (StorableArgument c) = show c


data Argument = ArgNull | ArgData StorableArgument | ArgString String | ArgFun Int
                    deriving (Show)


data FunctionDescr = FunctionDescr Symbol [Argument] Int
                        deriving (Show)

termComp :: Int -> [FunctionDescr] -> [FunctionDescr]
termComp n xs = map (\(FunctionDescr sym as next) -> FunctionDescr sym as (cut next)) xs
                where
                    cut x = if x == (n + length xs) then (-1) else x

-- DLS NetFunction

type NetFunction  = Function (SkBuff -> Action SkBuff)
type NetPredicate = Function (SkBuff -> Bool)
type NetProperty  = Function (SkBuff -> Word64)


data Function f where {

        MFunction  :: Symbol -> NetFunction;
        MFunction1 :: forall a. (Show a, Storable a) => Symbol -> a -> NetFunction;
        MFunction2 :: Symbol -> String -> NetFunction;
        MFunction3 :: forall a. (Show a, Storable a) => Symbol -> a -> NetPredicate -> NetFunction;
        MFunction4 :: Symbol -> String -> NetPredicate -> NetFunction;

        HFunction  :: Symbol -> NetPredicate -> NetFunction;
        HFunction1 :: Symbol -> NetPredicate -> NetFunction -> NetFunction;
        HFunction2 :: Symbol -> NetPredicate -> NetFunction -> NetFunction -> NetFunction;
        HFunction3 :: Symbol -> NetFunction  -> NetFunction;
        HFunction4 :: Symbol -> NetFunction  -> NetFunction -> NetFunction;

        Predicate  :: Symbol -> NetPredicate;
        Predicate1 :: forall a. (Show a, Storable a) => Symbol -> a -> NetPredicate;
        Predicate2 :: Symbol -> NetProperty -> NetPredicate;
        Predicate3 :: forall a. (Show a, Storable a) => Symbol -> NetProperty -> a -> NetPredicate;

        Property   :: Symbol -> NetProperty;
        Property1  :: forall a. (Show a, Storable a) => Symbol -> a -> NetProperty;

        Combinator1 :: Symbol -> NetPredicate -> NetPredicate;
        Combinator2 :: Symbol -> NetPredicate -> NetPredicate -> NetPredicate;

        Composition :: forall f1 f2 f. (Serializable (Function f1), Serializable (Function f2)) => Function f1 -> Function f2 -> Function f;

    }


-- DLS Kleisli operator: >->

(>->) :: Function (a -> m b) -> Function (b -> m c) -> Function (a -> m c)
f1 >-> f2 = Composition f1 f2


-- Show instance:

instance Show (Function f) where

        show (MFunction  symb)          = "(MFunction " ++ symb ++ ")"
        show (MFunction1 symb a)        = "(MFunction " ++ symb ++ " " ++ show a ++ ")"
        show (MFunction2 symb s)        = "(MFunction " ++ symb ++ " " ++ show s ++ ")"
        show (MFunction3 symb a p)      = "(MFunction " ++ symb ++ " " ++ show a ++ " " ++ show p ++ ")"
        show (MFunction4 symb s p)      = "(MFunction " ++ symb ++ " " ++ show s ++ " " ++ show p ++ ")"

        show (HFunction  symb p)        = "(HFunction " ++ symb ++ " " ++ show p  ++ ")"
        show (HFunction1 symb p n1)     = "(HFunction " ++ symb ++ " " ++ show p  ++ " " ++ show n1 ++ ")"
        show (HFunction2 symb p n1 n2)  = "(HFunction " ++ symb ++ " " ++ show p  ++ " " ++ show n1 ++ " " ++ show n2 ++ ")"
        show (HFunction3 symb f)        = "(HFunction " ++ symb ++ " " ++ show f  ++ ")"
        show (HFunction4 symb f g)      = "(HFunction " ++ symb ++ " " ++ show f  ++ " " ++ show g ++ ")"

        show (Predicate  symb)          = "(Predicate " ++ symb ++  ")"
        show (Predicate1 symb a)        = "(Predicate " ++ symb ++ " " ++ show a ++ ")"
        show (Predicate2 symb p)        = "(Predicate " ++ symb ++ " " ++ show p ++ ")"
        show (Predicate3 symb p a)      = "(Predicate " ++ symb ++ " " ++ show p ++ " " ++ show a ++ ")"

        show (Property  symb)           = "(Property " ++ symb ++ ")"
        show (Property1 symb a)         = "(Property " ++ symb ++ " " ++ show a ++ ")"

        show (Combinator1 "not" p)      = "(Combinator not " ++ show p  ++ ")"
        show (Combinator2 "and" p1 p2)  = "(Combinator and " ++ show p1 ++" " ++ show p2 ++ ")"
        show (Combinator2 "or"  p1 p2)  = "(Combinator or  " ++ show p1 ++" " ++ show p2 ++ ")"
        show (Combinator2 "xor" p1 p2)  = "(Combinator xor " ++ show p1 ++" " ++ show p2 ++ ")"
        show (Combinator1{})            = undefined
        show (Combinator2 {})           = undefined

        show (Composition a b)          = "(Composition " ++ show a ++ " " ++ show b ++ ")"

-- Pretty class:

class Pretty x where
        pretty :: x -> String

instance Pretty (Function f) where
        pretty (MFunction symb)           = symb
        pretty (MFunction1 symb a)        = "(" ++ symb ++ " " ++ show a ++ ")"
        pretty (MFunction2 symb s)        = "(" ++ symb ++ " " ++ show s ++ ")"
        pretty (MFunction3 symb a p)      = "(" ++ symb ++ " " ++ show a ++ " " ++ pretty p ++ " )"
        pretty (MFunction4 symb s p)      = "(" ++ symb ++ " " ++ show s ++ " " ++ pretty p ++ " )"

        pretty (HFunction symb p)         = "(" ++ symb ++ " " ++ pretty p  ++ ")"
        pretty (HFunction1 symb p n1)     = "(" ++ symb ++ " " ++ pretty p  ++ " " ++ pretty n1 ++ ")"
        pretty (HFunction2 symb p n1 n2)  = "(" ++ symb ++ " " ++ pretty p  ++ " " ++ pretty n1 ++ " " ++ pretty n2 ++ ")"
        pretty (HFunction3 symb f)        = "(" ++ symb ++ " " ++ pretty f  ++ ")"
        pretty (HFunction4 symb f g)      = "(" ++ symb ++ " " ++ pretty f  ++ " " ++ pretty g ++ ")"

        pretty (Predicate  symb)          = symb
        pretty (Predicate1 symb a)        = "(" ++ symb ++ " " ++ show a ++ ")"
        pretty (Predicate2 symb p)        = "(" ++ symb ++ " " ++ pretty p ++ ")"
        pretty (Predicate3 symb p a)      = "(" ++ symb ++ " " ++ pretty p ++ " " ++ show a ++ ")"

        pretty (Property symb)            = symb
        pretty (Property1 symb a)         = "(" ++ symb ++ " " ++ show a ++ ")"

        pretty (Combinator1 "not" p)      = "(not " ++ pretty p ++ ")"
        pretty (Combinator2 "and" p1 p2)  = "(" ++ pretty p1 ++" && " ++ pretty p2 ++ ")"
        pretty (Combinator2 "or"  p1 p2)  = "(" ++ pretty p1 ++" || " ++ pretty p2 ++ ")"
        pretty (Combinator2 "xor" p1 p2)  = "(" ++ pretty p1 ++" ^^ " ++ pretty p2 ++ ")"
        pretty (Combinator1{} )           = undefined
        pretty (Combinator2{})            = undefined
        pretty (Composition a b)          = pretty a ++ " >-> " ++ pretty b


-- Serializable class:


class Serializable a where
    serialize :: a -> Int -> ([FunctionDescr], Int)


instance Serializable (Function (a -> m b)) where

    serialize (MFunction  symb)    n = ([FunctionDescr symb [] (n+1) ], n+1)
    serialize (MFunction1 symb x)  n = ([FunctionDescr symb [ArgData $ StorableArgument x] (n+1) ], n+1)
    serialize (MFunction2 symb s)  n = ([FunctionDescr symb [ArgString s] (n+1) ], n+1)
    serialize (MFunction3 symb x p) n = let (s1, n1) = ([FunctionDescr symb [ArgData $ StorableArgument x, ArgFun n1] n2 ], n+1)
                                            (s2, n2) =  serialize p n1
                                        in (s1 ++ s2, n2)
    serialize (MFunction4 symb s p) n = let (s1, n1) = ([FunctionDescr symb [ArgString s, ArgFun n1] n2 ], n+1)
                                            (s2, n2) =  serialize p n1
                                        in (s1 ++ s2, n2)

    serialize (HFunction  symb p)  n = let (s1, n1) = ([FunctionDescr symb [ArgFun n1] n2 ], n+1)
                                           (s2, n2) =  serialize p n1
                                       in (s1 ++ s2, n2)

    serialize (HFunction1 symb p c) n = let (s1, n1) = ([FunctionDescr symb [ArgFun n1, ArgFun n2] n3 ], n+1)
                                            (s2, n2) =  serialize p n1
                                            (s3, n3) =  serialize c n2
                                         in (s1 ++ s2 ++ termComp n2 s3, n3)

    serialize (HFunction2 symb p c1 c2) n = let (s1, n1) = ([FunctionDescr symb [ArgFun n1, ArgFun n2, ArgFun n3] n4 ], n+1)
                                                (s2, n2) =  serialize p  n1
                                                (s3, n3) =  serialize c1 n2
                                                (s4, n4) =  serialize c2 n3
                                             in (s1 ++ s2 ++ termComp n2 s3 ++ termComp n3 s4, n4)

    serialize (HFunction3  symb f)  n = let (s1, n1) = ([FunctionDescr symb [ArgFun n1] n2 ], n+1)
                                            (s2, n2) =  serialize f n1
                                        in (s1 ++ termComp n1 s2, n2)

    serialize (HFunction4  symb f g) n = let (s1, n1) = ([FunctionDescr symb [ArgFun n1, ArgFun n2] n3 ], n+1)
                                             (s2, n2) =  serialize f n1
                                             (s3, n3) =  serialize g n2
                                         in (s1 ++ termComp n1 s2 ++ termComp n2 s3, n3)

    serialize (Composition a b) n = let (s1, n1) = serialize a n
                                        (s2, n2) = serialize b n1
                                    in (s1 ++ s2, n2)

    serialize _ _ = undefined

instance Serializable NetPredicate where
    serialize (Predicate  symb)    n = ([FunctionDescr symb [] (-1) ], n+1)
    serialize (Predicate1 symb x)  n = ([FunctionDescr symb [ArgData $ StorableArgument x] (-1) ], n+1)
    serialize (Predicate2 symb p)  n = let (s1, n1) = ([FunctionDescr symb [ArgFun n1] (-1) ], n+1)
                                           (s2, n2) = serialize p n1
                                       in (s1 ++ s2, n2)

    serialize (Predicate3 symb p x) n = let (s1, n1) = ([FunctionDescr symb [ArgFun n1, ArgData $ StorableArgument x] (-1) ], n+1)
                                            (s2, n2) = serialize p n1
                                        in (s1 ++ s2, n2)

    serialize (Combinator1 symb p) n = let (s1, n1) = ([FunctionDescr symb [ArgFun n1] (-1) ], n+1)
                                           (s2, n2) = serialize p n1
                                       in (s1 ++ s2, n2)

    serialize (Combinator2 symb p1 p2) n = let (s1, n1) = ([FunctionDescr symb [ArgFun n1, ArgFun n2] (-1) ], n+1)
                                               (s2, n2) = serialize p1 n1
                                               (s3, n3) = serialize p2 n2
                                           in (s1 ++ s2 ++ s3, n3)
    serialize _ _ = undefined


instance Serializable NetProperty where
    serialize (Property  symb)    n = ([FunctionDescr symb [] (-1) ], n+1)
    serialize (Property1 symb x)  n = ([FunctionDescr symb [ArgData $ StorableArgument x] (-1) ], n+1)

    serialize _ _ = undefined

