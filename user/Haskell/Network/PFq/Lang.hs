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

        IPv4(..),
        NetDevice(..),
        Argument(..),
        Pretty(..),
        Function(..),
        Serializable(..),
        FunctionDescr(..),
        Action,
        SkBuff,

        -- * Function types

        NetFunction,
        NetPredicate,
        NetProperty,
        (>->),
    ) where


import Debug.Trace

import Network.Socket
import Foreign.Storable
import Foreign.Storable.Newtype as Store

import Data.Word
import Data.String
import System.IO.Unsafe


-- | IPv4 data type

newtype IPv4 = IPv4 { getIP4Address :: HostAddress }

instance IsString IPv4 where
    fromString xs = IPv4 $ unsafePerformIO (inet_addr xs)

instance Storable IPv4 where
    sizeOf    = Store.sizeOf getIP4Address
    alignment = Store.alignment getIP4Address
    peek      = Store.peek IPv4
    poke      = Store.poke getIP4Address

instance Show IPv4 where
    show a = unsafePerformIO $ inet_ntoa (getIP4Address a)


-- |Symbol is a 'String' representing the name of a function.

type Symbol = String


-- |SkBuff placeholder type is used to model the kernel sk_buff data structure.

newtype SkBuff = SkBuff ()


-- |Function pointer data type represents a function in a list of FunctionDescr.

newtype FunPtr = FunPtr Int

instance Show FunPtr where
    show (FunPtr n) = "FunPtr(" ++ show n ++ ")"

instance Pretty FunPtr where
    pretty (FunPtr n) = "FunPtr(" ++ show n ++ ")"

-- |Action is a monad modelled after the Identity and implemented at kernel level.

newtype Action a = Identity a


-- |NetDevice data type.

data NetDevice = Dev String | DevQueue String Int
                    deriving (Eq, Show, Read)


-- | Argument data type.
-- Any PFQ/lang function can take up to 8 Arguments.

data Argument = forall a. (Show a, Storable a) => ArgData a     |
                forall a. (Show a, Storable a) => ArgVector [a] |
                ArgString String                                |
                ArgSVector [String]                             |
                ArgFunPtr Int                                   |
                ArgNull

instance Show Argument where
    show (ArgNull)       = "()"
    show (ArgFunPtr n)   = show (FunPtr n)
    show (ArgString xs)  = xs
    show (ArgData x)     = show x
    show (ArgVector xs)  = show xs
    show (ArgSVector xs) = show xs


-- | Argumentable class, a typeclass for building function Arguments.

class (Show a, Pretty a) => Argumentable a where
    argument :: a -> Argument

instance Argumentable String where
    argument = ArgString

instance Argumentable [String] where
    argument xs = ArgSVector xs

instance (Show a, Pretty a, Storable a) => Argumentable a where
    argument = ArgData

instance (Show a, Pretty [a], Storable a) => Argumentable [a] where
    argument xs = ArgVector xs

instance Argumentable FunPtr where
    argument (FunPtr n) = ArgFunPtr n

instance Argumentable () where
    argument () = ArgNull


mkArgument :: (Argumentable a) => a -> [FunctionDescr] -> Argument
mkArgument x [] = argument x
mkArgument _ xs = argument (FunPtr (functionIndex (head xs)))


-- | Function descriptor.

data FunctionDescr = FunctionDescr
                     {
                        functionSymbol    :: Symbol,
                        functionArguments :: [Argument],
                        functionIndex     :: Int,
                        functionLink      :: Int

                     }   deriving (Show)


-- |Simple monadic in-kernel PFQ/lang function.

type NetFunction  = Function (SkBuff -> Action SkBuff)


-- |Simple in-kernel PFQ/lang predicate.

type NetPredicate = Function (SkBuff -> Bool)

-- |Simple in-kernel PFQ/lang property function.

type NetProperty  = Function (SkBuff -> Word64)


-- | Parametric Function data type.

data Function fun where
    {
        MFunction    :: Symbol -> NetFunction;
        MFunction1   :: forall a. (Serializable a, Argumentable a) => Symbol -> a -> NetFunction;
        MFunction2   :: forall a b. (Serializable a, Argumentable a, Serializable b, Argumentable b) => Symbol -> a -> b -> NetFunction;
        MFunction3   :: forall a b c. (Serializable a, Argumentable a, Serializable b, Argumentable b, Serializable c, Argumentable c) => Symbol -> a -> b -> c -> NetFunction;

        Predicate    :: forall a b c d e f g h. (Serializable a, Argumentable a,
                                                 Serializable b, Argumentable b,
                                                 Serializable c, Argumentable c,
                                                 Serializable d, Argumentable d,
                                                 Serializable e, Argumentable e,
                                                 Serializable f, Argumentable f,
                                                 Serializable g, Argumentable g,
                                                 Serializable h, Argumentable h
                                                ) => Symbol -> a -> b -> c -> d -> e -> f -> g -> h -> NetPredicate;

        Property     :: forall a b c d e f g h. (Serializable a, Argumentable a,
                                                 Serializable b, Argumentable b,
                                                 Serializable c, Argumentable c,
                                                 Serializable d, Argumentable d,
                                                 Serializable e, Argumentable e,
                                                 Serializable f, Argumentable f,
                                                 Serializable g, Argumentable g,
                                                 Serializable h, Argumentable h
                                                ) => Symbol -> a -> b -> c -> d -> e -> f -> g -> h -> NetProperty;

        Combinator1  :: Symbol -> NetPredicate -> NetPredicate;
        Combinator2  :: Symbol -> NetPredicate -> NetPredicate -> NetPredicate;

        Composition  :: forall f1 f2 f. (Serializable (Function f1), Serializable (Function f2)) => Function f1 -> Function f2 -> Function f;
    }


instance Storable NetFunction where
    sizeOf    = undefined
    alignment = undefined
    peek      = undefined
    poke      = undefined

instance Storable NetProperty where
    sizeOf    = undefined
    alignment = undefined
    peek      = undefined
    poke      = undefined

instance Storable NetPredicate where
    sizeOf    = undefined
    alignment = undefined
    peek      = undefined
    poke      = undefined


-- | Like unwords, drop empty string istead...

unwords' = unwords . (filter (not . null))

-- |Kleisli left-to-right operator, for monadic composition of PFQ/lang functions.

(>->) :: Function (a -> m b) -> Function (b -> m c) -> Function (a -> m c)
f1 >-> f2 = Composition f1 f2


instance Show (Function f) where

        show (MFunction  symb)           = "(Function " ++ symb ++ ")"
        show (MFunction1 symb a)         = "(Function " ++ symb ++ " " ++ show a ++ ")"
        show (MFunction2 symb a b)       = "(Function " ++ symb ++ " " ++ show a ++ " " ++ show b ++ ")"
        show (MFunction3 symb a b c)     = "(Function " ++ symb ++ " " ++ show a ++ " " ++ show b ++ " " ++ show c ++ ")"

        show (Predicate symb a b c d e f g h) = let args = unwords' [show a, show b, show c, show d, show e, show f, show g, show h]
                                                in if null args then symb
                                                                else "(Predicate " ++ symb ++ " " ++ args  ++ ")"

        show (Property symb a b c d e f g h) = let args = unwords' [show a, show b, show c, show d, show e, show f, show g, show h]
                                               in if null args then symb
                                                               else "(Property " ++ symb ++ " " ++ args  ++ ")"

        show (Combinator1 "not" p)       = "(Combinator not " ++ show p  ++ ")"
        show (Combinator2 "and" p1 p2)   = "(Combinator and " ++ show p1 ++" " ++ show p2 ++ ")"
        show (Combinator2 "or"  p1 p2)   = "(Combinator or  " ++ show p1 ++" " ++ show p2 ++ ")"
        show (Combinator2 "xor" p1 p2)   = "(Combinator xor " ++ show p1 ++" " ++ show p2 ++ ")"
        show (Combinator1 {})            = undefined
        show (Combinator2 {})            = undefined

        show (Composition a b)           = "(Composition " ++ show a ++ " " ++ show b ++ ")"


-- | Pretty class, typeclass used to print a PFQ/lang computation.

class Pretty x where
        pretty :: x -> String

instance (Show a) => Pretty a where
    pretty = show

instance Pretty String where
    pretty = id

instance Pretty [String] where
    pretty = unwords

instance (Pretty a) => Pretty [a] where
    pretty xs = unwords (map pretty xs)

instance Pretty () where
    pretty _ = ""


instance Pretty (Function f) where

        pretty (MFunction symb)            = symb
        pretty (MFunction1 symb a)         = "(" ++ symb ++ " " ++ pretty a ++ ")"
        pretty (MFunction2 symb a b)       = "(" ++ symb ++ " " ++ pretty a ++ " " ++ pretty b ++ ")"
        pretty (MFunction3 symb a b c)     = "(" ++ symb ++ " " ++ pretty a ++ " " ++ pretty b ++ " " ++ pretty c ++ ")"

        pretty (Predicate symb a b c d e f g h) = let args = unwords' [pretty a, pretty b, pretty c, pretty d, pretty e, pretty f, pretty g, pretty h]
                                                  in if null args then symb
                                                                  else "(" ++ symb ++ " " ++ args ++ ")"

        pretty (Property symb a b c d e f g h) = let args = unwords' [pretty a, pretty b, pretty c, pretty d, pretty e, pretty f, pretty g, pretty h]
                                                 in if null args then symb
                                                                 else "(" ++ symb ++ " " ++ args ++ ")"

        pretty (Combinator1 "not" p)       = "(not " ++ pretty p ++ ")"
        pretty (Combinator2 "and" p1 p2)   = "(" ++ pretty p1 ++" && " ++ pretty p2 ++ ")"
        pretty (Combinator2 "or"  p1 p2)   = "(" ++ pretty p1 ++" || " ++ pretty p2 ++ ")"
        pretty (Combinator2 "xor" p1 p2)   = "(" ++ pretty p1 ++" ^^ " ++ pretty p2 ++ ")"
        pretty (Combinator1{} )            = undefined
        pretty (Combinator2{})             = undefined
        pretty (Composition a b)           = pretty a ++ " >-> " ++ pretty b


-- | Serializable class, a typeclass used to serialize computations.
-- Transform a Function into a list of FunctionDescr.

class Serializable a where
    serialize :: a -> Int -> ([FunctionDescr], Int)


instance Serializable (Function f) where

    serialize (MFunction  symb)    n   = ([FunctionDescr symb [] n (n+1) ], n+1)

    serialize (MFunction1  symb c)  n = let (s1, n1) = ([FunctionDescr symb [mkArgument c s2] n n2 ], n+1)
                                            (s2, n2) =  serialize c n1
                                        in (s1 ++ fixComputation n1 s2, n2)

    serialize (MFunction2 symb c1 c2) n = let (s1, n1) = ([FunctionDescr symb [mkArgument c1 s2, mkArgument c2 s3] n n3 ], n+1)
                                              (s2, n2) =  serialize c1 n1
                                              (s3, n3) =  serialize c2 n2
                                          in (s1 ++ fixComputation n1 s2 ++ fixComputation n2 s3, n3)

    serialize (MFunction3 symb p c1 c2) n = let (s1, n1) = ([FunctionDescr symb [mkArgument p s2, mkArgument c1 s3, mkArgument c2 s4] n n4 ], n+1)
                                                (s2, n2) =  serialize p  n1
                                                (s3, n3) =  serialize c1 n2
                                                (s4, n4) =  serialize c2 n3
                                            in (s1 ++ fixComputation n1 s2 ++ fixComputation n2 s3 ++ fixComputation n3 s4, n4)

    serialize (Composition a b) n = let (s1, n1) = serialize a n
                                        (s2, n2) = serialize b n1
                                    in (s1 ++ s2, n2)

    serialize _ _ = undefined



instance Serializable NetPredicate where

    serialize (Predicate symb a b c d e f g h) n = let (s1, n1) = ([FunctionDescr symb [mkArgument a s2,
                                                                                        mkArgument b s3,
                                                                                        mkArgument c s4,
                                                                                        mkArgument d s5,
                                                                                        mkArgument e s6,
                                                                                        mkArgument f s7,
                                                                                        mkArgument g s8,
                                                                                        mkArgument h s9] n (-1) ], n+1)
                                                       (s2, n2) = serialize a n1
                                                       (s3, n3) = serialize b n2
                                                       (s4, n4) = serialize c n3
                                                       (s5, n5) = serialize d n4
                                                       (s6, n6) = serialize e n5
                                                       (s7, n7) = serialize f n6
                                                       (s8, n8) = serialize g n7
                                                       (s9, n9) = serialize h n8

                                                   in (s1 ++ fixComputation n1 s2 ++
                                                             fixComputation n2 s3 ++
                                                             fixComputation n3 s4 ++
                                                             fixComputation n4 s5 ++
                                                             fixComputation n5 s6 ++
                                                             fixComputation n6 s7 ++
                                                             fixComputation n7 s8 ++
                                                             fixComputation n8 s9, n9)


    serialize (Combinator1 symb p) n = let (s1, n1) = ([FunctionDescr symb [mkArgument p s2] n (-1) ], n+1)
                                           (s2, n2) = serialize p n1
                                       in (s1 ++ s2, n2)

    serialize (Combinator2 symb p1 p2) n = let (s1, n1) = ([FunctionDescr symb [mkArgument p1 s2, mkArgument p2 s3] n (-1) ], n+1)
                                               (s2, n2) = serialize p1 n1
                                               (s3, n3) = serialize p2 n2
                                           in (s1 ++ s2 ++ s3, n3)
    serialize _ _ = undefined


instance Serializable NetProperty where

    serialize (Property  symb a b c d e f g h) n = let (s1, n1) = ([FunctionDescr symb [mkArgument a s2,
                                                                                        mkArgument b s3,
                                                                                        mkArgument c s4,
                                                                                        mkArgument d s5,
                                                                                        mkArgument e s6,
                                                                                        mkArgument f s7,
                                                                                        mkArgument g s8,
                                                                                        mkArgument h s9] n (-1) ], n+1)
                                                       (s2, n2) = serialize a n1
                                                       (s3, n3) = serialize b n2
                                                       (s4, n4) = serialize c n3
                                                       (s5, n5) = serialize d n4
                                                       (s6, n6) = serialize e n5
                                                       (s7, n7) = serialize f n6
                                                       (s8, n8) = serialize g n7
                                                       (s9, n9) = serialize h n8

                                                   in (s1 ++ fixComputation n1 s2 ++
                                                             fixComputation n2 s3 ++
                                                             fixComputation n3 s4 ++
                                                             fixComputation n4 s5 ++
                                                             fixComputation n5 s6 ++
                                                             fixComputation n6 s7 ++
                                                             fixComputation n7 s8 ++
                                                             fixComputation n8 s9, n9)

    serialize _ _ = undefined


instance Serializable a where
    serialize _ n = ([], n)

instance Serializable () where
    serialize _ n = ([], n)

fixComputation :: Int -> [FunctionDescr] -> [FunctionDescr]
fixComputation n xs = map (\(FunctionDescr sym as cur next) -> FunctionDescr sym as cur (cut next)) xs
                where
                    cut x = if x == (n + length xs) then (-1) else x

