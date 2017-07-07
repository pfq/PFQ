--    Copyright (c) 2011-16, Nicola Bonelli
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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Network.PFQ.Lang
    (
      -- * Basic types

      IPv4(..)
    , CIDR(..)
    , Argument(..)
    , Pretty(..)
    , Function(..)
    , Serializable(..)
    , FunctionDescr(..)
    , Qbuff(..)
    , Action

    -- * Function types

    , NetFunction
    , NetPredicate
    , NetProperty
    , (>->)
    ) where


import Control.Monad()
import Control.Monad.Identity

import GHC.Generics
import Data.Int
import Data.Word

import Data.Aeson
import Data.Typeable
import Data.List (isPrefixOf)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Monoid
import Data.Functor()
#else
import Data.Monoid()
#endif

import Foreign.Storable
import Network.PFQ.Types

-- |Symbol is a 'String' representing the name of a function.

type Symbol = String


-- |Qbuff placeholder type is used to model the kernel sk_buff data structure.

newtype Qbuff = Qbuff ()

-- |Function pointer data type represents a function in a list of FunctionDescr.

newtype FunPtr = FunPtr Int deriving (Generic, Typeable)

instance ToJSON FunPtr
instance FromJSON FunPtr


instance Show FunPtr where
    show (FunPtr n) = "FunPtr(" ++ show n ++ ")"

instance Pretty FunPtr where
    pretty (FunPtr n) = "FunPtr(" ++ show n ++ ")"


-- |Action is a monad modelled after the Identity as it is implemented at kernel level.

newtype Action a = Action { getIdentity :: Identity a } deriving (Functor, Applicative, Monad, Typeable)


-- | Argument data type.
-- Any pfq-lang function can take up to 8 Arguments.

data Argument = forall a. (Show a, Storable a, Typeable a, ToJSON a, FromJSON a) => ArgData a       |
                forall a. (Show a, Storable a, Typeable a, ToJSON a, FromJSON a) => ArgVector [a]   |
                ArgString String                                                                    |
                ArgStrings [String]                                                                 |
                ArgFunPtr Int                                                                       |
                ArgNull


instance ToJSON Argument where
  toJSON (ArgData x)     = object [ "argType" .= show (typeOf x),        "argValue" .= toJSON x  ]
  toJSON (ArgVector xs)  = object [ "argType" .= show (typeOf xs),       "argValue" .= toJSON xs ]
  toJSON (ArgString xs)  = object [ "argType" .= ("String"   :: String), "argValue" .= toJSON xs ]
  toJSON (ArgStrings xs) = object [ "argType" .= ("[String]" :: String), "argValue" .= toJSON xs ]
  toJSON (ArgFunPtr x)   = object [ "argType" .= ("Fun"      :: String), "argValue" .= toJSON x  ]
  toJSON ArgNull         = object [ "argType" .= (""         :: String), "argValue" .= toJSON () ]


instance FromJSON Argument where
  parseJSON (Object v) = do
    type_ <- v .: "argType"
    case () of
      _ | type_ == "Int"    -> (ArgData :: Int   -> Argument)      <$>  (v .: "argValue")
        | type_ == "Int64"  -> (ArgData :: Int64  -> Argument)     <$>  (v .: "argValue")
        | type_ == "Int32"  -> (ArgData :: Int32  -> Argument)     <$>  (v .: "argValue")
        | type_ == "Int16"  -> (ArgData :: Int16  -> Argument)     <$>  (v .: "argValue")
        | type_ == "Int8"   -> (ArgData :: Int8   -> Argument)     <$>  (v .: "argValue")
        | type_ == "Word64" -> (ArgData :: Word64 -> Argument)     <$>  (v .: "argValue")
        | type_ == "Word32" -> (ArgData :: Word32 -> Argument)     <$>  (v .: "argValue")
        | type_ == "Word16" -> (ArgData :: Word16 -> Argument)     <$>  (v .: "argValue")
        | type_ == "Word8"  -> (ArgData :: Word8  -> Argument)     <$>  (v .: "argValue")
        | type_ == "IPv4"   -> (ArgData :: IPv4   -> Argument)     <$>  (v .: "argValue")
        | type_ == "CIDR"   -> (ArgData :: CIDR   -> Argument)     <$>  (v .: "argValue")
        | type_ == "String" -> (ArgString :: String -> Argument)   <$>  (v .: "argValue")
        | type_ == "Fun"    -> (ArgFunPtr :: Int    -> Argument)   <$>  (v .: "argValue")
        | "[" `isPrefixOf` type_ ->
            case () of
              _ | type_ == "[Int]"    -> (ArgVector  :: [Int]   -> Argument) <$> (v .: "argValue")
                | type_ == "[Int64]"  -> (ArgVector  :: [Int64]  -> Argument) <$> (v .: "argValue")
                | type_ == "[Int32]"  -> (ArgVector  :: [Int32]  -> Argument) <$> (v .: "argValue")
                | type_ == "[Int16]"  -> (ArgVector  :: [Int16]  -> Argument) <$> (v .: "argValue")
                | type_ == "[Int8]"   -> (ArgVector  :: [Int8]   -> Argument) <$> (v .: "argValue")
                | type_ == "[Word64]" -> (ArgVector  :: [Word64] -> Argument) <$> (v .: "argValue")
                | type_ == "[Word32]" -> (ArgVector  :: [Word32] -> Argument) <$> (v .: "argValue")
                | type_ == "[Word16]" -> (ArgVector  :: [Word16] -> Argument) <$> (v .: "argValue")
                | type_ == "[Word8]"  -> (ArgVector  :: [Word8]  -> Argument) <$> (v .: "argValue")
                | type_ == "[IPv4]"   -> (ArgVector  :: [IPv4]   -> Argument) <$> (v .: "argValue")
                | type_ == "[CIDR]"   -> (ArgVector  :: [CIDR]   -> Argument) <$> (v .: "argValue")
                | type_ == "[String]" -> (ArgStrings :: [String] -> Argument) <$> (v .: "argValue")
                | otherwise -> error $ "FromJSON: Argument type " ++ type_ ++ " not supported!"
        | null type_          -> return ArgNull
        | otherwise           -> error $ "FromJSON: Argument type " ++ type_ ++ " not supported!"

  parseJSON _ = mempty


instance Show Argument where
    show ArgNull         = "()"
    show (ArgFunPtr n)   = show (FunPtr n)
    show (ArgString xs)  = show xs
    show (ArgData x)     = show x
    show (ArgVector xs)  = show xs
    show (ArgStrings xs) = show xs


instance Pretty Argument where
    pretty  ArgNull        = ""
    pretty (ArgFunPtr n)   = show (FunPtr n)
    pretty (ArgString xs)  = show xs
    pretty (ArgData x)     = show x
    pretty (ArgVector xs)  = show xs
    pretty (ArgStrings xs) = show xs


-- | ArgumentClass class, a typeclass for building function Arguments.

class (Show a, Pretty a, ToJSON a, FromJSON a, Serializable a) => ArgumentClass a where
    argument :: a -> Argument

instance ArgumentClass String where
    argument = ArgString

instance ArgumentClass [String] where
    argument = ArgStrings

instance
#if __GLASGOW_HASKELL__ >= 710
 {-# OVERLAPPABLE #-}
#endif
  (Show a, Pretty a, Storable a, Typeable a, ToJSON a, FromJSON a, Serializable a) => ArgumentClass a where
    argument = ArgData

instance
#if __GLASGOW_HASKELL__ >= 710
 {-# OVERLAPPABLE #-}
#endif
  (Show a, Pretty [a], Storable a, Typeable a, ToJSON a, FromJSON a, Serializable [a]) => ArgumentClass [a] where
    argument = ArgVector

instance ArgumentClass FunPtr where
    argument (FunPtr n) = ArgFunPtr n

instance ArgumentClass () where
    argument () = ArgNull


mkArgument :: (ArgumentClass a) => a -> [FunctionDescr] -> Argument
mkArgument x [] = argument x
mkArgument _ xs = argument (FunPtr (funIndex (head xs)))


-- | Function descriptor.

data FunctionDescr =
  FunctionDescr
  { funSymbol    :: Symbol
  , funArgs      :: [Argument]
  , funIndex     :: Int
  , funLink      :: Int
  } deriving (Show, Generic)


instance ToJSON FunctionDescr
instance FromJSON FunctionDescr


instance ToJSON (Function f) where
  toJSON comp = toJSON (fst(serialize comp 0))

instance FromJSON (Function f) where
  parseJSON = undefined



-- |Simple monadic in-kernel pfq-lang function.

type NetFunction  = Function (Qbuff -> Action Qbuff)

-- |Simple in-kernel pfq-lang predicate.

type NetPredicate = Function (Qbuff -> Bool)

-- |Simple in-kernel pfq-lang property function.

type NetProperty  = Function (Qbuff -> Word64)



-- | Parametric Function data type.

data Function fun where

        Function :: forall a b c d e f g h.
          ( ArgumentClass a
          , ArgumentClass b
          , ArgumentClass c
          , ArgumentClass d
          , ArgumentClass e
          , ArgumentClass f
          , ArgumentClass g
          , ArgumentClass h) => Symbol -> a -> b -> c -> d -> e -> f -> g -> h -> NetFunction

        Predicate :: forall a b c d e f g h.
          ( ArgumentClass a
          , ArgumentClass b
          , ArgumentClass c
          , ArgumentClass d
          , ArgumentClass e
          , ArgumentClass f
          , ArgumentClass g
          , ArgumentClass h) => Symbol -> a -> b -> c -> d -> e -> f -> g -> h -> NetPredicate

        Property :: forall a b c d e f g h.
          ( ArgumentClass a
          , ArgumentClass b
          , ArgumentClass c
          , ArgumentClass d
          , ArgumentClass e
          , ArgumentClass f
          , ArgumentClass g
          , ArgumentClass h) => Symbol -> a -> b -> c -> d -> e -> f -> g -> h -> NetProperty

        Combinator1  :: Symbol -> NetPredicate -> NetPredicate
        Combinator2  :: Symbol -> NetPredicate -> NetPredicate -> NetPredicate
        Kleisli      :: NetFunction -> NetFunction -> NetFunction


-- |Kleisli left-to-right operator

(>->) :: NetFunction -> NetFunction -> NetFunction
f1 >-> f2 = Kleisli f1 f2


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


instance Show (Function f) where

        show (Function  symb a b c d e f g h) = showFunction "Function"  symb a b c d e f g h
        show (Predicate symb a b c d e f g h) = showFunction "Predicate" symb a b c d e f g h
        show (Property  symb a b c d e f g h) = showFunction "Property"  symb a b c d e f g h

        show (Combinator1 "not" p)     = "(Combinator not " ++ show p  ++ ")"
        show (Combinator2 "and" p1 p2) = "(Combinator and " ++ show p1 ++" " ++ show p2 ++ ")"
        show (Combinator2 "or"  p1 p2) = "(Combinator or  " ++ show p1 ++" " ++ show p2 ++ ")"
        show (Combinator2 "xor" p1 p2) = "(Combinator xor " ++ show p1 ++" " ++ show p2 ++ ")"
        show Combinator1 {} = undefined
        show Combinator2 {} = undefined

        show (Kleisli a b) = "(Kleisli " ++ show a ++ " " ++ show b ++ ")"


-- | Pretty class, typeclass used to print a pfq-lang computation.

class Pretty x where
        pretty :: x -> String

instance
#if __GLASGOW_HASKELL__ >= 710
 {-# OVERLAPPABLE #-}
#endif
  (Show a) => Pretty a where
    pretty = show

instance Pretty String where
    pretty = id

instance
 Pretty [String] where
    pretty = prettyUnwords

instance
#if __GLASGOW_HASKELL__ >= 710
 {-# OVERLAPPABLE #-}
#endif
  (Pretty a) => Pretty [a] where
    pretty xs = prettyUnwords (map pretty xs)

instance Pretty () where
    pretty _ = ""


instance Pretty (Function f) where

        pretty (Function  symb a b c d e f g h) = prettyFunction symb a b c d e f g h
        pretty (Predicate symb a b c d e f g h) = prettyFunction symb a b c d e f g h
        pretty (Property  symb a b c d e f g h) = prettyFunction symb a b c d e f g h

        pretty (Combinator1 "not" p)     = "(not " ++ pretty p ++ ")"
        pretty (Combinator2 "and" p1 p2) = "(" ++ pretty p1 ++" && " ++ pretty p2 ++ ")"
        pretty (Combinator2 "or"  p1 p2) = "(" ++ pretty p1 ++" || " ++ pretty p2 ++ ")"
        pretty (Combinator2 "xor" p1 p2) = "(" ++ pretty p1 ++" ^^ " ++ pretty p2 ++ ")"
        pretty Combinator1{} = undefined
        pretty Combinator2{} = undefined
        pretty (Kleisli a b) = pretty a ++ " >-> " ++ pretty b


-- | Serializable class, a typeclass used to serialize computations.
-- Transform a Function into a list of FunctionDescr.

class (Show a) => Serializable a where
    serialize :: a -> Int -> ([FunctionDescr], Int)


serializeFun :: ( ArgumentClass a
                , ArgumentClass b
                , ArgumentClass c
                , ArgumentClass d
                , ArgumentClass e
                , ArgumentClass f
                , ArgumentClass g
                , ArgumentClass h) => String -> Int -> Bool -> a -> b -> c -> d -> e -> f -> g -> h -> ([FunctionDescr], Int)
serializeFun symb n cont a b c d e f g h =
    let (s1, n1) = ([FunctionDescr symb [mkArgument a s2,
                                         mkArgument b s3,
                                         mkArgument c s4,
                                         mkArgument d s5,
                                         mkArgument e s6,
                                         mkArgument f s7,
                                         mkArgument g s8,
                                         mkArgument h s9] n (if cont then n9 else (-1)) ], n+1)
        (s2, n2) = serialize a n1
        (s3, n3) = serialize b n2
        (s4, n4) = serialize c n3
        (s5, n5) = serialize d n4
        (s6, n6) = serialize e n5
        (s7, n7) = serialize f n6
        (s8, n8) = serialize g n7
        (s9, n9) = serialize h n8

    in (s1 ++ fixDescrList n1 s2 ++
              fixDescrList n2 s3 ++
              fixDescrList n3 s4 ++
              fixDescrList n4 s5 ++
              fixDescrList n5 s6 ++
              fixDescrList n6 s7 ++
              fixDescrList n7 s8 ++
              fixDescrList n8 s9, n9)

    where fixDescrList :: Int -> [FunctionDescr] -> [FunctionDescr]
          fixDescrList idx xs = map (\(FunctionDescr sym as cur next) -> FunctionDescr sym as cur (cut next)) xs
                where
                    cut x = if x == (idx + length xs) then (-1) else x


instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPING #-}
#endif
  Serializable (Function f) where

    serialize (Function symb a b c d e f g h) n = serializeFun symb n True a b c d e f g h

    serialize (Kleisli a b) n = let (s1, n1) = serialize a n
                                    (s2, n2) = serialize b n1
                                in (s1 ++ s2, n2)

    serialize (Combinator1 symb p) n = let (s1, n1) = ([FunctionDescr symb [mkArgument p s2, ArgNull, ArgNull, ArgNull, ArgNull, ArgNull, ArgNull, ArgNull] n (-1) ], n+1)
                                           (s2, n2) = serialize p n1
                                       in (s1 ++ s2, n2)

    serialize (Combinator2 symb p1 p2) n = let (s1, n1) = ([FunctionDescr symb [mkArgument p1 s2, mkArgument p2 s3, ArgNull, ArgNull, ArgNull, ArgNull, ArgNull, ArgNull] n (-1) ], n+1)
                                               (s2, n2) = serialize p1 n1
                                               (s3, n3) = serialize p2 n2
                                           in (s1 ++ s2 ++ s3, n3)

    serialize (Predicate symb a b c d e f g h) n = serializeFun symb n False a b c d e f g h

    serialize (Property symb a b c d e f g h) n = serializeFun symb n False a b c d e f g h


instance
#if __GLASGOW_HASKELL__ >= 710
 {-# OVERLAPPING #-}
#endif
  Serializable () where
      serialize _ n = ([], n)


instance
#if __GLASGOW_HASKELL__ >= 710
 {-# OVERLAPPABLE #-}
#endif
  (Show a) => Serializable a where
      serialize _ n = ([], n)


showFunction :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => String -> String -> a -> b -> c -> d -> e -> f -> g -> h -> String
showFunction kind symb a b c d e f g h =
  let args = prettyUnwords [show a, show b, show c, show d, show e, show f, show g, show h]
    in if null args then "(" ++ kind ++ " " ++ symb ++ ")"
                    else "(" ++ kind ++ " " ++ symb ++ " " ++ args  ++ ")"

prettyFunction :: (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h) => String -> a -> b -> c -> d -> e -> f -> g -> h -> String
prettyFunction symb a b c d e f g h =
  let args = prettyUnwords [pretty a, pretty b, pretty c, pretty d, pretty e, pretty f, pretty g, pretty h]
  in if null args then symb
                  else "(" ++ symb ++ " " ++ args  ++ ")"

prettyUnwords :: [String] -> String
prettyUnwords = unwords . filter (not .null)

