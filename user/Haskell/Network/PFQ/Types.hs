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

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.PFQ.Types
  (
    IPv4(..)
  , CIDR(..)
  , inetAtoN
  , inetNtoA
  ) where

import GHC.Generics

import Data.Aeson
import Data.Typeable
import Data.String
import Data.List
import Data.Maybe (isJust, fromJust)
import Data.Scientific (toBoundedInteger)
import Control.Monad (when)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#else
import Data.Monoid()
#endif


import Network.Socket
import System.IO.Unsafe

import Foreign.Storable
import Foreign.Storable.Tuple()
import qualified Foreign.Storable.Newtype as Store

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc


-- | CInt instance...

instance ToJSON CInt where
  toJSON n = toJSON (fromIntegral n :: Int)

instance FromJSON CInt where
  parseJSON (Number n) = return (fromJust $ toBoundedInteger n)
  parseJSON _ = mempty


-- | IPv4 data type

newtype IPv4 = IPv4 { getHostAddress :: HostAddress } deriving (Generic, Typeable)

instance IsString IPv4 where
  fromString xs = unsafePerformIO $ inetAtoN xs

instance Show IPv4 where
    show a = unsafePerformIO $ inetNtoA a

instance ToJSON IPv4
instance FromJSON IPv4

instance Storable IPv4 where
    sizeOf    = Store.sizeOf getHostAddress
    alignment = Store.alignment getHostAddress
    peek      = Store.peek IPv4
    poke      = Store.poke getHostAddress


-- | CIDR data-type

newtype CIDR = CIDR { getNetworkPair :: (IPv4, CInt) } deriving (Generic, Typeable, Storable)


instance Show CIDR where
    show (CIDR (addr,prefix)) = unsafePerformIO (inetNtoA addr) ++ "/" ++ show prefix

instance IsString CIDR where
  fromString xs = CIDR (fromString addr, read $ tail prefix)
    where (addr, prefix) = if isJust slash
                            then splitAt (fromJust slash) xs
                            else error "CIDR: bad format (slash missing)"
          slash = elemIndex '/' xs

instance ToJSON CIDR
instance FromJSON CIDR


-- Thread-safe utility functions for IPv4 conversion to String and viceversa

inetAtoN :: String -> IO IPv4
inetAtoN xs =
  withCString xs $ \str ->
    allocaBytes 4 $ \addr -> do
      r <- (inet_pton (packFamily AF_INET) str addr)
      when (r /= 1) $ error "inetAtoN: bad address format"
      fmap IPv4 (peek $ castPtr addr)


inetNtoA :: IPv4 -> IO String
inetNtoA (IPv4 h) =
  alloca $ \ptr -> do
  poke ptr h
  allocaBytes 16 $ \str -> do
    p <- inet_ntop (packFamily AF_INET) (castPtr ptr) str 16
    when (p == nullPtr) $ error "inetNtoA: bad IPv4 format"
    peekCString str


-- FFI network functions:

foreign import ccall unsafe "inet_ntop"
  inet_ntop :: CInt -> Ptr () -> Ptr CChar -> CSize -> IO (Ptr ())

foreign import ccall unsafe "inet_pton"
  inet_pton :: CInt -> Ptr CChar -> Ptr () -> IO CInt


