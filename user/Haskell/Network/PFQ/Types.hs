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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.PFQ.Types
  (
    IPv4(..)
  , inetAtoN
  , inetNtoA
  ) where

import GHC.Generics

import Data.Aeson
import Data.Typeable
import Data.String

import Network.Socket
import System.IO.Unsafe

import Foreign.Storable
import qualified Foreign.Storable.Newtype as Store

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc


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


-- Thread-safe utility functions for IPv4 conversion to String and viceversa

inetAtoN :: String -> IO IPv4
inetAtoN xs =
  withCString xs $ \str ->
    allocaBytes 4 $ \addr -> do
      inet_pton (packFamily AF_INET) str addr
      fmap IPv4 (peek $ castPtr addr)


inetNtoA :: IPv4 -> IO String
inetNtoA (IPv4 h) =
  alloca $ \ptr -> do
  poke ptr h
  allocaBytes 16 $ \str -> do
    inet_ntop (packFamily AF_INET) (castPtr ptr) str 16
    peekCString str


-- FFI network functions:

foreign import ccall unsafe "inet_ntop"
  inet_ntop :: CInt -> Ptr () -> Ptr CChar -> CSize -> IO ()

foreign import ccall unsafe "inet_pton"
  inet_pton :: CInt -> Ptr CChar -> Ptr () -> IO ()


