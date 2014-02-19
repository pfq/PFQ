--
--
--  (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software Foundation,
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  The full GNU General Public License is included in this distribution in
--  the file called "COPYING".

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network.PFq as Q
import Foreign
import System.Environment
import Numeric
import Control.Monad

import Foreign.C.Types

import Control.Applicative

-- import Debug.Trace

dumpPacket :: Q.Packet -> IO ()
dumpPacket p = do
                Q.waitForPacket p
                bytes <- peekByteOff (Q.pData p) 0 :: IO Word64
                putStrLn $ "[" ++ showHex bytes "" ++ "]"

recvLoop :: Ptr PFqTag -> IO ()
recvLoop q = do
    queue <- Q.read q 100000000
    if Q.qLen queue == 0
       then recvLoop q
       else do
            ps <- Q.getPackets queue
            mapM_ (getHeader >=> print) ps
            mapM_ dumpPacket ps
            Q.getStats q >>= print
            recvLoop q


-- Context shared as Storable Pair
--

data Pair = Pair Int Int
    deriving (Show, Eq)

instance Storable Pair where
    alignment _ = alignment (undefined :: CInt)
    sizeOf    _ = 8
    peek p      = Pair <$> fmap fromIntegral ((\ptr -> peekByteOff ptr 0 ::IO CInt) p)
                       <*> fmap fromIntegral ((\ptr -> peekByteOff ptr 4 ::IO CInt) p)
    poke p (Pair a b) = do
        (\ptr val -> pokeByteOff ptr 0 (val::CInt)) p (fromIntegral a)
        (\ptr val -> pokeByteOff ptr 4 (val::CInt)) p (fromIntegral b)


dumper :: String -> IO ()
dumper dev = do
    putStrLn  $ "dumping " ++ dev  ++ "..."
    fp <- Q.open 64 0 4096
    withForeignPtr fp  $ \q -> do
        Q.setTimestamp q True

        gid <- Q.getGroupId q
        Q.bindGroup q gid dev (-1)
        Q.enable q

        -- Q.vlanFiltersEnabled q gid True
        -- Q.vlanSetFilterId q gid (0)   -- untagged
        -- Q.vlanSetFilterId q gid (-1)  -- anyTag
        -- Q.groupFunctions q gid ["steer-ipv4"]

        -- Test state (requires dummy-state comptuation)!

        Q.groupFunction q gid 0 "dummy-state"
        Q.groupFunction q gid 1 "clone"

        -- set state
        Q.putContextFunction q (Pair 10 42) gid 0

        -- read state of 8 bytes...
        kp :: Pair <- Q.getContextFunction q gid 0 8
        print kp

        Q.getRxSlotSize q >>= \o -> putStrLn $ "slot_size: " ++ show o
        recvLoop q

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0   -> error "usage: pfq-read dev"
        _   -> dumper (head args)
