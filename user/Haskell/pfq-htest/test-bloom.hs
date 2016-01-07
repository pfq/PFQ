--
--
--  (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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

import Network.PFQ as Q
import Network.PFQ.Lang
import Network.PFQ.Lang.Default

import Foreign
import System.Environment
import Numeric
import Control.Monad


-- import Debug.Trace

dumpPacket :: Q.Packet -> IO ()
dumpPacket p = do
                Q.waitForPacket p
                bytes <- peekByteOff (Q.pData p) 0 :: IO Word64
                putStrLn $ "[" ++ showHex bytes "" ++ "]"


recvLoop :: Ptr PFqTag -> IO ()
recvLoop q = do
    queue <- Q.read q 100000000
    gid   <- Q.getGroupId q
    if Q.qLen queue == 0
       then recvLoop q
       else do
            ps <- Q.getPackets queue
            mapM_ (getPacketHeader >=> print) ps
            mapM_ dumpPacket ps
            Q.getGroupCounters q gid >>= print
            recvLoop q


-- Context shared as Storable Pair
--

dumper :: String -> IO ()
dumper dev = do
    putStrLn  $ "dumping " ++ dev  ++ "..."
    fp <- Q.open 64 4096 4096
    withForeignPtr fp  $ \q -> do
        Q.timestampingEnable q True

        gid <- Q.getGroupId q
        Q.bindGroup q gid dev (-1)
        Q.enable q

        let m = bloomCalcM 2 0.000001

        let comp = bloom_filter (fromIntegral m) ["192.168.0.0"] 16 >-> log_packet

        putStrLn $ pretty comp
        Q.setGroupComputation q gid comp

        recvLoop q

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0   -> error "usage: test-bloom dev"
        _   -> dumper (head args)
