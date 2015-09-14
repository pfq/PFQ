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
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.PFq as Q
import Network.PFq.Lang
import Network.PFq.Default
import Network.PFq.Experimental

import Foreign
import System.Environment
import Numeric
import Control.Monad


-- import Debug.Trace

prettyPrinter :: Serializable a => a -> IO ()
prettyPrinter comp = let (xs,_) = serialize comp 0
                 in forM_ (zip [0..] xs) $ \(n, x) -> putStrLn $ "    " ++ show n ++ ": " ++ show x


dumpPacket :: Q.Packet -> IO ()
dumpPacket p = do
    Q.waitForPacket p
    w0 <- liftM byteSwap64 $ peekByteOff (Q.pData p) 0 :: IO Word64
    w1 <- liftM byteSwap64 $ peekByteOff (Q.pData p) 8 :: IO Word64
    putStrLn $ "[" ++ showHex w0 "" ++ showHex w1 "" ++ "... ]"


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
            -- Q.getGroupCounters q gid >>= print
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

        -- pfq-lang example:
        -- let comp = (ip >-> addr "192.168.0.0" 16 >-> inc 0 >-> unit
        --                 >-> conditional (is_icmp .&&. has_addr "192.168.0.0" 16 .&&. (ip_tot_len .<. 1000) .&&. ip_id `any_bit` 0xffffffff )
        --                 (inc 1 >-> mark 1 >-> steer_ip >-> when' (has_mark 1) (inc 2))
        --                 drop')

        -- let comp = no_frag >-> forwardIO "lo" >-> tee "lo" is_icmp >-> dummy_vector [1,2,3] >-> par' icmp udp >-> addr "192.168.0.1" 24 >-> mark 42 >-> when' is_icmp (inc 1) >-> log_packet >-> log_msg "Hello World!"
        -- let comp = mark 0 >-> (conditional is_icmp (mark 1 >-> put_state 2) (mark 10 >-> put_state 20)) >-> log_msg "ok"

        -- putStrLn $ "Computation = " ++ pretty comp ++ ". Raw computation: "
        -- prettyPrinter comp
        -- Q.setGroupComputation q gid comp

        -- Q.vlanFiltersEnabled q gid True
        -- Q.vlanSetFilterId q gid (0)   -- untagged
        -- Q.vlanSetFilterId q gid (-1)  -- anyTag

        Q.getRxSlotSize q >>= \o -> putStrLn $ "slot_size: " ++ show o
        recvLoop q

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0   -> error "usage: test-read dev"
        _   -> dumper (head args)


