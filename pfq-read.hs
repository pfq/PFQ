module Main where

import Network.PFq as Q
import Foreign
import System.Environment
import Numeric
import Control.Monad

-- import Debug.Trace

dumpPacket :: Q.Packet -> IO ()
dumpPacket p = do
                Q.waitForPacket p
                bytes <- peekByteOff (Q.pData p) 0 :: IO Word64
                putStrLn $ "[" ++ (showHex bytes "") ++ "]"

recvLoop :: Ptr PFqTag -> IO ()
recvLoop q = do 
    queue <- Q.read q 10000
    if ( Q.qLen(queue) == 0 ) 
       then recvLoop q 
       else do
            ps <- Q.getPackets queue
            mapM_ (getHeader >=> print) ps
            mapM_ dumpPacket ps
            -- gid <- Q.getGroupId q
            -- Q.getStats q >>= print
            -- putStrLn $ "qlen: " ++ show(Q.qLen(queue)) ++ " hlen: " ++ show(length hs)
            recvLoop q

dumper :: String -> IO ()
dumper dev = do
    putStrLn  $ "dumping " ++ dev  ++ "..."
    fp <- Q.open 64 0 4096
    withForeignPtr fp  $ \q -> do
        Q.setTimestamp q True
        --Q.bind q dev (-1)
        gid <- Q.getGroupId q
        Q.bindGroup q gid dev (-1)
        Q.enable q 
        Q.steeringFunction q gid "steer-ipv4-addr"
        Q.getSlotSize q >>= \o -> putStrLn $ "slot_size: " ++ show(o)
        recvLoop q

main :: IO ()
main = do
    args <- getArgs
    case (length args) of
        0   -> error "usage: pfq-read dev"
        _   -> dumper (args !! 0)
