module Main where

import Network.PFq as Q
import Foreign
import System.Environment

handler :: Q.Callback
handler h _ = print h

recvDispatch :: Ptr PFqTag -> IO()
recvDispatch q = do 
        Q.dispatch q handler 1000 
        recvDispatch q

dumper :: String -> IO ()
dumper dev = do
    putStrLn  $ "dumping " ++ dev  ++ "..."
    fp <- Q.open 64 14 4096
    withForeignPtr fp  $ \q -> do
        Q.setTimestamp q True
        gid <- Q.getGroupId q
        Q.bindGroup q gid dev (-1)
        Q.enable q 
        Q.groupFunction q gid 0 "steer-ipv4"
        Q.getSlotSize q >>= \o -> putStrLn $ "slot_size: " ++ show o
        recvDispatch q

main :: IO ()
main = do    
    args <- getArgs
    case length args of
        0   -> error "usage: pfq-dispatch dev"
        _   -> dumper (head args)
