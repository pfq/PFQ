import PFq as Q
import Foreign
import System.Environment
import Numeric

import Control.Monad
import Control.Exception
import Control.Concurrent

import Data.Maybe

recvLoop :: Ptr PFqTag -> MVar a -> Int -> IO Int
recvLoop q stop tot = do 
    netQueue <- Q.read q 10000
    if ( Q.queueLen netQueue == 0 ) 
       then do
            c <- tryTakeMVar stop
            if (isNothing c)
            then recvLoop q stop tot
            else return tot    
       else do
            -- ps <- Q.getPackets netQueue
            -- mapM_ (print . fst) ps
            c <- tryTakeMVar stop
            if (isNothing c)
            then recvLoop q stop (tot + (fromIntegral $ Q.queueLen netQueue))
            else return tot    

dumper :: String -> IO ()
dumper dev = do
    putStrLn  $ "dumping " ++ dev  ++ "..."
    fp <- Q.open 64 0 4096
    withForeignPtr fp  $ \q -> do
        Q.setTimestamp q True
        Q.bind q dev (-1)
        Q.enable q 
        -- Q.steeringFunction q gid "steer-ipv4-addr"
        Q.getSlotSize q >>= \o -> putStrLn $ "slot_size: " ++ show(o)
       
        stop <- newEmptyMVar
        end  <- newEmptyMVar

        forkOS (    do
                    tot <- recvLoop q stop 0 
                    putStrLn $ "packets: " ++ show (tot) 
                    putMVar end True 
               )

        threadDelay 10000000
        putMVar stop True
        takeMVar end
        return ()

main = do
    dev <- liftM (!!0) getArgs
    dumper dev
