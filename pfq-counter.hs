import PFq as Q
import Foreign
import System.Environment
import Numeric

import Control.Monad
import Control.Exception
import Control.Concurrent

import Data.Maybe


recvLoop :: (Num a) => Ptr PFqTag -> MVar a -> IO Int
recvLoop q counter = do 
    netQueue <- Q.read q 10000
    if ( Q.queueLen netQueue == 0 ) 
       then recvLoop q counter 
       else do
            c <- takeMVar counter
            putMVar counter (c + fromIntegral (Q.queueLen netQueue))           
            recvLoop q counter



dumper :: String -> IO ()
dumper dev = do
        putStrLn  $ "dumping " ++ dev  ++ "..."
        -- Q.steeringFunction q gid "steer-ipv4-addr"
       
        c0 <- newMVar 0
        c1 <- newMVar 0
        c2 <- newMVar 0
        c3 <- newMVar 0
        c4 <- newMVar 0
        c5 <- newMVar 0
        c6 <- newMVar 0
        c7 <- newMVar 0
        c8 <- newMVar 0
        c9 <- newMVar 0
        c10 <- newMVar 0
        c11 <- newMVar 0
        
        forkOn 0 (
                    do
                    fp <- Q.openNoGroup 46 14 131000
                    withForeignPtr fp  $ \q -> do
                        Q.joinGroup q 42 1 3
                        Q.bindGroup q 42 dev (-1)
                        Q.enable q 
                        -- Q.steeringFunction q 42 "steer-ipv4-addr"
                        recvLoop q c0 >> return ()  
                 )

        forkOn 1 (    
                    do
                    fp <- Q.openNoGroup 46 14 131000
                    withForeignPtr fp  $ \q -> do
                        Q.joinGroup q 42 1 3
                        Q.bindGroup q 42 dev (-1)
                        Q.enable q 
                        recvLoop q c1 >> return ()  
                 )

        forkOn 2 (
                    do
                    fp <- Q.openNoGroup 46 14 131000
                    withForeignPtr fp  $ \q -> do
                        Q.joinGroup q 42 1 3
                        Q.bindGroup q 42 dev (-1)
                        Q.enable q 
                        recvLoop q c2 >> return ()  
                 )

        forkOn 3 (    
                    do
                    fp <- Q.openNoGroup 46 14 131000
                    withForeignPtr fp  $ \q -> do
                        Q.joinGroup q 42 1 3
                        Q.bindGroup q 42 dev (-1)
                        Q.enable q 
                        recvLoop q c3 >> return ()  
                 )

        forkOn 4 (
                    do
                    fp <- Q.openNoGroup 46 14 131000
                    withForeignPtr fp  $ \q -> do
                        Q.joinGroup q 42 1 3
                        Q.bindGroup q 42 dev (-1)
                        Q.enable q 
                        recvLoop q c4 >> return ()  
                 )

        forkOn 5 (    
                    do
                    fp <- Q.openNoGroup 46 14 131000
                    withForeignPtr fp  $ \q -> do
                        Q.joinGroup q 42 1 3
                        Q.bindGroup q 42 dev (-1)
                        Q.enable q 
                        recvLoop q c5 >> return ()  
                 )

        forkOn 6 (
                    do
                    fp <- Q.openNoGroup 46 14 131000
                    withForeignPtr fp  $ \q -> do
                        Q.joinGroup q 42 1 3
                        Q.bindGroup q 42 dev (-1)
                        Q.enable q 
                        recvLoop q c6 >> return ()  
                 )

        forkOn 7 (    
                    do
                    fp <- Q.openNoGroup 46 14 131000
                    withForeignPtr fp  $ \q -> do
                        Q.joinGroup q 42 1 3
                        Q.bindGroup q 42 dev (-1)
                        Q.enable q 
                        recvLoop q c7 >> return ()  
                 )
        
        forkOn 8 (
                    do
                    fp <- Q.openNoGroup 46 14 131000
                    withForeignPtr fp  $ \q -> do
                        Q.joinGroup q 42 1 3
                        Q.bindGroup q 42 dev (-1)
                        Q.enable q 
                        recvLoop q c8 >> return ()  
                 )

        forkOn 9 (    
                    do
                    fp <- Q.openNoGroup 46 14 131000
                    withForeignPtr fp  $ \q -> do
                        Q.joinGroup q 42 1 3
                        Q.bindGroup q 42 dev (-1)
                        Q.enable q 
                        recvLoop q c9 >> return ()  
                 )

        forkOn 10 (
                    do
                    fp <- Q.openNoGroup 46 14 131000
                    withForeignPtr fp  $ \q -> do
                        Q.joinGroup q 42 1 3
                        Q.bindGroup q 42 dev (-1)
                        Q.enable q 
                        recvLoop q c10 >> return ()  
                 )

        forkOn 11 (    
                    do
                    fp <- Q.openNoGroup 46 14 131000
                    withForeignPtr fp  $ \q -> do
                        Q.joinGroup q 42 1 3
                        Q.bindGroup q 42 dev (-1)
                        Q.enable q 
                        recvLoop q c11 >> return ()  
                 )

        dumpStat [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11]  


dumpStat :: (Show a, Num a) => [MVar a] -> IO ()
dumpStat cs = do
             threadDelay 1000000
             t <- mapM (\v -> swapMVar v 0) cs
             putStrLn $ "pkt/sec: " ++ show (sum t)
             dumpStat cs

main = do
    dev <- liftM (!!0) getArgs
    dumper dev
