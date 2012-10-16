import PFq as Q
import Foreign
import System.Environment
import System.Time
import Numeric

import Control.Monad
import Control.Exception
import Control.Concurrent

recvLoop :: (Num a) => Ptr PFqTag -> MVar a -> IO Int
recvLoop q counter = do 
    netQueue <- Q.read q 10000
    if ( Q.queueLen netQueue == 0 ) 
       then recvLoop q counter 
       else do
            modifyMVar_ counter $ \c -> return (c + fromIntegral (Q.queueLen netQueue))
            -- c <- takeMVar counter
            -- putMVar counter (c + fromIntegral (Q.queueLen netQueue))           
            recvLoop q counter


launcher :: (Num a) => String -> Int -> IO [MVar a]
launcher _ 0 = return []
launcher dev n  = do 

         c <- newMVar 0
         forkOn (n-1) (
                    do
                    fp <- Q.openNoGroup 46 14 131000
                    withForeignPtr fp  $ \q -> do
                        Q.joinGroup q 42 1 3
                        Q.bindGroup q 42 dev (-1)
                        Q.enable q 
                        -- Q.steeringFunction q 42 "steer-ipv4-addr"
                        recvLoop q c >> return ()  
                  )
         putStrLn $ "#" ++ show (n-1) ++ " started!"
         liftM2 (:) (return c) (launcher dev (n-1))
                         

dumper :: String -> Int -> IO ()
dumper dev n = do
        cs <- launcher dev n
        t <- getClockTime
        dumpStat cs t


diffUSec :: ClockTime -> ClockTime -> Int
diffUSec t1 t0 = (tdSec delta * 1000000) + truncate (fromIntegral(tdPicosec delta) / 1000000)
                    where delta = diffClockTimes t1 t0


dumpStat :: (Show a, Fractional a) => [MVar a] -> ClockTime -> IO ()
dumpStat cs t0 = do
             threadDelay 1000000
             t <- getClockTime
             cs' <- mapM (\v -> swapMVar v 0) cs
             let delta = diffUSec t t0
             let rate = (sum cs' * 1000000) / fromIntegral delta  
             putStrLn $ "duration: " ++ show delta ++ " pkt/sec: " ++ show rate
             dumpStat cs t

main = do
    args <- getArgs
    if (length args < 2)
    then error "usage: pfq-counter dev #thread"
    else do 
        dumper (args !! 0) (Prelude.read(args !! 1) :: Int)



