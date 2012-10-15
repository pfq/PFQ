import PFq as Q
import Foreign
import System.Environment

import Control.Monad

recvLoop :: Ptr PFqTag -> IO ()
recvLoop q = do 
    queue <- Q.read q 10000
    if ( Q.queueLen(queue) == 0 ) 
       then recvLoop q 
       else do
            hs <- Q.getHeaders queue
            mapM_ print hs
            gid <- Q.getGroupId q
            Q.getStats q >>= print
            -- putStrLn $ "qlen: " ++ show(Q.queueLen(queue)) ++ " hlen: " ++ show(length hs)
            recvLoop q

dumper :: String -> IO ()
dumper dev = do
    putStrLn  $ "dumping " ++ dev  ++ "..."
    fp <- Q.open 64 14 4096
    withForeignPtr fp  $ \q -> do
        Q.setTimestamp q True
        --Q.bind q dev (-1)
        gid <- Q.getGroupId q
        Q.bindGroup q gid dev (-1)
        Q.enable q 
        Q.steeringFunction q gid "steer-ipv4-addr"
        Q.getSlotSize q >>= \o -> putStrLn $ "slot_size: " ++ show(o)
        recvLoop q

main = do
    dev <- liftM (!!0) getArgs
    dumper dev
