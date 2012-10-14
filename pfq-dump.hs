import PFq 
import Foreign
import System.Environment

import Control.Monad

handler :: PFq.Callback
handler h _ = print h

recvDispatch :: Ptr PFqTag -> IO()
recvDispatch q = do 
        PFq.dispatch q handler 1000 
        recvDispatch q

recvLoop :: Ptr PFqTag -> IO ()
recvLoop q = do 
    queue <- PFq.readQ q 10000
    if ( PFq.queueLen(queue) == 0 ) 
       then recvLoop q 
       else do
            hs <- PFq.getHeaders queue
            mapM_ print hs
            gid <- PFq.getGroupId q
            PFq.getStats q >>= print
            -- putStrLn $ "qlen: " ++ show(PFq.queueLen(queue)) ++ " hlen: " ++ show(length hs)
            recvLoop q

dumper :: String -> IO ()
dumper dev = do
    putStrLn  $ "dumping " ++ dev  ++ "..."
    fp <- PFq.open 64 14 4096
    withForeignPtr fp  $ \q -> do
        PFq.setTimestamp q True
        --PFq.bind q dev (-1)
        gid <- PFq.getGroupId q
        PFq.bindGroup q gid dev (-1)
        PFq.enable q 
        PFq.steeringFunction q gid "steer-ipv4-addr"
        PFq.getSlotSize q >>= \o -> putStrLn $ "slot_size: " ++ show(o)
        -- recvLoop q
        recvDispatch q

main = do
    dev <- liftM (!!0) getArgs
    dumper dev
