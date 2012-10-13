import PFq 
import Foreign
import System.Environment

import Control.Monad

recvLoop :: Ptr PFqTag -> IO ()
recvLoop q = do 
    queue <- PFq.readQ q 10000
    if ( PFq.queueLen(queue) == 0 ) 
       then recvLoop q 
       else do
            hs <- PFq.getHeaders queue
            mapM_ print hs 
            -- putStrLn $ "qlen: " ++ show(PFq.queueLen(queue)) ++ " hlen: " ++ show(length hs)
            recvLoop q

dumper :: String -> IO ()
dumper dev = do
    putStrLn  $ "dumping " ++ dev  ++ "..."
    fp <- PFq.open 64 0 4096
    withForeignPtr fp  $ \q -> do
        PFq.setTimestamp q True
        PFq.bind q dev (-1)
        PFq.enable q
        recvLoop q

main = do
    dev <- liftM (!!0) getArgs
    dumper dev
