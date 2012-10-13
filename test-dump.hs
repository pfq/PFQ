import PFq 
import Foreign
import System.Environment

import Control.Monad

recv :: Ptr PFqTag -> IO ()
recv q = do 
    queue <- PFq.readQ q 10000
    if ( PFq.queueLen(queue) == 0 ) 
       then recv q 
       else do
            hs <- PFq.getHeaders queue
            mapM_ print hs 
            -- putStrLn $ "qlen: " ++ show(PFq.queueLen(queue)) ++ " hlen: " ++ show(length hs)
            recv q

dumper :: String -> IO ()
dumper dev = do
    putStrLn  $ "dumping " ++ dev  ++ "..."
    fp <- PFq.open 64 0 4096
    withForeignPtr fp  $ \q -> do
        PFq.enable q
        PFq.toggleTimestamp q True
        PFq.bind q dev (-1)
        recv q

main = do
    dev <- liftM (!!0) getArgs
    dumper dev
