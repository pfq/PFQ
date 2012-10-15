import PFq as Q
import Foreign
import System.Environment

import Control.Monad

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
        --Q.bind q dev (-1)
        gid <- Q.getGroupId q
        Q.bindGroup q gid dev (-1)
        Q.enable q 
        Q.steeringFunction q gid "steer-ipv4-addr"
        Q.getSlotSize q >>= \o -> putStrLn $ "slot_size: " ++ show(o)
        recvDispatch q

main = do
    dev <- liftM (!!0) getArgs
    dumper dev
