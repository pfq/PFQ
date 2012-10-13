import PFq 

import Foreign

recv :: Ptr PFqTag -> IO ()
recv q = do 
    queue <- PFq.readQ q 10000
    if ( PFq.queueLen(queue) == 0 ) then recv q else (print queue >> recv q)

xxx :: IO ()
xxx = do
    fp <- PFq.open 64 0 4096
    withForeignPtr fp  $ \q -> do
        PFq.enable q 
        PFq.bind q "eth0" (-1)
        recv q
    return ()

main = do
    xxx >> (print "done")
