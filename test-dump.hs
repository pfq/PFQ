import PFq 

xxx :: IO ()
xxx = do
        q <- PFq.open 100 100 100
        return ()

main = do
    xxx >> (print "done")
