import Network.PFq.Lang
import Network.PFq.Default
import Control.Monad

import Data.Typeable

prettyPrinter :: Serializable a => a -> IO ()
prettyPrinter comp = let (xs,_) = serialize 0 comp
                   in forM_ (zip [0..] xs) $ \(n, x) -> putStrLn $ "    " ++ show n ++ ": " ++ show x

main = do
        -- let p = ip >-> tcp >-> (conditional is_ip drop' steer_ip ) >-> class' 10
        -- putStrLn $ prettyPrint p
        -- putStrLn $ show p
        -- putStrLn $ show $ typeOf(port)
        -- putStrLn $ typeOf'(port)

        let p = (conditional is_ip (drop' >-> unit) (steer_ip)) >-> ip

        prettyPrinter p
