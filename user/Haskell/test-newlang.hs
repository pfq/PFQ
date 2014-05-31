import Network.PFq.Lang
import Network.PFq.Default

import Data.Typeable

main = do
        let p = ip >-> tcp >-> (conditional is_ip drop' steer_ip ) >-> class' 10

        putStrLn $ pretty p
        putStrLn $ show p
        putStrLn $ show $ typeOf(port)
        putStrLn $ typeOf'(port)
