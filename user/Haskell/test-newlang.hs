import Network.PFq.Lang
import Network.PFq.Default

main = do
        let p = ip >-> tcp >-> (conditional is_ip drop' steer_ip ) >-> class' 10

        putStrLn $ show p
