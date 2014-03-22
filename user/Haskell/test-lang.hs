import Control.Monad
import Network.PFqLang

main = do
        let comp = ip >-> udp >-> dummy 42
        print comp
        print $ eval comp

