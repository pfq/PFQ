-- simple

import Network.PFQ.Lang.Default

main =
    steer_flow >->
      when is_tcp drop >-> kernel

