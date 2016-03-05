-- example
--

import Network.PFQ.Lang.Default

import Lib.Filter

main =
    steer_flow >->
      when is_tcp drop >-> kernel

