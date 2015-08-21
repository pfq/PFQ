module PFQDaemon where

import Network.PFq.Lang
import Network.PFq.Default
import Network.PFq.Experimental

pfq_config =
    [
        (1, [Dev "eth0"],
            ip >-> steer_flow
        )

        (2, [Dev "eth0", DevQueue "eth1" 1],
            icmp
        )

    ] :: [(Integer, [NetDevice], Function (SkBuff -> Action SkBuff))]
