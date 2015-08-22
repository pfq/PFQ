module PFQDaemon where

import Config

import Network.PFq.Lang
import Network.PFq.Default
import Network.PFq.Experimental

config =
    [
        Group
        { gid       = 1
        , input     = [ dev "eth0.1" ]
        , output    = [ dev "eth2" .^ 1, dev "eth3" .^ 2 ]
        , function  = ip >-> steer_flow
        }
    ]
