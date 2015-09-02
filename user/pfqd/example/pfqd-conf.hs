module PFQDaemon where

import Config

import Network.PFq.Lang
import Network.PFq.Default
import Network.PFq.Experimental

config =
    [
        Group
        { policy    = Restricted
        , gid       = 1
        , input     = [ dev "eth0.1" ]
        , output    = [ dev "eth2" .& class_control_plane, dev "eth3" .^ 2 .& ClassMask 4 ]
        , function  = ip >-> steer_flow
        }
    ]
