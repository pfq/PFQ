module PFQDaemon where

import Config

import Network.PFq.Lang
import Network.PFq.Default
import Network.PFq.Experimental

config =

    [
        Group
        { gid       = 1
        , input     = [Dev "eth0"]
        , output    = []
        , function  = ip >-> steer_flow
        },

        Group
        { gid       = 2
        , input     = [Dev "eth0", DevQueue "eth1" 1]
        , output    = [ ]
        , function  = icmp
        }

    ]
