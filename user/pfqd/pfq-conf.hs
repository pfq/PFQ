module PFQconf where

import Network.PFq.Lang
import Network.PFq.Default
import Network.PFq.Experimental

pfq_config = [] :: [(Integer, Function (SkBuff -> Action SkBuff))]
