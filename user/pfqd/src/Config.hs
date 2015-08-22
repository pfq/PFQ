module Config where

import qualified Network.PFq as Q
import Network.PFq.Lang
import Data.List
import Data.List.Split

-- |NetDevice data type.

data NetDevice =  NetDevice
                  { devName   :: String
                  , devQueue  :: Int
                  , devWeight :: Int
                  }
                  deriving (Eq, Show, Read)


dev :: String -> NetDevice
dev str
    | "." `isInfixOf` str =  let [d,q] = take 2 (splitOn "." str) in NetDevice d (read q) 1
    | otherwise           =  NetDevice str (Q.getConstant Q.any_queue) 1


(.^) :: NetDevice -> Int -> NetDevice
(NetDevice n q _) .^ w = NetDevice n q w


-- | Group data type.

data Group = Group
            {   gid       :: Int
            ,   input     :: [NetDevice]
            ,   output    :: [NetDevice]
            ,   function  :: Function (SkBuff -> Action SkBuff)
            }
