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
                  , devClass  :: Q.ClassMask
                  }
                  deriving (Eq, Show, Read)


dev :: String -> NetDevice
dev str
    | "." `isInfixOf` str =  let [d,q] = take 2 (splitOn "." str) in NetDevice d (read q) 1 Q.class_default
    | otherwise           =  NetDevice str (Q.getConstant Q.any_queue) 1 Q.class_default


(.^) :: NetDevice -> Int -> NetDevice
(NetDevice n q _ cl) .^ w = NetDevice n q w cl

(.&) :: NetDevice -> Q.ClassMask -> NetDevice
(NetDevice n q w _) .& cl = NetDevice n q w cl


-- | Group policy

data Policy = Restricted | Shared
                deriving (Show, Read)

-- | Group data type.

data Group = Group
            {   policy    :: Policy
            ,   gid       :: Int
            ,   input     :: [NetDevice]
            ,   output    :: [NetDevice]
            ,   function  :: Function (SkBuff -> Action SkBuff)
            }
