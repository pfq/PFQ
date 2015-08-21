module Config where

import Network.PFq.Lang

-- |NetDevice data type.

data NetDevice = Dev String | DevQueue String Int
                    deriving (Eq, Show, Read)

-- | Group data type.

data Group = Group
            {   gid       :: Int
            ,   input     :: [NetDevice]
            ,   output    :: [NetDevice]
            ,   function  :: Function (SkBuff -> Action SkBuff)
            }
