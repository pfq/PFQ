--  (C) 2011-16 Nicola Bonelli <nicola@pfq.io>
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software Foundation,
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  The full GNU General Public License is included in this distribution in
--  the file called "COPYING".

{-# LANGUAGE OverloadedStrings #-}

import Network.PFQ.Lang
import Network.PFQ.Types
import Network.PFQ.Lang.Default as Q
import Network.PFQ.Lang.Experimental as Q

import Data.Aeson
import Data.Maybe

import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad

-- prettyPrinter (debug):

prettyPrinter :: (Show a) => [a] -> IO ()
prettyPrinter xs = forM_ (zip [0..] xs) $ \(n, x) -> putStrLn $ "    " ++ show n ++ ": " ++ show x

main = do
        let na1 = CIDR ("10.10.0.0",16)
        let na2 = "192.168.0.1/24" :: CIDR
        print na1
        print na2

        putStrLn "\nSerialize CIDR:"
        let s1 = encode na1
        BL.putStrLn s1

        let d1 = decode s1 :: Maybe CIDR
        putStrLn "\nDeserialized CIDR:"
        print $ fromJust d1

        let mycond = is_ip .&&. (is_tcp .||. is_udp)
        let mycond1 = is_udp

        let comp = par (ip >-> udp) (ip >-> tcp) >-> steer_rtp >->
                         Q.when is_tcp (inc 2) >->
                         addr "192.168.0.0/24" >->
                         steer_local_link "4c:60:de:86:55:46" >->
                         dummy 42 >->
                         dummy_string "hello world" >->
                         dummy_strings ["hello", "world"] >->
                         dummy_vector [1,2,3]

        putStrLn "\nFunctional computation (show):"
        print comp
        putStrLn "\nFunctional computation (prettyPrint):"
        putStrLn $ pretty comp
        putStrLn "\nSerialized AST:"

        let (ser, _) = serialize comp 0

        prettyPrinter ser

        putStrLn "\nSerialized JSON:"

        let json = encode ser
        BL.putStrLn json
        let ser2 = decode json :: Maybe [FunctionDescr]

        putStrLn "\nDeserialized: JSON -> AST"
        prettyPrinter $ fromJust ser2


