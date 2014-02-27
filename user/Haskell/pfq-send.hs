--
--
--  (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>
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

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Network.PFq as Q
import Foreign
import System.Environment

import Control.Concurrent
import Control.Monad

-- import Foreign.C.Types


import qualified Data.ByteString as C

ping = C.pack [
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xf0, 0xbf, -- L`..UF..
        0x97, 0xe2, 0xff, 0xae, 0x08, 0x00, 0x45, 0x00, -- ......E.
        0x00, 0x54, 0xb3, 0xf9, 0x40, 0x00, 0x40, 0x01, -- .T..@.@.
        0xf5, 0x32, 0xc0, 0xa8, 0x00, 0x02, 0xad, 0xc2, -- .2......
        0x23, 0x10, 0x08, 0x00, 0xf2, 0xea, 0x42, 0x04, -- #.....B.
        0x00, 0x01, 0xfe, 0xeb, 0xfc, 0x52, 0x00, 0x00, -- .....R..
        0x00, 0x00, 0x06, 0xfe, 0x02, 0x00, 0x00, 0x00, -- ........
        0x00, 0x00, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, -- ........
        0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, -- ........
        0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, -- .. !"#$%
        0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, -- &'()*+,-
        0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, -- ./012345
        0x36, 0x37                                      -- 67
        ]

-- import Debug.Trace

sender :: [String] -> IO ()
sender []       = undefined
sender (dev:xs) = do

    let numb = read (head xs) :: Int

    fp <- Q.open 128 0 1024

    putStrLn  $ "sending " ++ show numb ++ " packets to dev " ++ dev  ++ "..."

    withForeignPtr fp  $ \q -> do
            Q.enable q
            Q.bindTx q dev (-1)

            Q.startTxThread q 0

            replicateM_ numb $ do
               Q.send_async q ping
               threadDelay 1

            Q.wakeupTxThread q

            threadDelay 1000000

            Q.stopTxThread q

            stat <- Q.getStats q
            print $ stat

            Q.close q

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0  -> error "usage: pfq-send dev numb"
        1  -> error "usage: pfq-send dev numb"
        _  -> sender args

