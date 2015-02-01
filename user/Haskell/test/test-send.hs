--
--
--  (C) 2011-14 Nicola Bonelli <nicola@pfq.io>
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

sendSync :: Ptr Q.PFqTag -> Int -> IO Int
sendSync q n  =
    Q.send q ping >>= (\b -> if b then return (n+1)
                                  else return n)


sendAsync :: Ptr Q.PFqTag -> Int -> IO Int
sendAsync q n =
    Q.sendAsync q ping 128 >>= (\b -> if b then return (n+1)
                                           else return n)


while :: (a -> Bool) -> (a -> IO a) -> a -> IO a
while pred fun x
    | pred x   = do
        y <- fun x
        while pred fun y
    | otherwise = return x


sender :: [String] -> IO ()
sender xs = do

    let dev    = head xs
    let queue  = read (xs !! 1) :: Int
    let core   = read (xs !! 2) :: Int
    let num    = read (xs !! 3) :: Int

    fp <- Q.open' 64 1024 1024

    withForeignPtr fp  $ \q -> do
            Q.enable q
            Q.bindTxOnCpu q dev queue core

            if core /= -1
            then do
                putStrLn  $ "sending " ++ show num ++ " packets to dev " ++ dev  ++ " (async)..."
                Q.txAsync q True
                while (< num) (sendAsync q) 0
            else do
                putStrLn  $ "sending " ++ show num ++ " packets to dev " ++ dev  ++ "..."
                while (< num) (sendSync q) 0

            threadDelay 2000000

            stat <- Q.getStats q
            print stat

            Q.close q

main :: IO ()
main = do
    args <- getArgs
    case ()  of
        _ | length args < 4 -> error "usage: test-send dev queue node num"
          | otherwise       -> sender args

