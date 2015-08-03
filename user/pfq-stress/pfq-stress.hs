--
-- Copyright (c) 2015 Nicola Bonelli <nicola@pfq.io>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import System.Console.ANSI
import System.Console.CmdArgs
import System.Process
import System.Exit
import System.IO

import Control.Monad
import Control.Concurrent (threadDelay)

import Data.String.Here
import Data.Data
import Data.List (intercalate, (\\))

version, pfq_counters, pfq_load :: String

pfq_counters = "/usr/local/bin/pfq-counters"
pfq_load = "/root/.cabal/bin/pfq-load"
version = "4.5"

pfqOptions :: [ (Int, [String]) ]
pfqOptions = [ (rss, [mkOption "capture_incoming" icap,
                      mkOption "capture_outgoing" ocap,
                      mkOption "skb_pool_size" skbp,
                      mkOption "batch_len" blen]) |
                      rss  <- [1,3],
                      icap <- [0,1],
                      ocap <- [0],
                      blen <- [1, 16],
                      skbp <- [0, 1024]
                      ]


data Options = Options
    {
         core       :: Int,
         gid        :: Int,
         seconds    :: Int,
         device     :: [String],
         dryRun     :: Bool,
         run        :: [Int],
         skip       :: [Int]
    } deriving (Show, Read, Data, Typeable)


options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {
         core       = 0             &= typ "NUM" &= help "core where to run the tests",
         gid        = 0             &= typ "NUM" &= help "PFQ group id of the tests",
         seconds    = 5             &= typ "NUM" &= help "Duration of each test in seconds",
         device     = []            &= help "List of devices where to run the tests",
         run        = []            &= help "List of tests to run",
         skip       = []            &= help "List of tests to skip",
         dryRun     = False         &= help "Don't actually run tests"
    } &= summary ("pfq-tOest " ++ version) &= program "pfq-test"



main :: IO ()
main = do
    opts <- cmdArgsRun options

    let filt = (if null (run opts) then [0..] else run opts) \\ skip opts

    when (null $ device opts) $ error "[PFQ] At least a device must be specified"

    withFile "stress.run" WriteMode $ \h -> do

        sPutStrLn h $ bold ++ "[PFQ] Running regression tests..." ++ reset

        forM_ (zip [0..] pfqOptions) $ \(n, opt) ->

            when (n `elem` (take 1024 filt)) $ do

                    putChar '\n'

                    let banner = [i|${bold}[PFQ] ####### Test #${n}${reset}: ${opt}|]

                    sPutStrLn h banner

                    unless (dryRun opts) $ threadDelay 3000000

                    runSystem [i|${pfq_load} -q ${fst opt} ${unwords (snd opt)}|] "Could not load PFQ module!" ((not . dryRun) opts)

                    runSystem [i|${pfq_counters} --seconds ${seconds opts} -t ${gid opts}.${core opts}.${intercalate ":" (device opts)}|]
                              [i|"Could not run test# ${n} correctly!|] ((not . dryRun) opts)

        sPutStrLn h $ bold ++ "[PFQ] Stress Done." ++ reset


sPutStrLn :: Handle -> String -> IO ()
sPutStrLn h msg =
    putStrLn msg >> hPutStrLn h msg >> hFlush h


bold  = setSGRCode [SetConsoleIntensity BoldIntensity]
reset = setSGRCode []

mkOption :: (Show a) => String -> a -> String
mkOption opt x = opt ++ "=" ++ show x


runSystem :: String -> String -> Bool -> IO ()
runSystem cmd errmsg run = do
    putStrLn $ "-> " ++ cmd
    when run $
        system cmd >>= \ec -> when (ec /= ExitSuccess) $ error errmsg

