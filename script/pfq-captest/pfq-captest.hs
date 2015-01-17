--
-- Copyright (c) 2015 Bonelli Nicola <nicola.bonelli@antifork.org>
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

module Main where

import System.Console.ANSI
import System.Console.CmdArgs
import System.Process
import System.Exit

import Control.Monad
import Control.Concurrent (threadDelay)

import Data.Data
import Data.List (intercalate)


pfq_counters = "/usr/local/bin/pfq-counters"
pfq_load = "/root/.cabal/bin/pfq-load"

version = "4.0"

pfqOptions = [ (rss, [mkOption "direct_capture" dcap,
                      mkOption "capture_incoming" icap,
                      mkOption "capture_outgoing" ocap,
                      mkOption "batch_len" blen]) |

                      rss  <- [1,2],
                      dcap <- [0,1],
                      icap <- [0,1],
                      ocap <- [0],
                      blen <- [1, 16],
                      dcap /= 0 || icap /= 0]


bold    = setSGRCode [SetConsoleIntensity BoldIntensity]
reset   = setSGRCode []


mkOption :: (Show a) => String -> a -> String
mkOption opt x = opt ++ "=" ++ show x


runSystem :: String -> String -> IO ()
runSystem cmd errmsg = do
    putStrLn $ "-> " ++ cmd
    system cmd >>= \ec -> when (ec /= ExitSuccess) $ error errmsg

data Options = Options
    {
         core       :: Int,
         gid        :: Int,
         seconds    :: Int,
         device     :: [String]
    } deriving (Show, Read, Data, Typeable)


options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {
         core       = 0             &= typ "NUM" &= help "core where to run the tests",
         gid        = 0             &= typ "NUM" &= help "PFQ group id of the tests",
         seconds    = 5             &= typ "NUM" &= help "Duration of each test in seconds",
         device     = []            &= help "List of devices where to run the tests"
    } &= summary ("pfq-tOest " ++ version) &= program "pfq-test"



main :: IO ()
main = do
    opts <- cmdArgsRun options
    when (null $ device opts) $ error "At least a device must be specified"

    putStrLn $ bold ++ "[PFQ] running regression tests..." ++ reset

    forM_ (zip [0..] pfqOptions) $ \(n, opt) -> do
        putStrLn $ bold ++ "\n### Test #" ++ show n ++ reset ++ ": " ++ show opt
        runSystem (pfq_load ++ " -q " ++ show (fst opt) ++ " " ++ unwords (snd opt))
            "Could not load PFQ module!"
        runSystem (pfq_counters ++ " --seconds " ++ show (seconds opts) ++
            " -t " ++ show(core opts) ++ "." ++ show(gid opts) ++ "." ++ intercalate ":" (device opts))
            "Could not run test correctly!"

    putStrLn $ bold ++ "[PFQ] Done." ++ reset

