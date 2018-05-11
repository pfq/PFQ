--
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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Network.PFQ as Q

import Foreign
import System.Time
import System.Exit

import Control.Monad as M
import Control.Applicative
import Control.Concurrent
import Control.Exception

import Data.List.Split
import Data.Data
import Data.Maybe
import Data.Atomics.Counter

import qualified Data.Set as S
import qualified Data.Map as M

import System.Console.CmdArgs


newtype State = State { sCounter :: AtomicCounter }


-- Command line options
--
data Options = Options
               {    caplen   :: Int
               ,    slots    :: Int
               ,    function :: Maybe String
               ,    thread   :: [String]
               } deriving (Data, Typeable, Show)


-- default options
--
options = cmdArgsMode $
    Options
    {   caplen   = 64
    ,   slots    = 8192
    ,   function = Nothing &= typ "FUNCTION"  &= help "Where FUNCTION = pfq-lang computation (i.e. main = steer_p2p)"
    ,   thread   = [] &= typ "BINDING" &= help "Where BINDING = core.gid[.[eth0:queue,queue,queue...[.ethx:queue,queue...]]]"
    } &= summary "PFQ multi-threaded packet counter." &= program "pfq-counters"


-- Group Options
--

type Queue = Int
type Gid   = Int


data NetDev = NetDev
    {   devName     :: String
    ,   devQueues   :: [Queue]
    } deriving (Eq, Show, Read)


data Binding = Binding
    {   coreNum   :: Int
    ,   groupId   :: Gid
    ,   netDevs   :: [NetDev]
    } deriving (Eq, Show)


makeBinding :: String -> Binding
makeBinding s = case splitOn "." s of
    []         ->  error "thread_binding: parse error"
    [_]        ->  error "thread_binding: parse error"
    [c, g]     ->  Binding (read c) (read g) []
    c : g : ds ->  Binding (read c) (read g) (map (\s -> let (dn : qs) = splitOn ":" s
                                                         in  NetDev dn (case qs of [] -> [-1]; _ -> map read (splitOn "," (head qs)))) ds)


-- main function
--

main :: IO ()
main = do
    opt <- cmdArgsRun options
    putStrLn $ "[pfq] " ++ show opt
    cs  <- runThreads opt
    t   <- getClockTime
    dumpStat cs t


dumpStat :: [AtomicCounter] -> ClockTime -> IO ()
dumpStat cs t0 = do
    threadDelay 1000000
    t <- getClockTime
    cs' <- mapM (\a -> do
                    x <- readCounter a
                    writeCounter a 0
                    return x ) cs
    M.void( when ((-1) `elem` cs') exitFailure)
    let delta = diffUSec t t0
    let rate = fromIntegral (sum cs' * 1000000) / fromIntegral delta
    putStrLn $ "Total rate pkt/sec: " ++ show (truncate rate :: Integer)
    dumpStat cs t


diffUSec :: ClockTime -> ClockTime -> Int
diffUSec t1 t0 = (tdSec delta * 1000000) + truncate ((fromIntegral(tdPicosec delta) / 1000000) :: Double)
                    where delta = diffClockTimes t1 t0


runThreads :: Options -> IO [AtomicCounter]
runThreads Options{..} =
    forM thread $ \tb -> do
        let binding = makeBinding tb
        c <- newCounter 0
        _ <- forkOn (coreNum binding) $
                 handle ((\e -> M.void (putStrLn ("[pfq] Exception: " ++ show e))) :: SomeException -> IO ()) $ do
                 hq <- Q.openNoGroup caplen slots caplen 1024
                 Q.withPfq hq $ \q -> do
                     Q.joinGroup q (groupId binding) Q.class_default Q.policy_shared
                     forM_ (netDevs binding) $ \dev ->
                       forM_ (devQueues dev) $ \queue -> do
                         Q.setPromisc q (devName dev) True
                         Q.bindGroup q (groupId binding) (devName dev) queue
                         when (isJust function) $ do
                             putStrLn $ "[pfq] Gid " ++ show (groupId binding) ++ " is using computation: " ++ fromJust function
                             Q.setGroupComputationFromString q (groupId binding) (fromJust function)
                     Q.enable q
                     M.void (recvLoop q (State c))
        putStrLn $ "[pfq] " ++ show binding ++ " @core " ++ show (coreNum binding) ++ " started!"
        return c


recvLoop :: Q.PfqHandlePtr -> State -> IO Int
recvLoop q state = do
    netQueue <- Q.read q 20000
    incrCounter (fromIntegral (Q.qLen netQueue)) (sCounter state)
    recvLoop q state

