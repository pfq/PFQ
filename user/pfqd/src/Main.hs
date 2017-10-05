--
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


{-# LANGUAGE TupleSections #-}

import Control.Concurrent
import Control.Monad
import Control.Exception as E

import Data.Maybe

import System.Log.Logger
import qualified System.Log.Handler as SLH
import System.Log.Handler.Syslog

import System.Process
import System.Posix.Daemon
import System.Posix.Types
import System.Posix.Process(getProcessID)

import System.Environment
import System.Console.CmdArgs
import System.Directory
import System.IO.Error

import Network.PFQ as Q
import Network.PFQ.Lang

import Foreign.Marshal.Utils
import Foreign.ForeignPtr
import Foreign.Ptr

import PFQDaemon
import Options
import Daemon
import Config


main :: IO ()
main = do

    -- read command-line options

    opts  <- cmdArgsRun options

    -- force to call help in case config_file is not specified

    when (null $ config_file opts) $ withArgs ["--help"] $ void (cmdArgsRun options)

    -- open log...

    s <- openlog "pfqd" [PID] USER DEBUG

    updateGlobalLogger "daemon" (addHandler s)
    updateGlobalLogger "daemon" (setLevel DEBUG)

    -- getting workdir...

    workdir <- getAppUserDataDirectory "pfqd"
    setCurrentDirectory workdir

    -- log its pid...

    ps <- pidof =<< getProgName

    when (length ps > 1) $
        getProcessID >>= \me ->
            error $ "error: another session is running with pid " ++ show (head $ filter (/= me) ps)

    -- rebuild itself

    if dont_rebuild opts
        then  do
            unless (null config) $ infoM "daemon" $ "Loading configuration for " ++ show (length config) ++ " groups:"
            forM_ config (\(Group pol gid devs _ comp) -> infoM "daemon" ("    PFQ group " ++ show gid ++ ": " ++ pretty comp ))
        else  infoM "daemon" "PFQd started!" >> rebuildRestart opts (SLH.close s)

    -- run daemon...

    let negrs = countEgress config
    infoM "daemon" ("Total number of egress port: " ++ show negrs)

    runDetached Nothing DevNull $
        (Q.openNoGroup 1520 8192 1520 8192 >>= \hq ->
            withPfq hq $ \ctrl -> do
            fps <- replicateM (countEgress config) (Q.openNoGroup 1520 8192 1520 8192)
            withMany withPfq fps $ \egrs -> do
                    runQSetup opts ctrl egrs
                    foreverDaemon opts (SLH.close s >> Q.close ctrl >> mapM_ Q.close egrs))
                `E.catch` (\e -> errorM "daemon" (show (e :: SomeException)) >> foreverDaemon opts (SLH.close s))


countEgress :: [Group] -> Int
countEgress gs = sum $ map (\Group{ output = out } -> length out) gs


bindInput :: PfqHandlePtr -> Int -> NetDevice ->  IO ()
bindInput q gid (NetDevice d hq _ _) =
    Q.bindGroup q gid d hq


bindOutput :: PfqHandlePtr -> (Int, Policy, NetDevice) ->  IO ()
bindOutput q (gid, pol, NetDevice d hq w cl) = bindEgress q gid d hq
    where bindEgress q gid dev queue = do
            infoM "daemon" ("    egress bind on dev " ++ dev ++ ", port " ++ show queue ++ ", class " ++ show cl)
            Q.joinGroup q gid cl (mkPolicy pol)
            Q.egressBind q dev queue
            Q.setWeight q w


runQSetup :: Options -> PfqHandlePtr -> [PfqHandlePtr] -> IO ()
runQSetup opts ctrl egrs = do
    infoM "daemon" $ "Running daemon with " ++ show opts
    infoM "daemon" $ "Loading new configuration for " ++ show (length config) ++ " group(s)..."
    let egrs' = zip egrs (concatMap (\Group {policy = pol, output = out, gid = gid} ->  map (gid,pol,) out) config)
    infoM "daemon" $ "Setting up egress port: " ++ show egrs'
    mapM_ (uncurry bindOutput) egrs'
    forM_ config $ \(Group pol g ins _ comp) -> do
        let gid = fromIntegral g
        infoM "daemon" $ "Setting up group " ++ show gid ++ " for dev " ++ show ins ++ ". Computation: " ++ pretty comp
        Q.joinGroup ctrl gid class_control (mkPolicy pol)
        Q.setGroupComputation ctrl gid comp
        forM_ ins $ \dev -> bindInput ctrl gid dev


mkPolicy :: Policy -> Q.GroupPolicy
mkPolicy Shared     = Q.policy_shared
mkPolicy Restricted = Q.policy_restricted


pidof :: String -> IO [ProcessID]
pidof name = (map Prelude.read . words) <$> catchIOError (readProcess "/bin/pidof" [name] "") (const $ return [])

