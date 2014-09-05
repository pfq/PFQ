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

import Control.Concurrent
import Control.Monad
import Control.Exception as E

import Data.Default
import Data.Maybe

import System.Log.Logger
import System.Log.Handler.Syslog
import System.Posix.Daemon
import System.Environment
import System.Console.CmdArgs
import System.Directory

import Network.PFq as Q
import Network.PFq.Lang

import Foreign.ForeignPtr
import Foreign.Ptr

import Options
import PFQconf
import Daemon


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

    workdir <-getAppUserDataDirectory "pfqd"
    setCurrentDirectory workdir

    -- rebuild itself

    if dont_rebuild opts
        then  do
            unless (null pfq_config) $ infoM "daemon" $ "Loading configuration for " ++ show (length pfq_config) ++ " groups:"
            forM_ pfq_config (\(gid,comp) -> infoM "daemon" ("    PFQ group " ++ show gid ++ ": " ++ pretty comp ))
        else  infoM "daemon" "PFQd started!" >> rebuildRestart opts (return ())

    -- run daemon...

    infoM "daemon" "Running daemon..."

    runDetached Nothing DevNull $
        (Q.openDefault >>= \fp ->
            withForeignPtr fp $ \q -> runQSetup opts q >> daemon opts (Q.close q))
            `E.catch` (\e -> errorM "daemon" (show (e :: SomeException)) >> daemon opts (return()))


runQSetup :: Options -> Ptr PFqTag -> IO ()
runQSetup opts q =
    forM_ pfq_config $ \(g, comp) -> do
        let gid = fromIntegral g
        Q.joinGroup q gid [class_control] policy_shared
        Q.groupComputation q gid comp


