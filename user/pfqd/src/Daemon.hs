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

module Daemon where

import Control.Concurrent
import Control.Monad
import Data.List
import Data.List.Split

import System.Log.Logger
import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.Posix.Process
import System.Exit

import Foreign.ForeignPtr
import Foreign.Ptr

import Network.PFQ as Q
import Network.PFQ.Lang.Default

import Options
import Config
import PFQDaemon



foreverDaemon :: Options -> IO () -> IO ()
foreverDaemon opts closefds = forever $ do
    (src, dst) <- getConfigFiles opts
    new <- newerFile src dst
    when new $ rebuildRestart opts closefds
    threadDelay 1000000


rebuildRestart :: Options -> IO () -> IO ()
rebuildRestart opts closefds = do
   infoM "daemon" "Configuration updated. Rebuilding..."
   (src, dst) <- getConfigFiles opts
   copyFile src dst
   userDir <- getAppUserDataDirectory "pfqd"
   let newDaemon = userDir </> "pfqd"
   runCompiler >>= \(ec,_,msg) -> if ec == ExitSuccess
       then do
            infoM "daemon" ("Done. Restarting " ++ newDaemon ++ "...")
            closefds
            executeFile newDaemon False ["-c" , src, "-d"] Nothing
       else mapM_ (errorM "daemon") (lines $ replace "PFQDaemon.hs" (config_file opts) msg)


getConfigFiles :: Options -> IO (FilePath, FilePath)
getConfigFiles opts = getAppUserDataDirectory "pfqd" >>=
    \udata -> let src = config_file opts
                  dst = udata </> "PFQDaemon.hs" in return (src, dst)


newerFile :: FilePath -> FilePath -> IO Bool
newerFile a b = do
    at <- getModificationTime a
    be <- doesFileExist b
    if not be
        then return True
        else do bt <- getModificationTime b
                return ( at > bt )


replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old


runCompiler :: IO (ExitCode, String, String)
runCompiler = readProcessWithExitCode "ghc" ["--make", "Main", "-o", "pfqd", "-lpfq", "-XOverloadedStrings"] ""


