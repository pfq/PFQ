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

import Network.PFq.Default
import Options
import PFQconf

import Network.PFq as Q


daemon :: Options -> IO () -> IO ()
daemon opts callback = forever $ do
    (src, dst) <- getConfigFiles opts
    eq <- equalFile src dst
    unless eq $ rebuildRestart opts callback
    threadDelay 1000000


rebuildRestart :: Options -> IO () -> IO ()
rebuildRestart opts action = do
   infoM "daemon" "Configuration updated. Rebuilding..."
   (src, dst) <- getConfigFiles opts
   copyFile src dst
   runCompiler >>= \(ec,_,msg) -> if ec == ExitSuccess
       then action >> infoM "daemon" "Done. Restarting..." >> executeFile "pfqd" False ["-c" , src, "-d"] Nothing
       else mapM_ (errorM "daemon") (lines $ replace "PFQconf.hs" (config_file opts) msg)


getConfigFiles :: Options -> IO (FilePath, FilePath)
getConfigFiles opts = getAppUserDataDirectory "pfqd" >>=
    \udata -> let src = config_file opts
                  dst = udata </> "PFQconf.hs" in return (src, dst)


equalFile :: FilePath -> FilePath -> IO Bool
equalFile a b = liftM2 (==) (readFile a) (readFile b)


replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old


runCompiler :: IO (ExitCode, String, String)
runCompiler = readProcessWithExitCode "ghc" ["--make", "Main", "-o", "pfqd", "-lpfq"] ""


