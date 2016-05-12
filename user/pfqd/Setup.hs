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

import Control.Monad (unless, forM_)
import Distribution.Simple
import Distribution.Simple.Setup(InstallFlags(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.PackageDescription (PackageDescription(..))

import System.Environment
import System.Directory
import System.FilePath.Posix


haskellFiles = [ "Main.hs", "Daemon.hs", "Options.hs", "Config.hs", "PFQDaemon.hs" ]


main = defaultMainWithHooks $
        simpleUserHooks
        {
            postInst = pfqdInstall
        }

pfqdInstall :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
pfqdInstall args _ _ _ = do
    path <- getAppUserDataDirectory "pfqd" >>= mkDirectoryIfNotExist
    putStrLn $ "Installing haskell files in " ++ path
    forM_ haskellFiles $ \file -> copyFile ("src" </> file) (path </> file)
    putStrLn "Done."


mkDirectoryIfNotExist :: FilePath -> IO FilePath
mkDirectoryIfNotExist path = doesDirectoryExist path >>= \b -> unless b (createDirectory path) >> return path


