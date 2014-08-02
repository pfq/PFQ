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


import System.IO.Unsafe
import System.Process
import System.Directory
import System.FilePath
import Control.Monad(when,unless,liftM,forM,filterM)
import Control.Applicative
import Text.Regex.Posix

import Data.List
import Data.Maybe
import Data.Function(on)

pfq_omatic_ver,pfq_kcompat,proc_cpuinfo :: String
pfq_symvers :: [String]

pfq_omatic_ver  = "3.0"
proc_cpuinfo    = "/proc/cpuinfo"
pfq_kcompat     = "/usr/include/linux/pf_q-kcompat.h"
pfq_symvers     = [ "/lib/modules/" ++ uname_r ++ "/kernel/net/pfq/Module.symvers",
                    home_dir ++ "/PFQ/kernel/Module.symvers",
                    "/opt/PFQ/kernel/Module.symvers"
                  ]


getMostRecentFile :: [FilePath] -> IO (Maybe FilePath)
getMostRecentFile xs = do
    xs' <- filterM doesFileExist xs >>=
             mapM (\f -> liftM (\m -> (f,m)) $ getModificationTime f) >>= \x ->
               return $ sortBy (flip compare `on` snd) x
    return $ if null xs' then Nothing
                         else Just (fst $ head xs')

main :: IO ()
main = do
    putStrLn $ "[PFQ] pfq-omatic: v" ++ pfq_omatic_ver
    sanityCheck
    getRecursiveContents "." [".c"] >>= mapM_ tryPatch
    symver <- getMostRecentFile pfq_symvers
    copyFile (fromJust symver) "Module.symvers"
    let cmd = "make -j" ++ show getNumberOfPhyCores
    putStrLn $ "[PFQ] compiling: " ++ cmd ++ "..."
    _ <- system cmd
    putStrLn "[PFQ] done."


uname_r :: String
uname_r = unsafePerformIO $
    head . lines <$> readProcess "/bin/uname" ["-r"] ""


home_dir :: String
home_dir = unsafePerformIO getHomeDirectory


regexFunCall :: String -> Int -> String
regexFunCall fun n =
    fun ++ "[[:space:]]*" ++ "\\(" ++ args n  ++ "\\)"
        where args 0 = "[[:space:]]*"
              args 1 = "[^,]*"
              args x = "[^,]*," ++ args (x-1)


tryPatch :: FilePath -> IO ()
tryPatch file =
    readFile file >>= \c ->
        when (c =~ (regexFunCall "netif_rx" 1 ++ "|" ++ regexFunCall "netif_receive_skb" 1 ++ "|" ++ regexFunCall "napi_gro_receive" 2)) $
            doesFileExist (file ++ ".orig") >>= \orig ->
                if orig
                then putStrLn $ "[PFQ] " ++ file ++ " is already patched :)"
                else makePatch file


makePatch :: FilePath -> IO ()
makePatch file = do
    putStrLn $ "[PFQ] patching " ++ file
    src <- readFile file
    renameFile file $ file ++ ".orig"
    writeFile file $ "#include " ++ show pfq_kcompat ++ "\n" ++ src


sanityCheck :: IO ()
sanityCheck = do
    doesFileExist pfq_kcompat >>= \kc ->
        unless kc $ error "error: could not locate pfq-kcompat header!"
    symver <- getMostRecentFile pfq_symvers
    unless (isJust symver) $ error "error: could not locate pfq Module.symvers!"
    putStrLn $ "[PFQ] using " ++ fromJust symver ++ " file (most recent)"
    doesFileExist "Makefile"  >>= \mf ->
        unless mf $ error "error: Makefile not found!"


type Ext = String

getRecursiveContents :: FilePath -> [Ext] -> IO [FilePath]
getRecursiveContents topdir ext = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \fname -> do
    let path = topdir </> fname
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path ext
      else return [path | takeExtensions path `elem` ext]
  return (concat paths)


getNumberOfPhyCores :: Int
getNumberOfPhyCores = unsafePerformIO $
    length . filter (isInfixOf "processor") . lines <$> readFile proc_cpuinfo

