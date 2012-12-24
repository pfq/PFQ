--
--
--  (C) 2011-13 Nicola Bonelli <nicola.bonelli@cnit.it>   
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
import Control.Monad(when,unless,forM)
import Text.Regex.Posix

import Data.List

pfq_omatic_ver,pfq_kcompat,pfq_symvers,proc_cpuinfo :: String    

pfq_omatic_ver  = "1.1"
pfq_kcompat     = "/usr/local/include/pfq/pfq_kcompat.h"
pfq_symvers     = "/lib/modules/" ++ uname_r ++ "/kernel/net/pfq/Module.symvers"
proc_cpuinfo    = "/proc/cpuinfo"


uname_r :: String
uname_r = head . lines $ unsafePerformIO $ readProcess "/bin/uname" ["-r"] "" 


main :: IO ()
main = do
    putStrLn $ "[PFQ] pfq-omatic: v" ++ pfq_omatic_ver
    sanityCheck
    getRecursiveContents "." ".c" >>= mapM_ tryPatch 
    copyFile pfq_symvers "Module.symvers"
    let cmd = "make -j" ++ show getNumberOfPhyCores
    putStrLn $ "[PFQ] compiling: " ++ cmd ++ "..."
    _ <- system $ cmd
    putStrLn "[PFQ] done."
                                       

regexFunCall :: String -> Int -> String
regexFunCall fun n = fun ++ "[[:space:]]*" ++ "\\(" ++ args n  ++ "\\)"
                        where args 0 = "[[:space:]]*"
                              args 1 = "[^,]*"
                              args x = "[^,]*," ++ args (x-1)


tryPatch :: FilePath -> IO ()
tryPatch file = do
    readFile file >>= \c ->
        when (c =~ ((regexFunCall "netif_rx" 1) ++ "|" ++ (regexFunCall "netif_receive_skb" 1) ++ "|" ++ (regexFunCall "napi_gro_receive" 2))) $ do
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
    doesFileExist pfq_kcompat >>= \kc -> (unless kc $ error "error: could not locate pfq-kcompat header!")
    doesFileExist pfq_symvers >>= \sv -> (unless sv $ error "error: could not locate pfq Module.symvers!") 
    doesFileExist "Makefile"  >>= \mf -> (unless mf $ error "error: Makefile not found!") 


type Ext = String


getRecursiveContents :: FilePath -> Ext -> IO [FilePath]
getRecursiveContents topdir ext = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \fname -> do
    let path = topdir </> fname
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path ext
      else if (takeExtensions path == ext) 
            then return [path]
            else return []
  return (concat paths)
 

getNumberOfPhyCores :: Int
getNumberOfPhyCores = unsafePerformIO $ readFile proc_cpuinfo >>= \file ->  
    return $ length $ filter (isInfixOf "processor") $ lines file 

