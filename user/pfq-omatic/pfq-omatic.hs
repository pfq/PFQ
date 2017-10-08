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


import System.IO.Unsafe
import System.Process
import System.Directory
import System.Environment
import System.FilePath
import System.Exit

import Control.Monad(void,when,unless,forM,filterM)
import Control.Applicative
import Text.Regex.Posix

import Data.List
import Data.Maybe
import Data.Function(on)
import qualified Network.PFQ as Q (version)


--
-- Configuration
--

procCpuInfo = "/proc/cpuinfo" :: String

pfqKcompat  = "/usr/include/linux/pf_q-kcompat.h" :: String

pfqSymvers  =
    [ "/lib/modules/" ++ uname_r ++ "/kernel/net/pfq/Module.symvers"
    , home_dir ++ "/PFQ/kernel/Module.symvers"
    , "/opt/PFQ/kernel/Module.symvers"
    , "/usr/src/PFQ/kernel/Module.symvers"
    ] :: [String]

makeFilePatch = unlines
    [ ""
    , "all:"
    , "\tmake -C /lib/modules/$(shell uname -r)/build M=$(PWD) modules"
    , "\n"
    , "clean:"
    , "\tmake -C /lib/modules/$(shell uname -r)/build M=$(PWD) clean"
    ] :: String


--
-- Builder script
--

data Option = Option
    {
        help     :: Bool
    ,   inKernel :: Bool

    } deriving (Eq, Show)


getArguments :: IO (Option, [String])
getArguments = do
    args <- getArgs
    let xs = deleteFirstsBy (==) args ["-?", "-ik", "--in-kernel"]
        ik = "-ik" `elem` args || "--in-kernel" `elem` args
        hl = "-?" `elem` args
    return (Option hl ik, xs)


printHelp :: IO ()
printHelp = putStrLn "pfq-omatic [-?] [-ik|--in-kernel] [Makefile args...]"


main :: IO ()
main = do
    (opt, args) <- getArguments
    putStrLn $ "[PFQ] pfq-omatic: PFQ v" ++ Q.version
    when (help opt) $ printHelp  >> exitSuccess
    checkPreconditions
    getRecursiveContents "." [".c"] >>= mapM_ tryPatch
    symver <- fromJust <$> getMostRecentFile pfqSymvers
    copyFile symver "Module.symvers"

    makefile <- if inKernel opt
                then do patchMakeFile "Makefile"
                        return "Makefile.ik"
                else return "Makefile"

    let cmd = "make KBUILD_EXTRA_SYMBOLS=" ++ symver ++ " -j" ++ show getNumberOfPhyCores ++ " -f " ++ makefile ++ " " ++ unwords args
    putStrLn $ "[PFQ] compiling: " ++ cmd ++ "..."
    void $ system cmd
    putStrLn "[PFQ] done."


getMostRecentFile :: [FilePath] -> IO (Maybe FilePath)
getMostRecentFile xs = do
    xs' <- filterM doesFileExist xs >>=
             mapM (\f -> (\m -> (f,m)) <$> getModificationTime f) >>= \x ->
               return $ sortBy (flip compare `on` snd) x
    return $ listToMaybe (map fst xs')


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
            doesFileExist (file ++ ".omatic") >>= \orig ->
                if orig
                then putStrLn $ "[PFQ] " ++ file ++ " is already patched :)"
                else patchSource file


patchSource :: FilePath -> IO ()
patchSource file = do
    putStrLn $ "[PFQ] patching " ++ file
    src <- readFile file
    renameFile file $ file ++ ".omatic"
    writeFile file $ "#include " ++ show pfqKcompat ++ "\n" ++ src


patchMakeFile :: FilePath -> IO ()
patchMakeFile file = do
    putStrLn "[PFQ] creating Makefile.ik..."
    orig <- readFile file
    writeFile (file ++ ".ik") (orig ++ makeFilePatch)


checkPreconditions :: IO ()
checkPreconditions = do
    doesFileExist pfqKcompat >>= \kc ->
        unless kc $ error "error: could not locate pfq-kcompat.h header!"
    symver <- getMostRecentFile pfqSymvers
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


{-# NOINLINE getNumberOfPhyCores #-}
getNumberOfPhyCores :: Int
getNumberOfPhyCores = unsafePerformIO $
    length . filter (isInfixOf "processor") . lines <$> readFile procCpuInfo


{-# NOINLINE uname_r #-}
uname_r :: String
uname_r = unsafePerformIO $
    head . lines <$> readProcess "/bin/uname" ["-r"] ""


{-# NOINLINE home_dir #-}
home_dir :: String
home_dir = unsafePerformIO getHomeDirectory


