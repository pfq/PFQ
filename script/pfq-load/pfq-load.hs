--
-- Copyright (c) 2013 Bonelli Nicola <nicola.bonelli@antifork.org>
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

import Data.Semigroup
import Data.List
import Data.List.Split
import Data.Maybe

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Data

import System.Console.ANSI
import System.Console.CmdArgs
import System.Directory (getHomeDirectory)
import System.IO.Unsafe
import System.Process
import System.Exit
import System.FilePath

proc_cpuinfo, proc_modules :: String
proc_cpuinfo = "/proc/cpuinfo"
proc_modules = "/proc/modules"

bold  = setSGRCode [SetConsoleIntensity BoldIntensity]
reset = setSGRCode []

version = "3.7"

data YesNo = Yes | No
    deriving (Show, Read, Eq, Data, Typeable)

newtype OptString = OptString { getOptString :: String }
    deriving (Show, Read, Eq, Data, Typeable)

instance Semigroup OptString where
    a <> OptString "" = a
    _ <> b = b


data Device =
    Device
    {
        devname   :: String,
        devspeed  :: Maybe Int,
        flowctrl  :: YesNo,
        ethopt    :: [(String, String, Int)]
    } deriving (Show, Read, Eq)


data Driver =
    Driver
    {
        drvmod  :: String,
        drvopt  :: [String],
        devices :: [Device]
    } deriving (Show, Read, Eq)


data Config = Config
    {
         pfq_module     :: String,
         pfq_options    :: [String],
         exclude_core   :: [Int],
         irq_affinity   :: String,
         drivers        :: [Driver]
    } deriving (Show, Read, Eq)


instance Semigroup Config where
    Config {
        pfq_module   = mod1,
        pfq_options  = opt1,
        exclude_core = excl1,
        irq_affinity = algo1,
        drivers      = drvs1
    } <>
        Config {
            pfq_module   = mod2,
            pfq_options  = opt2,
            exclude_core = excl2,
            irq_affinity = algo2,
            drivers      = drvs2
        } =
        Config {
            pfq_module   = getOptString $ OptString mod1  <> OptString mod2,
            pfq_options  = opt1  <> opt2,
            exclude_core = excl1 <> excl2,
            irq_affinity = getOptString $ OptString algo1 <> OptString algo2,
            drivers      = drvs1 <> drvs2
        }


data Options = Options
    {
         config     :: Maybe String,
         algorithm  :: String,
         first_core :: Int,
         exclude    :: [Int],
         queues     :: Maybe Int,
         others     :: [String]
    } deriving (Show, Read, Data, Typeable)


options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {
         config     = Nothing       &= typ "FILE" &= help "Specify configuration file (default ~/.pfq.conf)",
         queues     = Nothing       &= help "Specify hardware queues (i.e. Intel RSS)",
         algorithm  = ""            &= help "Irq affinity algorithm: naive, round-robin, even, odd, all-in:id, comb:id",
         first_core = 0             &= typ "NUM" &= help "First core used for irq affinity",
         exclude    = []            &= typ "CORE" &= help "Exclude core from irq affinity",
         others     = []            &= args
    } &= summary ("pfq-load " ++ version) &= program "pfq-load"


-------------------------------------------------------------------------------------------


main :: IO ()
main = do

    -- load options...
    home <- getHomeDirectory
    opt  <- cmdArgsRun options
    conf <- (<> mkConfig opt) <$> loadConfig (fromMaybe (home </> ".pfq.conf") (config opt))
    pmod <- loadProcModules
    core <- getNumberOfPhyCores

    -- check queues
    when (maybe False (> core) (queues opt)) $ error "queues number too big!"

    -- unload pfq and dependent drivers...
    evalStateT (unloadModule "pfq") pmod

    -- load PFQ...
    loadModule (pfq_module conf) (pfq_options conf)

    -- update current loaded proc/modules
    pmod2 <- loadProcModules

    -- unload drivers...
    putStrBoldLn "Unloading vanilla/standard drivers..."
    evalStateT (mapM_ (unloadModule . takeBaseName . drvmod) (drivers conf)) pmod2

    -- load device drivers...
    forM_ (drivers conf) $ \drv -> do
        let rss = maybe [] (mkRssOption (drvmod drv) (length $ devices drv)) (queues opt)
        loadModule (drvmod drv) (drvopt drv ++ rss)
        mapM_ setupDevice (devices drv)

    -- set interrupt affinity...
    putStrBoldLn "Setting irq affinity..."

    setupIRQAffinity (first_core opt) (exclude_core conf) (irq_affinity conf) (getDevices conf)
    putStrBoldLn "PFQ ready."


mkRssOption :: String -> Int -> Int -> [String]
mkRssOption driver numdev queues =
    case () of
     _   | "ixgbe.ko" `isSuffixOf` driver -> [ "RSS=" ++ intercalate "," (replicate numdev (show queues)) ]
         | "igb.ko"   `isSuffixOf` driver -> [ "RSS=" ++ intercalate "," (replicate numdev (show queues)) ]
         | otherwise -> error "queue option: driver not supported!"


mkConfig :: Options -> Config
mkConfig
    Options { config = _,
              algorithm = algo,
              exclude   = excl,
              others    = mod
            } =
    Config {
        pfq_module    = if null mod || not (isModuleName (head mod)) then "" else head mod,
        pfq_options   = [],
        exclude_core  = excl,
        irq_affinity  = algo,
        drivers       = []
    }
    where isModuleName = (".ko" `isSuffixOf`)


loadConfig :: FilePath -> IO Config
loadConfig conf =
    liftM read $ fmap (unlines . filter (not . ("#" `isPrefixOf`)) . lines) (readFile conf)


getNumberOfPhyCores :: IO Int
getNumberOfPhyCores = readFile proc_cpuinfo >>= \file ->
    return $ length $ filter (isInfixOf "processor") $ lines file


type ProcModules = [ (String, [String]) ]
type ModStateT   = StateT ProcModules


loadProcModules :: IO ProcModules
loadProcModules =
    readFile proc_modules >>= \file ->
        return $ map (\l -> let ts = words l in (head ts,  filter (\s -> (not . null) s && s /= "-") $
                                splitOn "," (ts !! 3))) $ lines file


rmmodFromProcMOdules :: String -> ProcModules -> ProcModules
rmmodFromProcMOdules name = filter (\(m,ds) -> m /= name )


moduleDependencies :: String -> ProcModules -> [String]
moduleDependencies  name =
    concatMap (\(m,ds) -> if m == name then ds else [])


unloadModule :: String -> ModStateT IO ()
unloadModule name = do
    proc_mods <- get
    mapM_ unloadModule (moduleDependencies name proc_mods)
    when (isModuleLoaded name proc_mods) $ do
        liftIO $ rmmod name
        put $ rmmodFromProcMOdules name proc_mods
    where rmmod name = do
            putStrBoldLn $ "Unloading " ++ name ++ "..."
            runSystem ("/sbin/rmmod " ++ name)
                ("rmmod " ++ name ++ " error.")
          isModuleLoaded name =  any (\(mod,_) -> mod == name)


loadModule :: String -> [String] -> IO ()
loadModule name opts = do
    putStrBoldLn $ "Loading " ++ name ++ "..."
    runSystem ("/sbin/insmod " ++ name ++ " " ++ unwords opts)
        ("insmod " ++ name ++ " error.")


setupDevice :: Device -> IO ()
setupDevice (Device dev speed fctrl opts) = do
    putStrBoldLn $ "Activating " ++ dev ++ "..."
    runSystem ("/sbin/ifconfig " ++ dev ++ " up") "ifconfig error!"
    when (fctrl == No) $ do
        putStrBoldLn $ "Disabling flow control for " ++ dev ++ "..."
        runSystem ("/sbin/ethtool -A " ++ dev ++ " autoneg off rx off tx off") "ethtool error!"
    when (isJust speed) $ do
        let s = fromJust speed
        putStrBoldLn $ "Setting speed (" ++ show s ++ ") for " ++ dev ++ "..."
        runSystem ("/sbin/ethtool -s " ++ dev ++ " speed " ++ show s ++ " duplex full") "ethtool error!"
    forM_ opts $ \(opt, arg, value) -> do
        runSystem ("/sbin/ethtool " ++ opt ++ " " ++ dev ++ " " ++ arg ++ " " ++ show value) "ethtool error!"

getDevices ::  Config -> [String]
getDevices conf = map devname (concatMap devices (drivers conf))


setupIRQAffinity :: Int -> [Int] -> String -> [String] -> IO ()
setupIRQAffinity fc excl algo devs = do
    let excl_opt = unwords (map (\n -> " -e " ++ show n) excl)
    unless (null algo) $
        runSystem ("irq-affinity -f " ++ show fc  ++ " " ++ excl_opt ++ " -a " ++ algo ++ " -m TxRx " ++ unwords devs) "irq-affinity error!"


runSystem :: String -> String -> IO ()
runSystem cmd errmsg = do
    putStrLn $ "-> " ++ cmd
    system cmd >>= \ec -> when (ec /= ExitSuccess) $ error errmsg


putStrBoldLn :: String -> IO ()
putStrBoldLn msg = putStrLn $ bold ++ msg ++ reset

