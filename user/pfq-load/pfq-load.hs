
-- Copyright (c) 2015 Nicola Bonelli <nicola@pfq.io>
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
import Data.Char

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Concurrent (threadDelay)

import Data.Data

import System.Console.ANSI
import System.Console.CmdArgs
import System.Directory (getHomeDirectory)
import System.IO.Unsafe
import System.IO.Error
import System.Process
import System.Exit
import System.FilePath
import System.Posix.Signals
import System.Posix.Types

proc_cpuinfo, proc_modules :: String
proc_cpuinfo = "/proc/cpuinfo"
proc_modules = "/proc/modules"

bold  = setSGRCode [SetConsoleIntensity BoldIntensity]
reset = setSGRCode []

version = "4.4.3"


data YesNo = Yes | No | Unspec
    deriving (Show, Read, Eq)

newtype OptString = OptString { getOptString :: String }
    deriving (Show, Read, Eq)

newtype OptList a = OptList { getOptList :: [a] }
    deriving (Show, Read, Eq)

instance Semigroup OptString where
    a <> OptString "" = a
    _ <> b = b

instance Semigroup (OptList a) where
    a <> OptList [] = a
    _ <> b = b


data Device =
    Device
    {   devname   :: String
    ,   devspeed  :: Maybe Int
    ,   channels  :: Maybe Int
    ,   flowctrl  :: YesNo
    ,   ethopt    :: [(String, String, Int)]
    } deriving (Show, Read, Eq)


data Driver =
    Driver
    {   drvmod      :: String
    ,   drvopt      :: [String]
    ,   instances   :: Int
    ,   devices     :: [Device]
    } deriving (Show, Read, Eq)


data Config = Config
    {   pfq_module     :: String
    ,   pfq_options    :: [String]
    ,   exclude_core   :: [Int]
    ,   irq_affinity   :: [String]
    ,   cpu_governor   :: String
    ,   drivers        :: [Driver]
    } deriving (Show, Read, Eq)


instance Semigroup Config where
    (Config mod1 opt1 excl1 algo1 gov1 drvs1) <> (Config mod2 opt2 excl2 algo2 gov2 drvs2) =
     Config
     {
        pfq_module   = getOptString $ OptString mod1  <> OptString mod2,
        pfq_options  = getOptList   $ OptList opt1 <> OptList opt2,
        exclude_core = excl1 <> excl2,
        irq_affinity = algo1 <> algo2,
        cpu_governor = getOptString $ OptString gov1 <> OptString gov2,
        drivers      = drvs1 <> drvs2
     }


data Options = Options
    {   config     :: Maybe String
    ,   kmodule    :: String
    ,   algorithm  :: String
    ,   governor   :: String
    ,   first_core :: Int
    ,   exclude    :: [Int]
    ,   queues     :: Maybe Int
    ,   others     :: [String]
    } deriving (Show, Read, Data, Typeable)


options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {   config     = Nothing       &= typ "FILE" &= help "Specify config file (default ~/.pfq.conf)"
    ,   kmodule    = ""            &= help "Override the kmodule specified in config file"
    ,   queues     = Nothing       &= help "Specify hardware channels (i.e. Intel RSS)"
    ,   algorithm  = ""            &= help "Irq affinity algorithm: naive, round-robin, even, odd, all-in:id, comb:id"
    ,   governor   = ""            &= help "Set cpufreq governor"
    ,   first_core = 0             &= typ "NUM" &= help "First core used for irq affinity"
    ,   exclude    = []            &= typ "CORE" &= help "Exclude core from irq affinity"
    ,   others     = []            &= args
    } &= summary ("pfq-load " ++ version) &= program "pfq-load"


-------------------------------------------------------------------------------------------


main :: IO ()
main = do

    -- load options...
    home <- getHomeDirectory
    opt  <- cmdArgsRun options
    conf <- (<> mkConfig opt) <$> loadConfig (fromMaybe (home </> ".pfq.conf") (config opt)) opt
    pmod <- getProcModules
    core <- getNumberOfPhyCores
    bal  <- getProcessID "irqbalance"
    frd  <- getProcessID "cpufreqd"

    -- check queues
    when (maybe False (> core) (queues opt)) $ error "queues number too big!"

    -- unload pfq and drivers that depend on it...
    evalStateT (unloadModule "pfq") pmod

    -- check irqbalance deaemon
    unless (null bal) $ do
        putStrBoldLn $ "Irqbalance daemon detected @pid " ++ show bal ++ ". Sending SIGKILL..."
        forM_ bal $ signalProcess sigKILL

    -- check cpufreqd deaemon
    unless (null frd) $ do
        putStrBoldLn $ "Cpufreqd daemon detected @pid " ++ show frd ++ ". Sending SIGKILL..."
        forM_ frd $ signalProcess sigKILL

    -- set cpufreq governor...
    runSystem ("/usr/bin/cpufreq-set -g " ++ cpu_governor conf) ("*** cpfreq-set error! Make sure you have cpufrequtils installed! *** ", True)

    -- load PFQ...
    if null (pfq_module conf)
        then loadModule ProbeMod  "pfq" (pfq_options conf)
        else loadModule InsertMod (pfq_module conf) (pfq_options conf)

    -- update current loaded proc/modules
    pmod2 <- getProcModules

    -- unload drivers...
    unless (null (drivers conf)) $ do
        putStrBoldLn "Unloading vanilla/standard drivers..."
        evalStateT (mapM_ (unloadModule . takeBaseName . drvmod) (drivers conf)) pmod2

    -- load and configure device drivers...
    forM_ (drivers conf) $ \drv -> do
        let rss = maybe [] (mkRssOption (drvmod drv) (instances drv)) (queues opt)
        loadModule InsertMod (drvmod drv) (drvopt drv ++ rss)
        threadDelay 1000000
        mapM_ (setupDevice (queues opt)) (devices drv)

    -- set interrupt affinity...
    putStrBoldLn "Setting irq affinity..."
    setupIRQAffinity (first_core opt) (exclude_core conf) (irq_affinity conf) (getDevices conf)

    putStrBoldLn "PFQ ready."


mkRssOption :: String -> Int -> Int -> [String]
mkRssOption driver numdev queues =
    case () of
     _   | "ixgbe.ko" `isSuffixOf` driver -> [ "RSS=" ++ intercalate "," (replicate numdev (show queues)) ]
         | "igb.ko"   `isSuffixOf` driver -> [ "RSS=" ++ intercalate "," (replicate numdev (show queues)) ]
         | otherwise -> []


mkConfig :: Options -> Config
mkConfig
    Options
    {   config    = _
    ,   kmodule   = mod
    ,   algorithm = algo
    ,   exclude   = excl
    ,   governor  = gov
    ,   others    = opt } =
    Config
    {   pfq_module    = mod
    ,   pfq_options   = opt
    ,   exclude_core  = excl
    ,   irq_affinity  = [algo | not (null algo)]
    ,   cpu_governor  = gov
    ,   drivers       = []
    }


notCommentLine :: String -> Bool
notCommentLine = (not . ("#" `isPrefixOf`)) . (dropWhile isSpace)


loadConfig :: FilePath -> Options -> IO Config
loadConfig conf opt =
    catchIOError (liftM (read . unlines . filter notCommentLine . lines) (readFile conf)) (\_ -> return $ mkConfig opt)


getNumberOfPhyCores :: IO Int
getNumberOfPhyCores = readFile proc_cpuinfo >>= \file ->
    return $ (length . filter (isInfixOf "processor") . lines) file


type ProcModules = [ (String, [String]) ]
type ModStateT   = StateT ProcModules


getProcModules :: IO ProcModules
getProcModules =
    readFile proc_modules >>= \file ->
        return $ map (\l -> let ts = words l in (head ts,  filter (\s -> (not . null) s && s /= "-") $
                                splitOn "," (ts !! 3))) $ lines file


rmmodFromProcMOdules :: String -> ProcModules -> ProcModules
rmmodFromProcMOdules name = filter (\(m,ds) -> m /= name )


getProcessID :: String -> IO [ProcessID]
getProcessID name = liftM (map read . words) $ catchIOError (readProcess "/bin/pidof" [name] "") (\_-> return [])


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
            runSystem ("/sbin/rmmod " ++ name) ("rmmod " ++ name ++ " error.", True)
          isModuleLoaded name =  any (\(mod,_) -> mod == name)


data LoadMode = InsertMod | ProbeMod
                    deriving Eq

loadModule :: LoadMode -> String -> [String] -> IO ()
loadModule mode name opts = do
    putStrBoldLn $ "Loading " ++ name ++ "..."
    runSystem (tool ++ " " ++ name ++ " " ++ unwords opts) ("insmod " ++ name ++ " error.", True)
    where tool = if mode == InsertMod then "/sbin/insmod"
                                      else "/sbin/modprobe"


setupDevice :: Maybe Int -> Device -> IO ()
setupDevice queues (Device dev speed channels fctrl opts) = do

    putStrBoldLn $ "Activating " ++ dev ++ "..."
    runSystem ("/sbin/ifconfig " ++ dev ++ " up") ("ifconfig error!", True)

    case fctrl of
        No -> do
                putStrBoldLn $ "Disabling flow control for " ++ dev ++ "..."
                runSystem ("/sbin/ethtool -A " ++ dev ++ " autoneg off rx off tx off") ("ethtool: flowctrl error!", False)
        Yes -> do
                putStrBoldLn $ "Enabling flow control for " ++ dev ++ "..."
                runSystem ("/sbin/ethtool -A " ++ dev ++ " autoneg on rx on tx on") ("ethtool: flowctrl error!", False)
        Unspec -> return ()

    when (isJust speed) $ do
        let s = fromJust speed
        putStrBoldLn $ "Setting speed (" ++ show s ++ ") for " ++ dev ++ "..."
        runSystem ("/sbin/ethtool -s " ++ dev ++ " speed " ++ show s ++ " duplex full") ("ethtool: set speed error!", False)

    when (isJust queues || isJust channels) $ do
        let c = fromJust (queues <|> channels)
        putStrBoldLn $ "Setting channels to " ++ show c ++ "..."
        runSystem ("/sbin/ethtool -L " ++ dev ++ " combined " ++ show c) ("", False)

    forM_ opts $ \(opt, arg, value) ->
        runSystem ("/sbin/ethtool " ++ opt ++ " " ++ dev ++ " " ++ arg ++ " " ++ show value) ("ethtool:" ++ opt ++ " error!", True)


getDevices ::  Config -> [String]
getDevices conf =
    map devname (concatMap devices (drivers conf))


setupIRQAffinity :: Int -> [Int] -> [String] -> [String] -> IO ()
setupIRQAffinity fc excl algs devs = do
    let excl_opt = unwords (map (\n -> " -e " ++ show n) excl)
    let affinity = zip algs (tails devs)
    unless (null affinity) $
        forM_ affinity $ \(alg, devs') ->
            runSystem ("/root/.cabal/bin/irq-affinity -f " ++ show fc  ++ " " ++ excl_opt ++ " -a " ++ alg ++ " -m TxRx " ++ unwords devs') ("irq-affinity error!", True)


runSystem :: String -> (String,Bool) -> IO ()
runSystem cmd (errmsg,term) = do
    putStrLn $ "-> " ++ cmd
    system cmd >>= \ec -> when (ec /= ExitSuccess) $ (if term then error else putStrLn) errmsg


putStrBoldLn :: String -> IO ()
putStrBoldLn msg = putStrLn $ bold ++ msg ++ reset

