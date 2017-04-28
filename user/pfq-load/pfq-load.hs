-- Copyright (c) 2015-16 Nicola Bonelli <nicola@pfq.io>
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Semigroup
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Either
import Data.Char
import Data.Time.Clock
import qualified Data.ByteString as B (ByteString, readFile)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?))

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Concurrent (threadDelay)
import Control.Exception

import Data.Data

import System.Console.ANSI
import System.Console.CmdArgs
import System.Directory (getHomeDirectory, doesFileExist, getModificationTime)
import System.IO.Unsafe
import System.IO.Error
import System.Process
import System.Exit
import System.FilePath
import System.Posix.Signals
import System.Posix.Types
import qualified Network.PFQ as Q (version)

proc_cpuinfo, proc_modules, tmp_pfq :: String

proc_cpuinfo = "/proc/cpuinfo"
proc_modules = "/proc/modules"
tmp_pfq      = "/tmp/pfq.opt"

dmesg, pidof, ethtool, setpci, cpufreq_set, rmmod, insmod, modprobe, lsmod, ifconfig :: String

dmesg       = "/bin/dmesg"
pidof       = "/bin/pidof"
ethtool     = "/sbin/ethtool"
setpci      = "/usr/bin/setpci"
cpufreq_set = "/usr/bin/cpufreq-set"
rmmod       = "/sbin/rmmod"
insmod      = "/sbin/insmod"
modprobe    = "/sbin/modprobe"
lsmod       = "/sbin/lsmod"
ifconfig    = "/sbin/ifconfig"


bold  = setSGRCode [SetConsoleIntensity BoldIntensity]
red   = setSGRCode [SetColor Foreground Vivid Red]
blue  = setSGRCode [SetColor Foreground Vivid Blue]
reset = setSGRCode []


configFiles = [ "/etc/pfq.conf.yaml"
              , "/root/.pfq.conf.yaml"
              , "/root/pfq.conf.yaml"
              , "/etc/pfq.conf"
              , "/root/.pfq.conf"
              , "/root/pfq.conf"
              ]

defaultDelay = 200000


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


data SystemError = SystemError String ExitCode
    deriving (Show, Typeable)

instance Exception SystemError


data Device =
    Device
    {   devname   :: String
    ,   devspeed  :: Maybe Int
    ,   channels  :: Maybe Int
    ,   flowctrl  :: Maybe Bool
    ,   ethopt    :: [(String, String)]
    } deriving (Show, Read, Eq)

instance FromJSON Device where
  parseJSON (Y.Object v) =
    Device <$>
        v .:   "devname"    <*>
        v .:?  "devspeed"   <*>
        v .:?  "channels"   <*>
        v .:?  "flowctrl"   <*>
        v .:   "ethopt"
  parseJSON _ = fail "Expected Object for Device value"

data Driver =
    Driver
    {   drvmod      :: String
    ,   drvopt      :: [String]
    ,   pci         :: [(String, String)]
    ,   instances   :: Int
    ,   devices     :: [Device]
    } deriving (Show, Read, Eq)


instance FromJSON Driver where
  parseJSON (Y.Object v) =
    Driver <$>
        v .:   "drvmod"    <*>
        v .:   "drvopt"    <*>
        v .:   "pci"       <*>
        v .:   "instances" <*>
        v .:   "devices"
  parseJSON _ = fail "Expected Object for Driver value"


data Config = Config
    {   pfq_module     :: String
    ,   pfq_options    :: [String]
    ,   exclude_core   :: [Int]
    ,   irq_affinity   :: [String]
    ,   cpu_governor   :: String
    ,   drivers        :: [Driver]
    } deriving (Show, Read, Eq)


instance FromJSON Config where
  parseJSON (Y.Object v) =
      Config <$>
        v .:   "pfq_module"     <*>
        v .:   "pfq_options"    <*>
        v .:   "exclude_core"   <*>
        v .:   "irq_affinity"   <*>
        v .:   "cpu_governor"   <*>
        v .:   "drivers"
  parseJSON _ = fail "Expected Object for Config value"



instance Semigroup Config where
    (Config mod1 opt1 excl1 algo1 gov1 drvs1) <> (Config mod2 opt2 excl2 algo2 gov2 drvs2) =
     Config
     {  pfq_module   = getOptString $ OptString mod1  <> OptString mod2
     ,  pfq_options  = getOptList   $ OptList opt1 <> OptList opt2
     ,  exclude_core = excl1 <> excl2
     ,  irq_affinity = algo1 <> algo2
     ,  cpu_governor = getOptString $ OptString gov1 <> OptString gov2
     ,  drivers      = drvs1 <> drvs2
     }


data Options = Options
    {   config     :: Maybe String
    ,   kmodule    :: String
    ,   algorithm  :: String
    ,   governor   :: String
    ,   first_core :: Int
    ,   exclude    :: [Int]
    ,   queues     :: Maybe Int
    ,   force      :: Bool
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
    ,   force      = False         &= help "Force PFQ reload!"
    ,   others     = []            &= args
    } &= summary ("pfq-load (PFQ " ++ Q.version ++ ")") &= program "pfq-load"


-------------------------------------------------------------------------------------------


main :: IO ()
main = handle (\(SystemError msg code) -> putStrBoldLn msg *> exitWith code) $ do

    -- load options...
    home <- getHomeDirectory
    opt  <- cmdArgsRun options
    conf <- (<> mkConfig opt) <$> loadConfig (catMaybes (config opt : map Just configFiles)) opt
    pmod <- getProcModules
    core <- getNumberOfPhyCores
    bal  <- getProcessID "irqbalance"
    frd  <- getProcessID "cpufreqd"

    -- compute the command to load pfq...

    let pfqLoadCmd = if null (pfq_module conf)
        then mkLoadModuleCmd ProbeMod  "pfq" (pfq_options conf)
        else mkLoadModuleCmd InsertMod (pfq_module conf) (pfq_options conf)


    -- determine whether pfq load is required...

    tstamp <- if null (pfq_module conf)
                then getCurrentTime
                else getModificationTime $ pfq_module conf


    pfqForceLoad <- or <$> sequence [ return $ force opt
                                    , not <$> isLoaded "pfq"
                                    , (/= (pfqLoadCmd ++ " " ++ show tstamp)) <$> possiblyReadFile tmp_pfq
                                    ]
    writeFile tmp_pfq pfqLoadCmd
    appendFile tmp_pfq (" " ++ show tstamp)

    -- check queues
    when (maybe False (> core) (queues opt)) $ errorWithoutStackTrace "queues number is too big!"

    -- unload pfq and drivers that depend on it...
    evalStateT (unloadDependencies pfqForceLoad "pfq") pmod

    -- check irqbalance deaemon
    unless (null bal) $
        putStrBoldLn ("Irqbalance daemon detected @pid " ++ show bal ++ ". Sending SIGKILL...") *> forM_ bal (signalProcess sigKILL)

    -- check cpufreqd deaemon
    unless (null frd) $
        putStrBoldLn ("Cpufreqd daemon detected @pid " ++ show frd ++ ". Sending SIGKILL...") *> forM_ frd (signalProcess sigKILL)

    -- set cpufreq governor...
    unless (null (cpu_governor conf)) $
        forM_ [0.. core-1] $ \n ->
            runSystem (cpufreq_set ++ " -g " ++ cpu_governor conf ++ " -c " ++ show n) "*** cpufreq-set error! Make sure you have cpufrequtils installed! ***"

    -- load PFQ (if required)...
    if pfqForceLoad
       then do
            putStrBoldLn (blue ++ "Loading pfq [" ++ pfqLoadCmd ++ "]")
            system $ dmesg ++ " --clear"
            runSystem pfqLoadCmd (red ++ "load pfq.ko error!")
            xs <- lines <$> readProcess dmesg ["-t"] []
            forM_ xs $ \x -> when ("pfq: unknown parameter" `isInfixOf` x) (throw $ SystemError (red ++ "load pfq.ko: " ++ x ++ "!") (ExitFailure 42))

       else putStrBoldLn $ red ++ "Using pfq.ko module already loaded (use --force to reload it)..."

    -- update current loaded proc/modules
    pmod2 <- getProcModules

    -- unload drivers...
    unless (null (drivers conf)) $
        putStrBoldLn "Unloading vanilla/standard drivers..." *>
        evalStateT (forM_ (drivers conf) $ unloadDependencies True . takeBaseName . drvmod) pmod2

    -- load and configure device drivers...
    forM_ (drivers conf) $ \drv -> do
        let rss = maybe [] (mkRssOption (drvmod drv) (instances drv)) (queues opt)
        unless (null $ drvmod drv) $
            loadModule InsertMod (drvmod drv) (drvopt drv ++ rss)
        threadDelay defaultDelay
        forM_ (devices drv) $ setupDevice (queues opt)

    -- setup per-driver pci options..
    forM_ (drivers conf) $  setupPCI . pci


    -- set interrupt affinity...
    putStrBoldLn "Setting irq affinity..."
    setupIRQAffinity (first_core opt) (exclude_core conf) (irq_affinity conf) (getDevices conf)

    putStrBoldLn "PFQ ready."


mkRssOption :: String -> Int -> Int -> [String]
mkRssOption driver ndev nqueue
    | "ixgbe.ko" `isSuffixOf` driver = [ "RSS=" ++ intercalate "," (replicate ndev (show nqueue)) ]
    | "igb.ko"   `isSuffixOf` driver = [ "RSS=" ++ intercalate "," (replicate ndev (show nqueue)) ]
    | otherwise                      = []


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


getFirstConfig :: [FilePath] -> IO (Maybe FilePath)
getFirstConfig xs = filterM doesFileExist xs >>= \case
      [ ]   -> return Nothing
      (x:_) -> return $ Just x


clean :: String -> String
clean =  unlines . filter notComment . lines
        where notComment = (not . ("#" `isPrefixOf`)) . dropWhile isSpace


loadConfig :: [FilePath] -> Options -> IO Config
loadConfig confs opt =
    getFirstConfig confs >>= \case
        Nothing   -> putStrBoldLn "Using default config..." >> return (mkConfig opt)
        Just conf ->
            putStrBoldLn ("Using " ++ conf ++ " config...") *>
            if "yaml" `isInfixOf` conf
               then do
                   conf' <- Y.decodeFileEither conf
                   case conf' of
                      Right x -> return x
                      Left  x -> throw $ SystemError (Y.prettyPrintParseException x) (ExitFailure 42)
               else (read . clean) <$> readFile conf


{-# NOINLINE getNumberOfPhyCores #-}
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
getProcessID name = (map read . words) <$> catchIOError (readProcess pidof [name] "") (\_-> return [])


moduleDependencies :: String -> ProcModules -> [String]
moduleDependencies  name =
    concatMap (\(m,ds) -> if m == name then ds else [])


unloadDependencies :: Bool              -- unload the given module as well
                   -> String            -- kernel module name
                   -> ModStateT IO ()
unloadDependencies rm name = do
    proc_mods <- get
    mapM_ (unloadDependencies True) (moduleDependencies name proc_mods)
    when (rm && isModuleLoaded name proc_mods) $ do
        liftIO $ rmmod' name
        put $ rmmodFromProcMOdules name proc_mods
        where rmmod' name = putStrBoldLn ("Unloading " ++ name ++ "...") *>
                            runSystem (rmmod ++ " " ++ name) ("rmmod " ++ name ++ " error.")
              isModuleLoaded name =  any (\(mod,_) -> mod == name)


data LoadMode = InsertMod | ProbeMod
                    deriving Eq


isLoaded :: String -> IO Bool
isLoaded mod = getProcModules >>= (\pmod -> return $ mod `elem` fmap fst pmod)


-- putStrBoldLn $ "Loading " ++ name ++ " " ++ show opts


mkLoadModuleCmd :: LoadMode -> String -> [String] -> String
mkLoadModuleCmd mode name opts =
    tool ++ " " ++ name ++ " " ++ unwords opts
    where tool = if mode == InsertMod then insmod
                                      else modprobe


loadModule :: LoadMode -> String -> [String] -> IO ()
loadModule mode name opts = putStrBoldLn ("Loading " ++ name ++ " " ++ show opts) *>
    runSystem (mkLoadModuleCmd mode name opts) ("insmod " ++ name ++ " error.")



setupDevice :: Maybe Int -> Device -> IO ()
setupDevice queues (Device dev speed channels fctrl opts) = do

    putStrBoldLn $ "Activating " ++ dev ++ "..."

    runSystem (ifconfig ++ " " ++ dev ++ " up") "ifconfig error!"

    threadDelay defaultDelay

    if isJust speed
       then do
            let s = fromJust speed
            putStrBoldLn $ "Setting speed (" ++ show s ++ ") for " ++ dev ++ "..."
            runSystem (ethtool ++ " -s " ++ dev ++ " speed " ++ show s ++ " duplex full autoneg off") "ethtool: set speed error!"
       else
            putStrBoldLn ("Auto-negotiating speed for " ++ dev ++ "...") *>
            runSystem (ethtool ++ " -s " ++ dev ++ " duplex full autoneg off") "ethtool: auto-negotiation error!"

    threadDelay defaultDelay

    when (isJust fctrl) $ do
        let t = fromJust fctrl
        if t then putStrBoldLn ("Disabling flow control for " ++ dev ++ "...") *>
                    runSystem (ethtool ++ " -A " ++ dev ++ " rx on tx on") "ethtool: enabling flow-ctrl error!"
             else putStrBoldLn ("Enabling flow control for " ++ dev ++ "...") *>
                    runSystem (ethtool ++ " -A " ++ dev ++ " autoneg off rx off tx off") "ethtool: disabling flow-ctrl error!"

    threadDelay defaultDelay

    when (isJust queues || isJust channels) $ do
        let c = fromJust (channels <|> queues)
        putStrBoldLn $ "Setting channels to " ++ show c ++ "..."
        runSystem (ethtool ++ " -L " ++ dev ++ " combined " ++ show c) ""

    forM_ opts $ \(opt, arg) ->
        threadDelay defaultDelay *>
        runSystem (ethtool ++ " " ++ opt ++ " " ++ dev ++ " " ++ arg) ("ethtool:" ++ opt ++ " error!")


setupPCI :: [(String, String)] -> IO ()
setupPCI = mapM_ (\(a, v) -> runSystem (setpci ++ " -v -d " ++ show a ++ " " ++ show v) (setpci ++ ": error!"))


getDevices ::  Config -> [String]
getDevices conf =
    map devname (concatMap devices (drivers conf))


setupIRQAffinity :: Int -> [Int] -> [String] -> [String] -> IO ()
setupIRQAffinity fc excl algs devs = do
    let excl_opt = unwords (map (\n -> " -e " ++ show n) excl)
    let affinity = zip algs (tails devs)
    unless (null affinity) $
        forM_ affinity $ \(alg, devs') ->
            runSystem ("pfq-affinity -f " ++ show fc  ++ " " ++ excl_opt ++ " -a " ++ alg ++ " -m TxRx " ++ unwords devs') "pfq-affinity error!"


runSystem :: String -> String -> IO ()
runSystem cmd errmsg =
    putStrLn ("-> " ++ cmd) *>
        system cmd >>= \ec -> unless (null errmsg || ec == ExitSuccess) $ throw (SystemError (red ++ errmsg ++ reset) ec)


putStrBoldLn :: String -> IO ()
putStrBoldLn msg = putStrLn $ bold ++ msg ++ reset


possiblyReadFile :: FilePath -> IO String
possiblyReadFile file = do
    res <- tryIOError $ readFile file
    case res of
        Left _    -> return ""
        Right str -> length str `seq` return str

