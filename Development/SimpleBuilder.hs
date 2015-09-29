--
-- Copyright (c) 2014 Nicola Bonelli <nicola@pfq.io>
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Development.SimpleBuilder (

    Target(..),
    BuilderScript,
    Command,
    Options,
    (*>>),
    requires,
    into,
    simpleBuilder,
    numberOfPhyCores,

    -- predefined actions
    Development.SimpleBuilder.empty,
    cabalConfigure,
    cabalBuild,
    cabalInstall,
    cabalClean,
    cabalDistClean,
    cmake,
    cmake_distclean,
    make,
    make_install,
    make_clean,
    make_distclean,
    ldconfig,
    configure,
    cmd

) where


import System.Console.GetOpt

import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO.Unsafe

import Control.Monad
import Control.Monad.Writer.Lazy
import Control.Monad.IO.Class
import Control.Monad.RWS.Lazy
import Control.Applicative

import qualified Control.Exception as E

import Data.Data
import Data.List
import Data.Maybe


-- version

verString = "v0.4" :: String


-- predefined commands


empty = return () :: Action ()


cabalConfigure = tellCmd $
    DynamicCmd
        (\o ->
              case () of
                  _   | Just path <- sandbox o -> "cabal sandbox init --sandbox=" ++ path ++ " && cabal configure"
                      | otherwise -> "runhaskell Setup configure --user")


cabalBuild      = tellCmd $ StaticCmd "runhaskell Setup build"
cabalInstall    = tellCmd $ StaticCmd "runhaskell Setup install"
cabalClean      = tellCmd $ StaticCmd "runhaskell Setup clean"
cabalDistClean  = tellCmd $ StaticCmd "rm -rf dist; rm -f cabal.sandbox.config"

make_install    = tellCmd $ StaticCmd "make install"
make_clean      = tellCmd $ StaticCmd "make clean"
make_distclean  = tellCmd $ StaticCmd "make distclean"
ldconfig        = tellCmd $ StaticCmd "ldconfig"
configure       = tellCmd $ StaticCmd "./configure"


make = tellCmd $
    DynamicCmd
        (\o ->
              case () of
                  _   | jobs o == 0 -> "make"
                      | jobs o > numberOfPhyCores -> "make -j " ++ show (numberOfPhyCores + 1)
                      | otherwise -> "make -j " ++ show (jobs o))

cmake = tellCmd $
    DynamicCmd
        (\o ->
              let build = case buildType o of
                          Nothing -> ""
                          Just Release -> "-DCMAKE_BUILD_TYPE=Release"
                          Just Debug -> "-DCMAKE_BUILD_TYPE=Debug"
                  cc = case ccComp o of
                          Nothing -> ""
                          Just xs -> "-DCMAKE_C_COMPILER=" ++ xs
                  cxx = case cxxComp o of
                          Nothing -> ""
                          Just xs -> "-DCMAKE_CXX_COMPILER=" ++ xs
              in unwords ["cmake", build, cc, cxx, "."])


cmake_distclean :: Action ()
cmake_distclean = do
    cmd "rm -f install_manifest.txt"
    cmd "rm -f cmake.depends"
    cmd "rm -f cmake.chek_depends"
    cmd "rm -f CMakeCache.txt"
    cmd "rm -f *.cmake"
    cmd "rm -f Makefile"
    cmd "rm -rf CMakeFiles"


cmd :: String -> Action ()
cmd xs = tell ([StaticCmd xs], [])


tellCmd :: Command -> Action ()
tellCmd c = tell ([c], [])


-- options...


data Options = Options
    {   buildType  :: Maybe BuildType
    ,   cxxComp    :: Maybe String
    ,   ccComp     :: Maybe String
    ,   sandbox    :: Maybe String
    ,   dryRun     :: Bool
    ,   verbose    :: Bool
    ,   version    :: Bool
    ,   help       :: Bool
    ,   jobs       :: Int
    } deriving (Show, Read)


defaultOptions :: Options
defaultOptions = Options
    {   buildType =  Nothing
    ,   cxxComp   =  Nothing
    ,   ccComp    =  Nothing
    ,   sandbox   =  Nothing
    ,   dryRun    =  False
    ,   verbose   =  False
    ,   version   =  False
    ,   help      =  False
    ,   jobs      =  0
    }


options :: [OptDescr (Options -> Options)]
options = [ Option "v" ["verbose"]
                (NoArg (\o -> o { verbose = True }))
                "Verbose mode"
          , Option "V" ["version"]
                (NoArg (\o -> o { version = True }))
                "Print version information"
          , Option "h?" ["help"]
                (NoArg (\o -> o { help = True }))
                "Print this help"
          , Option "d" ["dry-run"]
                (NoArg (\o -> o { dryRun = True }))
                "Print commands, don't actually run them"
          , Option "j" ["jobs"]
                (OptArg ((\f o -> o { jobs = read f :: Int }) .  fromMaybe "jobs") "NUM")
                "Allow N jobs at once"
          , Option [] ["build-type"]
                (OptArg ((\f o -> o { buildType = Just (read f) }) .  fromMaybe "build-type") "type")
                "Specify the build type (Release, Debug)"
          , Option [] ["sandbox"]
                (OptArg ((\f o -> o { sandbox = Just f }) .  fromMaybe "sandbox") "DIR")
                "Shared sandbox among Haskell tools/libs"
          , Option [] ["cxx-comp"]
                (OptArg ((\f o -> o { cxxComp = Just f }) .  fromMaybe "cxx-comp") "COMP")
                "Compiler to use for C++ programs"
          , Option [] ["c-comp"]
                (OptArg ((\f o -> o { ccComp = Just f }) .  fromMaybe "c-comp") "COMP")
                "Compiler to use for C programs"]


helpBanner :: String
helpBanner = "[ITEMS] = COMMAND [TARGETS]\n" <> "\n" <> "Commands:\n" <>
    "  configure                     Prepare to build PFQ framework.\n" <>
    "  build                         Build PFQ framework.\n" <>
    "  install                       Copy the files into the install location.\n" <>
    "  clean                         Clean up after a build.\n" <>
    "  distclean                     Clean up additional files/dirs.\n" <>
    "  show                          Show targets.\n" <> "\n" <> "Options:"


-- data types...

data Target = Configure { getTargetName :: String}
            | Build { getTargetName :: String}
            | Install { getTargetName :: String}
            | Clean { getTargetName :: String}
            | DistClean { getTargetName :: String}


instance Show Target where
    show (Configure x) = "configure " ++ x
    show (Build x)     = "build " ++ x
    show (Install x)   = "install " ++ x
    show (Clean x)     = "clean " ++ x
    show (DistClean x) = "distclean " ++ x


instance Eq Target where
    (Configure a) == (Configure b)  = a == b || a == "*" || b == "*"
    (Build a)     == (Build b)      = a == b || a == "*" || b == "*"
    (Install a)   == (Install b)    = a == b || a == "*" || b == "*"
    (Clean a)     == (Clean b)      = a == b || a == "*" || b == "*"
    (DistClean a) == (DistClean b)  = a == b || a == "*" || b == "*"
    _ == _ = False


data Command = StaticCmd String | DynamicCmd (Options -> String)

instance Show Command where
    show (StaticCmd xs) = xs
    show (DynamicCmd f) = f defaultOptions

evalCmd :: Options -> Command -> String
evalCmd _ (StaticCmd xs) = xs
evalCmd opt (DynamicCmd fun) = fun opt


execCmd :: Options -> Command -> IO ExitCode
execCmd opt cmd' = let raw = evalCmd opt cmd'
    in system raw


data BuildType = Release | Debug
    deriving (Data, Typeable, Show, Read, Eq)

type ActionLog = ([Command], [Target])

newtype Action a = Action { runAction :: Writer ActionLog a }
    deriving(Functor, Applicative, Monad, MonadWriter ActionLog)

instance (Show a) => Show (Action a) where
    show act = (\(cs, ts) -> show cs ++ ": " ++ show ts) $ (runWriter . runAction) act

data Component = Component { getTarget :: Target,  getActionInfo :: ActionInfo }

data ActionInfo = ActionInfo { basedir :: FilePath, action :: Action () }


type BuilderScript = Writer Script ()

type Script = [Component]

type BuilderT = RWST Options () [Target]


infixr 0 *>>

(*>>) :: Target -> ActionInfo -> Writer [Component] ()
t *>> r = tell [Component t r]


into :: FilePath -> Action () -> ActionInfo
into = ActionInfo


requires :: Action () -> [Target] -> Action ()
ac `requires` xs = ac >> Action (tell ([],xs))


buildTargets :: [Target] -> Script -> FilePath -> Int -> BuilderT IO ()
buildTargets tgts script baseDir level = do
    opt <- ask
    let targets = map getTarget script
    let script' = filter (\(Component tar' _) -> tar' `elem` tgts) script

    when (length tgts > length script') $
        liftIO $ error ("SimpleBuilder: " ++ unwords (map getTargetName $ filter (`notElem` targets) tgts) ++ ": target not found!")

    forM_ (zip [1 ..] script') $
        \(n,Component target (ActionInfo path action)) ->
             do let (cmds',deps') = execWriter $ runAction action
                done <- get
                unless (target `elem` done) $
                    do put (target : done)
                       putStrLnVerbose Nothing $ replicate level '.' ++ "[" ++ show n ++ "/" ++ show (length script') ++ "] " ++ show target ++ ":"
                       -- satisfy dependencies
                       unless
                           (null deps') $
                           do putStrLnVerbose
                                  (Just $ verbose opt) $ "# Satisfying dependencies for " ++ show target ++ ": " ++ show deps'
                              forM_ deps' $
                                  \t -> when (t `notElem` done) $
                                        buildTargets [t] script baseDir (level + 1)
                       putStrLnVerbose
                           (Just $ verbose opt) $ "# Building target " ++ show target ++ ": " ++ show (map (evalCmd opt) cmds')
                       liftIO $
                           do -- set working dir...
                              let workDir = dropTrailingPathSeparator $ baseDir </> path
                              cur <- getCurrentDirectory
                              when (cur /= workDir) $
                                  do setCurrentDirectory workDir
                                     when (dryRun opt || verbose opt) $
                                         putStrLn $ "cd " ++ workDir
                              -- build target
                              if dryRun opt
                                  then void $
                                       mapM (putStrLn . evalCmd opt) cmds'
                                  else void $
                                       do ec <- sequenceWhile (== ExitSuccess) $ map (execCmd opt) cmds'
                                          when (length ec /= length cmds') $
                                            let show_cmd (c,e) = show c ++ " -> (" ++ show e ++ ")" in
                                                error ("SimpleBuilder: " ++ show target ++ " aborted: '" ++ show (head (drop (length ec) cmds'))  ++ "' command failed!")



--  ...from monad extras

sequenceWhile :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceWhile _ [] = return []
sequenceWhile p (m:ms) = do
    a <- m
    if p a
        then do
            as <- sequenceWhile p ms
            return (a:as)
        else return []


putStrLnVerbose  :: Maybe Bool -> String -> BuilderT IO ()
putStrLnVerbose Nothing xs = liftIO $ putStrLn xs
putStrLnVerbose (Just v) xs = when v (liftIO $ putStrLn xs)


parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo "--help for additional information" options))


simpleBuilder :: BuilderScript -> [String] -> IO ()
simpleBuilder script' args = do
    (opt,cmds) <- parseOpts args
    baseDir <- getCurrentDirectory
    when (help opt) $
        putStrLn (usageInfo helpBanner options) >>
        exitSuccess
    when (version opt) $
        putStrLn ("SimpleBuilder " ++ verString) >>
        exitSuccess
    sb <- let sb = sandbox opt
          in if isJust sb
               then liftM Just $
                    checkDir (fromJust sb) >>
                    canonicalizePath
                        (fromJust sb)
               else return sb
    let script = execWriter script'

    E.catch (case cmds of
                ("configure":xs) -> evalRWST (buildTargets (map Configure (mkTargets xs)) script baseDir 0) opt { sandbox = sb } [] >> putStrLn "Done."
                ("build":xs)     -> evalRWST (buildTargets (map Build (mkTargets xs)) script baseDir 0)     opt { sandbox = sb } [] >> putStrLn "Done."
                ("install":xs)   -> evalRWST (buildTargets (map Install (mkTargets xs)) script baseDir 0)   opt { sandbox = sb } [] >> putStrLn "Done."
                ("clean":xs)     -> evalRWST (buildTargets (map Clean (mkTargets xs)) script baseDir 0)     opt { sandbox = sb } [] >> putStrLn "Done."
                ("distclean":xs) -> evalRWST (buildTargets (map DistClean (mkTargets xs)) script baseDir 0) opt { sandbox = sb } [] >> putStrLn "Done."
                ("show":_)       -> showTargets script
                _                -> putStr $ usageInfo helpBanner options)
        (\e -> setCurrentDirectory baseDir >> print (e :: E.SomeException))

    where mkTargets xs = if null xs
                            then ["*"]
                            else xs


checkDir :: FilePath -> IO ()
checkDir path = do
    v <- doesDirectoryExist path
    unless v $ error ("SimpleBuilder: " ++ path ++ " directory does not exist")


showTargets :: Script -> IO ()
showTargets script =
    putStrLn "targets:" >> mapM_ putStrLn (nub (map (\(Component t _) -> "    " ++ getTargetName t) script))


{-# NOINLINE numberOfPhyCores #-}
numberOfPhyCores :: Int
numberOfPhyCores = unsafePerformIO $
    liftM (length . filter (isInfixOf "processor") . lines) $ readFile "/proc/cpuinfo"


