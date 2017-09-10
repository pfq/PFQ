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
{-# LANGUAGE RecordWildCards #-}

module Development.SimpleBuilder (

      Target(..)
    , Options(..)
    , BuilderScript
    , Command

    , objective

    , config
    , build
    , install
    , clean
    , distclean

    , configOf
    , buildOf
    , installOf
    , cleanOf
    , distcleanOf

    , req
    , reqs
    , simpleBuilder
    , defaultOptions
    , numberOfPhyCores

    -- predefined actions
    , Development.SimpleBuilder.empty
    , cabalConfigure
    , cabalBuild
    , cabalInstall
    , cabalClean
    , cabalDistClean
    , stackConfigure
    , stackBuild
    , stackInstall
    , stackClean
    , cmake
    , cmake_distclean
    , make
    , make_install
    , make_clean
    , make_distclean
    , ldconfig
    , configure
    , cmd

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

verString = "v0.5" :: String


-- predefined commands

empty = return () :: Action ()


cabalConfigure = tellCmd $
    DynamicCmd $ \Options{..} -> case () of
                    _ | stack             -> "stack setup"
                      | Just p <- sandbox -> "cabal sandbox init --sandbox=" ++ p ++ " && cabal configure"
                      | otherwise         -> "runhaskell Setup configure --user"

cabalBuild = tellCmd $
    DynamicCmd $ \Options{..} -> case () of
                    _ | stack             -> "stack build"
                      | Just _ <- sandbox -> "cabal build"
                      | otherwise         -> "runhaskell Setup build"

cabalInstall = tellCmd $
    DynamicCmd $ \Options{..} -> case () of
                    _ | stack             -> "stack install"
                      | Just _ <- sandbox -> "cabal build"
                      | otherwise         -> "runhaskell Setup install"

cabalClean = tellCmd $
    DynamicCmd $ \Options{..} -> case () of
                    _ | stack             -> "stack clean"
                      | Just _ <- sandbox -> "cabal clean"
                      | otherwise         -> "runhaskell Setup clean"


cabalDistClean  = tellCmd $ StaticCmd "rm -rf dist; rm -f cabal.sandbox.config; rm -rf .stack-work"


make_install    = tellCmd $ StaticCmd "make install"
make_clean      = tellCmd $ StaticCmd "make clean"
make_distclean  = tellCmd $ StaticCmd "make distclean"
ldconfig        = tellCmd $ StaticCmd "ldconfig"
configure       = tellCmd $ StaticCmd "./configure"


stackConfigure  = tellCmd $ StaticCmd "stack setup"
stackBuild      = tellCmd $ StaticCmd "stack build"
stackInstall    = tellCmd $ StaticCmd "stack install"
stackClean      = tellCmd $ StaticCmd "stack clean"
stackDistClean  = tellCmd $ StaticCmd "rm -rf .stack-work"


make = tellCmd $
    DynamicCmd
        (\Options{..} ->
              case () of
                  _   | jobs == 0 -> "make"
                      | jobs > numberOfPhyCores -> "make -j " ++ show (numberOfPhyCores + 1)
                      | otherwise -> "make -j " ++ show jobs)

cmake = tellCmd $
    DynamicCmd
        (\Options{..} ->
              let build = case buildType of
                          Nothing -> ""
                          Just Release -> "-DCMAKE_BUILD_TYPE=Release"
                          Just Debug   -> "-DCMAKE_BUILD_TYPE=Debug"
                  cc = case ccComp of
                          Nothing -> ""
                          Just xs -> "-DCMAKE_C_COMPILER=" ++ xs
                  cxx = case cxxComp of
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


-- Options...

data Options = Options
    {   buildType  :: Maybe BuildType
    ,   cxxComp    :: Maybe String
    ,   ccComp     :: Maybe String
    ,   sandbox    :: Maybe String
    ,   stack      :: Bool
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
    ,   stack     =  False
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
          , Option "s" ["stack"]
                (NoArg (\o -> o { stack = True }))
                "Use the stack tool"
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


banner :: String
banner = "[ITEMS] = COMMAND [TARGETS]\n" <> "\n" <> "Commands:\n" <>
    "  config                        Prepare to build PFQ framework.\n" <>
    "  build                         Build PFQ framework.\n" <>
    "  install                       Copy the files into the install location.\n" <>
    "  clean                         Clean up after a build.\n" <>
    "  distclean                     Clean up additional files/dirs.\n" <>
    "  show                          Show targets.\n" <> "\n" <> "Options:"


-- data types...


data Target =  Config    { getTargetName :: String}
            |  Build     { getTargetName :: String}
            |  Install   { getTargetName :: String}
            |  Clean     { getTargetName :: String}
            |  DistClean { getTargetName :: String}


instance Show Target where
    show (Config x)    = "config " ++ x
    show (Build x)     = "build " ++ x
    show (Install x)   = "install " ++ x
    show (Clean x)     = "clean " ++ x
    show (DistClean x) = "distclean " ++ x


instance Eq Target where
    (Config a)    == (Config b)    = a == b || a == "*" || b == "*"
    (Build a)     == (Build b)     = a == b || a == "*" || b == "*"
    (Install a)   == (Install b)   = a == b || a == "*" || b == "*"
    (Clean a)     == (Clean b)     = a == b || a == "*" || b == "*"
    (DistClean a) == (DistClean b) = a == b || a == "*" || b == "*"
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
    in case raw of
         "" -> return ExitSuccess
         _  -> system raw


data BuildType = Release | Debug
    deriving (Data, Typeable, Show, Read, Eq)


data Objective = Objective
    {
        getObjectiveName :: String
    ,   getFolder        :: FilePath
    ,   getScript        :: Writer [Component] ()
    }

data Component = Component
    {  getTarget :: Target
    ,  getActionInfo :: ActionInfo
    } deriving (Show)


data ActionInfo = ActionInfo
    { basedir :: FilePath
    , action :: Action ()
    } deriving (Show)

newtype Action a = Action { runAction :: Writer ActionLog a }
    deriving(Functor, Applicative, Monad, MonadWriter ActionLog)

instance (Show a) => Show (Action a) where
    show act = (\(cs, ts) -> show cs ++ ": " ++ show ts) $ (runWriter . runAction) act

type ActionLog = ([Command], [Target])

config, build, install, clean, distclean  :: Action () -> Writer [Component] ()

config    act = tell [Component (Config    "") (ActionInfo "" act)]
build     act = tell [Component (Build     "") (ActionInfo "" act)]
install   act = tell [Component (Install   "") (ActionInfo "" act)]
clean     act = tell [Component (Clean     "") (ActionInfo "" act)]
distclean act = tell [Component (DistClean "") (ActionInfo "" act)]


objective :: String -> FilePath -> Writer [Component] () -> Writer [Objective] ()
objective name path cs = tell [Objective name path cs]


type BuilderScript = Writer [Objective] ()

type BuilderT = RWST Options () [Target]


configOf    = Config
buildOf     = Build
installOf   = Install
cleanOf     = Clean
distcleanOf = DistClean


req :: Action () -> Target -> Action ()
ac `req` x = ac >> Action (tell ([], [x]))

reqs :: Action () -> [Target] -> Action ()
ac `reqs` xs = ac >> Action (tell ([],xs))


runBuilders :: [Target] -> [Component] -> FilePath -> Int -> BuilderT IO ()
runBuilders ts script baseDir level = do
    opt <- ask
    let targets = map getTarget script
    let script' = filter (\(Component tar' _) -> tar' `elem` ts) script

    when (length ts > length script') $
        liftIO $ error ("SimpleBuilder: " ++ unwords (map getTargetName $ filter (`notElem` targets) ts) ++ ": target not found!")

    forM_ (zip [1 ..] script') $ \(n,Component target (ActionInfo path action)) ->
        do let (cmds',deps') = execWriter $ runAction action
           done <- get
           unless (target `elem` done) $
               do put (target : done)
                  putStrLnVerbose Nothing $ replicate level '.' ++ "[" ++ show n ++ "/" ++ show (length script') ++ "] " ++ show target ++ ":"
                  -- satisfy dependencies
                  unless (null deps') $
                      do putStrLnVerbose
                             (Just $ verbose opt) $ "# Satisfying dependencies for " ++ show target ++ ": " ++ show deps'
                         forM_ deps' $
                             \t -> when (t `notElem` done) $
                                   runBuilders [t] script baseDir (level + 1)
                  putStrLnVerbose (Just $ verbose opt) $ "# Building target '" ++ show target ++ "': " ++ show (map (evalCmd opt) cmds')
                  liftIO $ do -- set working dir...
                      let workDir = dropTrailingPathSeparator $ baseDir </> path
                      cur <- getCurrentDirectory
                      when (cur /= workDir) $ do
                          setCurrentDirectory workDir
                          when (dryRun opt || verbose opt) $
                              putStrLn $ "cd " ++ workDir

                      -- build target
                      if dryRun opt
                          then mapM_ (putStrLn . evalCmd opt) cmds'
                          else void $ do
                              ec <- sequenceWhile (== ExitSuccess) $ map (execCmd opt) cmds'
                              when (length ec /= length cmds') $
                                let show_cmd (c,e) = show c ++ " -> (" ++ show e ++ ")" in
                                    error ("SimpleBuilder: " ++ show target ++ " aborted: '" ++ show (head (drop (length ec) cmds'))  ++ "' command failed!")


simpleBuilder :: BuilderScript -> Options -> [String] -> IO ()
simpleBuilder script' opt' args = do

    (opt, cmds) <- parseOpts args opt'

    when (help opt) $
        putStrLn (usageInfo banner options) *> exitSuccess
    when (version opt) $
        putStrLn ("SimpleBuilder " ++ verString) *> exitSuccess

    let objs = execWriter script'

    when (verbose opt) $
        mapM_ print $ concatMap mkComponents objs

    simpleBuilder' (concatMap mkComponents objs) opt cmds

    where mkComponents :: Objective -> [Component]
          mkComponents Objective{..} =
            map (\(Component target ai) -> Component target{getTargetName = getObjectiveName} ai{basedir = getFolder}) (snd $ runWriter getScript)



simpleBuilder' :: [Component] -> Options -> [String] -> IO ()
simpleBuilder' script opt cmds = do

    baseDir <- getCurrentDirectory

    sb <- let sb = sandbox opt
          in if isJust sb
                then fmap Just $
                    checkDir (fromJust sb) >>
                    canonicalizePath
                        (fromJust sb)
               else return sb

    E.catch (case cmds of
               ("config":xs)    -> evalRWST (runBuilders (map Config    (mkTargets xs)) script baseDir 0) opt { sandbox = sb } [] >> putStrLn "Done."
               ("build":xs)     -> evalRWST (runBuilders (map Build     (mkTargets xs)) script baseDir 0) opt { sandbox = sb } [] >> putStrLn "Done."
               ("install":xs)   -> evalRWST (runBuilders (map Install   (mkTargets xs)) script baseDir 0) opt { sandbox = sb } [] >> putStrLn "Done."
               ("clean":xs)     -> evalRWST (runBuilders (map Clean     (mkTargets xs)) script baseDir 0) opt { sandbox = sb } [] >> putStrLn "Done."
               ("distclean":xs) -> evalRWST (runBuilders (map DistClean (mkTargets xs)) script baseDir 0) opt { sandbox = sb } [] >> putStrLn "Done."
               ("show":_)       -> showTargets script
               _                -> putStr $ usageInfo banner options)
        (\e -> setCurrentDirectory baseDir >> print (e :: E.SomeException))

    where mkTargets xs = if null xs
                            then ["*"]
                            else xs


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



checkDir :: FilePath -> IO ()
checkDir path = do
    v <- doesDirectoryExist path
    unless v $ error ("SimpleBuilder: " ++ path ++ " directory does not exist")


showTargets :: [Component] -> IO ()
showTargets script =
    putStrLn "targets:" >> mapM_ putStrLn (nub (map (\(Component t _) -> "    " ++ getTargetName t) script))


{-# NOINLINE numberOfPhyCores #-}
numberOfPhyCores :: Int
numberOfPhyCores = unsafePerformIO $
    (length . filter (isInfixOf "processor") . lines) <$> readFile "/proc/cpuinfo"


parseOpts :: [String] -> Options -> IO (Options, [String])
parseOpts argv opt =
    case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) opt o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo "--help for additional information" options))

