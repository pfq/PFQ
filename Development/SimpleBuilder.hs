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
    (.<),
    requires,
    into,
    simpleBuilder,
    numberOfPhyCores,

    -- predefined actions
    Development.SimpleBuilder.empty,
    cabalConfigureUser,
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


import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Console.ANSI
import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO.Unsafe

import Control.Monad
import qualified Control.Monad.Trans.RWS.Lazy as RWS
import Control.Monad.Writer.Lazy
import Control.Applicative

import qualified Control.Exception as E

import Data.Data
import Data.Monoid
import Data.List
import Data.Maybe
import Data.String


-- version

version = "0.2"


-- predefined commands

empty              = return () :: Action ()
cabalConfigureUser = tellAction $ BareCmd "runhaskell Setup configure --user"
cabalBuild         = tellAction $ BareCmd "runhaskell Setup build"
cabalInstall       = tellAction $ BareCmd "runhaskell Setup install"
cabalClean         = tellAction $ BareCmd "runhaskell Setup clean"
cabalDistClean     = tellAction $ BareCmd "rm -rf dist"
make_install       = tellAction $ BareCmd "make install"
make_clean         = tellAction $ BareCmd "make clean"
make_distclean     = tellAction $ BareCmd "make distclean"
ldconfig           = tellAction $ BareCmd "ldconfig"
configure          = tellAction $ BareCmd "./configure"


make   = tellAction $ AdornedCmd (\o -> case () of
                            _ | jobs o == 0               -> "make"
                              | jobs o > numberOfPhyCores -> "make -j " ++ show (numberOfPhyCores + 1)
                              | otherwise                 -> "make -j " ++ show (jobs o))


cmake  = tellAction $ AdornedCmd (\o ->
                    let build = case buildType o of
                                    Nothing      -> ""
                                    Just Release -> "-DCMAKE_BUILD_TYPE=Release"
                                    Just Debug   -> "-DCMAKE_BUILD_TYPE=Debug"
                        cc  = case ccComp o of
                                Nothing  -> ""
                                Just xs  -> "-DCMAKE_C_COMPILER=" ++ xs
                        cxx = case cxxComp o of
                                Nothing  -> ""
                                Just xs  -> "-DCMAKE_CXX_COMPILER=" ++ xs
                        in unwords [ "cmake" , build, cc, cxx, "." ]
                    )


cmake_distclean :: Action ()
cmake_distclean = Action $ do
    tell ([BareCmd "rm -f install_manifest.txt"], [])
    tell ([BareCmd "rm -f cmake.depends"], [])
    tell ([BareCmd "rm -f cmake.chek_depends"],[])
    tell ([BareCmd "rm -f CMakeCache.txt"],[])
    tell ([BareCmd "rm -f *.cmake"],[])
    tell ([BareCmd "rm -f Makefile"],[])
    tell ([BareCmd "rm -rf CMakeFiles"],[])


cmd :: String -> Action ()
cmd xs = Action (tell ([BareCmd xs], []))


tellAction :: Command -> Action ()
tellAction c = Action (tell ([c], []))


-- data types

data Options = Options
    {   buildType  :: Maybe BuildType
    ,   cxxComp    :: Maybe String
    ,   ccComp     :: Maybe String
    ,   dryRun     :: Bool
    ,   verbose    :: Bool
    ,   jobs       :: Int
    ,   extra      :: [String]
    } deriving (Data, Typeable, Show, Read)


options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {   buildType = Nothing    &= explicit &= name "buildType"     &= help "Specify the build type (Release, Debug)"
    ,   cxxComp   = Nothing    &= explicit &= name "cxx"           &= help "Compiler to use for C++ programs"
    ,   ccComp    = Nothing    &= explicit &= name "cc"            &= help "Compiler to use for C programs"
    ,   dryRun    = False      &= explicit &= name "dry-run"       &= help "Print commands, don't actually run them"
    ,   verbose   = False      &= explicit &= name "verbose"       &= help "Verbose mode"
    ,   jobs      = 0          &= help "Allow N jobs at once (if possible)"
    ,   extra     = []         &= typ "ITEMS" &= args
    } &= summary ("SimpleBuilder " ++ version) &= program "Build" &= details detailsBanner


detailsBanner = [ "[ITEMS] = COMMAND [TARGETS]",
  "",
  "Commands:",
  "    configure   Prepare to build PFQ framework.",
  "    build       Build PFQ framework.",
  "    install     Copy the files into the install location.",
  "    clean       Clean up after a build.",
  "    distclean   Clean up additional files/dirs.",
  "    show        Show targets.", ""]


bold  = setSGRCode [SetConsoleIntensity BoldIntensity]
reset = setSGRCode []


data Target = Configure { getTargetName :: String } |
              Build     { getTargetName :: String } |
              Install   { getTargetName :: String } |
              Clean     { getTargetName :: String } |
              DistClean { getTargetName :: String }

instance Show Target where
    show (Configure x) = "configure " ++ x
    show (Build     x) = "build " ++ x
    show (Install   x) = "install " ++ x
    show (Clean     x) = "clean " ++ x
    show (DistClean x) = "distclean " ++ x

instance Eq Target where
    (Configure a) == (Configure b) = a == b || a == "*" || b == "*"
    (Build a)     == (Build b)     = a == b || a == "*" || b == "*"
    (Install a)   == (Install b)   = a == b || a == "*" || b == "*"
    (Clean a)     == (Clean b)     = a == b || a == "*" || b == "*"
    (DistClean a) == (DistClean b) = a == b || a == "*" || b == "*"
    _ == _ = False


data Command = BareCmd String | AdornedCmd (Options -> String)

instance Show Command where
    show (BareCmd xs) = xs
    show (AdornedCmd f) = f (Options Nothing Nothing Nothing False False 0 [])


evalCmd :: Options -> Command -> String
evalCmd _   (BareCmd xs) = xs
evalCmd opt (AdornedCmd fun) = fun opt


runCmd :: Options -> Command -> IO ExitCode
runCmd opt cmd = let raw = evalCmd opt cmd in system raw


data BuildType = Release | Debug
    deriving (Data, Typeable, Show, Read, Eq)

newtype Action a = Action { getAction :: Writer ([Command], [Target]) a }
    deriving(Functor, Applicative, Monad)

instance (Show a) => Show (Action a) where
    show action =  (\(cs, ts) -> show cs ++ ": " ++ show ts) $ runWriter (getAction action)

data Component = Component { getTarget :: Target,  getActionInfo :: ActionInfo }

data ActionInfo = ActionInfo { basedir :: FilePath, action :: Action () }

type BuilderScript = Writer Script ()

type Script = [Component]

type BuilderT = RWS.RWST Options () [Target]


infixr 0 *>>

(*>>) :: Target -> ActionInfo -> Writer [Component] ()
t *>> r = tell [Component t r]


into :: FilePath -> Action () -> ActionInfo
into = ActionInfo


(.<) :: Action () -> Target -> Action ()
ac .< x = ac >> Action (tell ([],[x]))


requires :: Action () -> [Target] -> Action ()
ac `requires` xs = ac >> Action (tell ([],xs))


buildTargets :: [Target] -> Script -> FilePath -> Int -> BuilderT IO ()
buildTargets tgts script baseDir level = do

    opt <- RWS.ask

    let targets = map getTarget script
    let script' = filter (\(Component tar' _ ) -> tar' `elem` tgts) script

    when (length tgts > length script') $
        liftIO $ error ("SimpleBuilder: " ++ unwords (
            map getTargetName $ filter (`notElem` targets) tgts)  ++ ": target not found!")

    forM_ (zip [1..] script') $ \(n, Component target (ActionInfo path action)) -> do

        let (cmds',deps') = execWriter $ getAction action

        done <- RWS.get

        unless (target `elem` done) $ do

            RWS.put (target : done)

            putStrLnVerbose Nothing $ replicate level '.' ++ bold ++ "[" ++ show n ++ "/" ++ show (length script') ++ "] " ++ show target ++ ":" ++ reset

            -- satisfy dependencies

            unless (null deps') $ do
                putStrLnVerbose (Just $ verbose opt) $ bold ++ "# Satisfying dependencies for " ++ show target ++ ": " ++ show deps' ++ reset
                forM_ deps' $ \t -> when (t `notElem` done) $ buildTargets [t] script baseDir (level+1)

            putStrLnVerbose (Just $ verbose opt) $ bold ++ "# Building target " ++ show target ++ ": " ++ show (map (evalCmd opt) cmds') ++ reset

            liftIO $ do

                -- set working dir...

                let workDir = dropTrailingPathSeparator $ baseDir </> path

                cur <- getCurrentDirectory

                when (cur /= workDir) $ do
                    setCurrentDirectory workDir
                    when (dryRun opt || verbose opt) $ putStrLn $ "cd " ++ workDir

                -- build target

                if dryRun opt then void $ mapM (putStrLn . evalCmd opt) cmds'
                              else void $ do ec <- mapM (runCmd opt) cmds'
                                             unless (all (== ExitSuccess) ec) $
                                                error ("SimpleBuilder: " ++ show target ++ " aborted!")


putStrLnVerbose  :: Maybe Bool -> String -> BuilderT IO ()
putStrLnVerbose Nothing xs = liftIO $ putStrLn xs
putStrLnVerbose (Just v) xs = when v (liftIO $ putStrLn xs)


simpleBuilder :: BuilderScript -> [String] -> IO ()
simpleBuilder script' args = do

    let script = execWriter script'

    opt  <- cmdArgsRun options
    baseDir <- getCurrentDirectory

    E.catch (case extra opt of
            ("configure":xs) -> RWS.evalRWST (buildTargets (map Configure (mkTargets xs)) script baseDir 0) opt [] >> putStrLn ( bold ++ "Done." ++ reset )
            ("build":xs)     -> RWS.evalRWST (buildTargets (map Build     (mkTargets xs)) script baseDir 0) opt [] >> putStrLn ( bold ++ "Done." ++ reset )
            ("install":xs)   -> RWS.evalRWST (buildTargets (map Install   (mkTargets xs)) script baseDir 0) opt [] >> putStrLn ( bold ++ "Done." ++ reset )
            ("clean":xs)     -> RWS.evalRWST (buildTargets (map Clean     (mkTargets xs)) script baseDir 0) opt [] >> putStrLn ( bold ++ "Done." ++ reset )
            ("distclean":xs) -> RWS.evalRWST (buildTargets (map DistClean (mkTargets xs)) script baseDir 0) opt [] >> putStrLn ( bold ++ "Done." ++ reset )
            ("show":_)       -> showTargets script
            _                -> putStr $ show $ helpText [] HelpFormatDefault options)
        (\e -> setCurrentDirectory baseDir >> print (e :: E.SomeException))
    where mkTargets xs =  if null xs then ["*"] else xs


showTargets :: Script -> IO ()
showTargets script =
    putStrLn "targets:" >> mapM_ putStrLn (nub (map (\(Component t _) -> "    " ++ getTargetName t) script))


{-# NOINLINE numberOfPhyCores #-}
numberOfPhyCores :: Int
numberOfPhyCores = unsafePerformIO $
    liftM (length . filter (isInfixOf "processor") . lines) $ readFile "/proc/cpuinfo"


