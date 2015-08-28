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
    cmake,
    make,
    make_install,
    make_clean,
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
import Control.Monad.State
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
make_install       = tellAction $ BareCmd "make install"
make_clean         = tellAction $ BareCmd "make clean"
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
  "    show        Show targets.", ""]


bold  = setSGRCode [SetConsoleIntensity BoldIntensity]
reset = setSGRCode []


data Target = Configure { getTargetName :: String } |
              Build     { getTargetName :: String } |
              Install   { getTargetName :: String } |
              Clean     { getTargetName :: String }


instance Show Target where
    show (Configure x) = "configure " ++ x
    show (Build     x) = "build " ++ x
    show (Install   x) = "install " ++ x
    show (Clean     x) = "clean " ++ x

instance Eq Target where
    (Configure a) == (Configure b) = a == b || a == "*" || b == "*"
    (Build a)     == (Build b)     = a == b || a == "*" || b == "*"
    (Install a)   == (Install b)   = a == b || a == "*" || b == "*"
    (Clean a)     == (Clean b)     = a == b || a == "*" || b == "*"
    _ == _ = False


data Command = BareCmd String | AdornedCmd (Options -> String)


evalCmd :: Options -> Command -> String
evalCmd _   (BareCmd xs) = xs
evalCmd opt (AdornedCmd fun) = fun opt


runCmd :: Options -> Command -> IO ExitCode
runCmd opt cmd = let raw = evalCmd opt cmd in system raw


data BuildType = Release | Debug
    deriving (Data, Typeable, Show, Read, Eq)


newtype Action a = Action { getAction :: Writer ([Command], [Target]) a }
    deriving(Functor, Applicative, Monad)

data Component = Component { getTarget :: Target,  getActionInfo :: ActionInfo }

data ActionInfo = ActionInfo { basedir :: FilePath, action :: Action () }

type BuilderScript  = Writer Script ()

type Script  = [Component]

type ScriptT = StateT (Script, [Target], Options)


infixr 0 *>>

(*>>) :: Target -> ActionInfo -> Writer [Component] ()
t *>> r = tell [Component t r]


into :: FilePath -> Action () -> ActionInfo
into = ActionInfo


(.<) :: Action () -> Target -> Action ()
ac .< x = ac >> Action (tell ([],[x]))


requires :: Action () -> [Target] -> Action ()
ac `requires` xs = ac >> Action (tell ([],xs))


buildTargets :: [Target] -> FilePath -> Int -> ScriptT IO ()
buildTargets tgts baseDir level = do

    (script, done, _) <- get

    let targets = map getTarget script
    let script' = filter (\(Component tar' _ ) -> tar' `elem` tgts) script

    when (length tgts > length script') $
        lift $ error ("SimpleBuilder: " ++ unwords (
            map getTargetName $ filter (`notElem` targets) tgts)  ++ ": target not found!")

    let tot = length script'

    forM_ (zip [1..] script') $ \(n, Component target (ActionInfo path action)) -> do

        let (cmds',deps') = execWriter $ getAction action

        (_, done', opt) <- get
        unless (target `elem` done') $ do

            put (script, target : done', opt)

            unless (dryRun opt) $
                lift $ putStrLn $ replicate level '.' ++ bold ++ "[" ++ show n ++ "/" ++ show tot ++ "] " ++ show target ++ ":" ++ reset

            -- satisfy dependencies
            unless (null deps') $ do
                lift $ putStrLnVerbose (verbose opt) (bold ++ "# Satisfying dependencies for " ++ show target ++ ": " ++ show deps' ++ reset)
                forM_ deps' $ \t -> when (t `notElem` done') $ buildTargets [t] baseDir (level+1)

            lift $ putStrLnVerbose (verbose opt) (bold ++ "# Building target " ++ show target ++ ": " ++ show (map (evalCmd opt) cmds') ++ reset)

            -- build target
            if dryRun opt
            then void . lift $ do
                       putStrLn $ "cd " ++ baseDir </> path
                       mapM (putStrLn . evalCmd opt) cmds'
            else void . lift $ do
                        setCurrentDirectory $ baseDir </> path
                        ec <- mapM (runCmd opt) cmds'
                        unless (all (== ExitSuccess) ec) $
                             error ("SimpleBuilder: " ++ show target ++ " aborted!")


putStrLnVerbose :: Bool -> String -> IO ()
putStrLnVerbose verb xs = when verb $ putStrLn xs


simpleBuilder :: BuilderScript -> [String] -> IO ()
simpleBuilder script' args = do

    let script = execWriter script'

    opt  <- cmdArgsRun options
    baseDir <- getCurrentDirectory

    E.catch (case extra opt of
            ("configure":xs) -> evalStateT (buildTargets (map Configure (mkTargets xs)) baseDir 0) (script, [], opt) >> putStrLn ( bold ++ "Done." ++ reset )
            ("build":xs)     -> evalStateT (buildTargets (map Build     (mkTargets xs)) baseDir 0) (script, [], opt) >> putStrLn ( bold ++ "Done." ++ reset )
            ("install":xs)   -> evalStateT (buildTargets (map Install   (mkTargets xs)) baseDir 0) (script, [], opt) >> putStrLn ( bold ++ "Done." ++ reset )
            ("clean":xs)     -> evalStateT (buildTargets (map Clean     (mkTargets xs)) baseDir 0) (script, [], opt) >> putStrLn ( bold ++ "Done." ++ reset )
            ("show":_)       -> showTargets script
            _                -> putStr $ show $ helpText [] HelpFormatDefault options)
        (\e -> setCurrentDirectory baseDir >> print (e :: E.SomeException))
    where mkTargets xs =  if null xs then ["*"] else xs


showTargets :: Script -> IO ()
showTargets script =
    putStrLn "targets:" >> mapM_ putStrLn (nub (map (\(Component t _) -> "    " ++ getTargetName t) script))


{-# NOINLINE numberOfPhyCores #-}
numberOfPhyCores :: Int
numberOfPhyCores = unsafePerformIO $ readFile "/proc/cpuinfo" >>= \file ->
    return $ (length . filter (isInfixOf "processor") . lines) file


