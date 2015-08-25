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

module Development.SimpleBuilder (
    Target(..),
    Script,
    Command,
    Options,
    (*>>),
    (.|.),
    into,
    simpleBuilder,
    numberOfPhyCores,
    cabalConfigureUser,
    cabalBuild,
    cabalInstall,
    cabalClean,
    cmake,
    make,
    make_install,
    make_clean,
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

import qualified Control.Exception as E

import Data.Data
import Data.List
import Data.Maybe
import Data.String

version = "0.1"

-- Predefined commands

cabalConfigureUser = BareCmd "runhaskell Setup configure --user"
cabalBuild         = BareCmd "runhaskell Setup build"
cabalInstall       = BareCmd "runhaskell Setup install"
cabalClean         = BareCmd "runhaskell Setup clean"

make_install       = BareCmd "make install"
make_clean         = BareCmd "make clean"

make   = AdornedCmd (\o -> case () of
                            _ | jobs o == 0               -> "make"
                              | jobs o > numberOfPhyCores -> "make -j " ++ show (numberOfPhyCores + 1)
                              | otherwise                 -> "make -j " ++ show (jobs o))

cmake  = AdornedCmd (\o -> let build = case buildType o of
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

-- data types

data Options = Options
    {   buildType  :: Maybe BuildType
    ,   cxxComp    :: Maybe String
    ,   ccComp     :: Maybe String
    ,   dryRun     :: Bool
    ,   jobs       :: Int
    ,   extra      :: [String]
    } deriving (Data, Typeable, Show, Read)


options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
    {   buildType = Nothing    &= explicit &= name "buildType"     &= help "Specify the build type (Release, Debug)"
    ,   cxxComp   = Nothing    &= explicit &= name "cxx"           &= help "Compiler to use for C++ programs"
    ,   ccComp    = Nothing    &= explicit &= name "cc"            &= help "Compiler to use for C programs"
    ,   dryRun    = False      &= explicit &= name "dry-run"       &= help "Print commands, don't actually run them"
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

instance Eq Target where
    (Configure a) == (Configure b) = a == b || a == "*" || b == "*"
    (Build a)     == (Build b)     = a == b || a == "*" || b == "*"
    (Install a)   == (Install b)   = a == b || a == "*" || b == "*"
    (Clean a)     == (Clean b)     = a == b || a == "*" || b == "*"
    _ == _ = False


instance Show Target where
    show (Configure t) = "Configuring " ++ t
    show (Build     t) = "Building "    ++ t
    show (Install   t) = "Installing "  ++ t
    show (Clean     t) = "Cleaning "    ++ t


data Command = BareCmd String | AdornedCmd (Options -> String)

instance IsString Command where
    fromString = BareCmd


evalCmd :: Options -> Command -> String
evalCmd _   (BareCmd xs) = xs
evalCmd opt (AdornedCmd fun) = fun opt


data BuildType = Release | Debug
    deriving (Data, Typeable, Show, Read, Eq)

data Action    = Action { basedir :: FilePath, cmds :: [Command], deps :: [Target] }
data Component = Component { getTarget :: Target,  getAction :: Action }

type Script  = [Component]
type ScriptT = StateT (Script, [Target], Options)


infix 1 *>>

(*>>) :: Target -> Action -> Component
t *>> r = Component t r


into :: FilePath -> [Command] -> Action
into dir cs = Action dir cs []


(.|.) :: Action -> [Target] -> Action
action .|. ts = action { deps = ts }


buildTarget :: [Target] -> FilePath -> Int -> ScriptT IO ()
buildTarget tars base level = do
    (script, done, _) <- get
    let targets = map getTarget script
    let script' = filter (\(Component tar' _ ) -> tar' `elem` tars) script
    when (length tars > length script') $
        lift $ error ("Error: " ++ unwords (
            map getTargetName $ filter (`notElem` targets) tars)  ++ ": target(s) not found!")
    let tot = length script'
    forM_ (zip [1..] script') $ \(n, Component target (Action path cmds' deps')) -> do
        (_, done', opt) <- get
        unless (target `elem` done') $ do
            put (script, target : done', opt)

            unless (dryRun opt) $
                lift $ putStrLn $ replicate level '.' ++ bold ++ "[" ++ show n ++ "/" ++ show tot ++ "] " ++ show target ++ ":" ++ reset

            -- satisfy dependencies
            unless (null deps') $ do
                lift $ putStrLn $ "# Satisfying dependencies for " ++ show target ++ "..."
                forM_ deps' $ \t -> when (t `notElem` done') $ buildTarget [t] base (level+1)

            -- build target
            if dryRun opt
            then void . lift $ do
                       putStrLn $ "cd " ++ base </> path
                       mapM (putStrLn . evalCmd opt) cmds'
            else void . lift $ do
                        setCurrentDirectory $ base </> path
                        ec <- mapM (system . evalCmd opt) cmds'
                        unless (all (== ExitSuccess) ec) $
                             error ("Error: " ++ show target ++ " aborted!")


simpleBuilder :: Script -> [String] -> IO ()
simpleBuilder script args = do
    opt  <- cmdArgsRun options
    base <- getCurrentDirectory
    E.catch (case extra opt of
            ("configure":xs) -> evalStateT (buildTarget (map Configure (mkTargets xs)) base 0) (script,[], opt) >> putStrLn ( bold ++ "Done." ++ reset )
            ("build":xs)     -> evalStateT (buildTarget (map Build     (mkTargets xs)) base 0) (script,[], opt) >> putStrLn ( bold ++ "Done." ++ reset )
            ("install":xs)   -> evalStateT (buildTarget (map Install   (mkTargets xs)) base 0) (script,[], opt) >> putStrLn ( bold ++ "Done." ++ reset )
            ("clean":xs)     -> evalStateT (buildTarget (map Clean     (mkTargets xs)) base 0) (script,[], opt) >> putStrLn ( bold ++ "Done." ++ reset )
            ("show":_)       -> showTargets script
            _                -> putStr $ show $ helpText [] HelpFormatDefault options)
        (\e -> setCurrentDirectory base >> print (e :: E.SomeException))
    where mkTargets xs =  if null xs then ["*"] else xs


showTargets :: Script -> IO ()
showTargets script =
    putStrLn "targets:" >> mapM_ putStrLn (nub (map (\(Component t _) -> "    " ++ getTargetName t) script))


{-# NOINLINE numberOfPhyCores #-}
numberOfPhyCores :: Int
numberOfPhyCores = unsafePerformIO $ readFile "/proc/cpuinfo" >>= \file ->
    return $ (length . filter (isInfixOf "processor") . lines) file

