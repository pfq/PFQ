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

module Development.SimpleBuilder (
    Target(..),
    Script,
    (*>>),
    (.|.),
    into,
    simpleBuilder,
    cabalConfigure,
    cabalBuild,
    cabalInstall,
    cabalClean,
    numberOfPhyCores,
) where

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

import Data.List

bold    = setSGRCode [SetConsoleIntensity BoldIntensity]
reset   = setSGRCode []

data Target = Configure { getTargetName :: String } |
              Build     { getTargetName :: String } |
              Install   { getTargetName :: String } |
              Clean     { getTargetName :: String }


data Options =
    Options
    {
        dryRun :: Bool

    } deriving (Eq, Show)

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


data Action    = Action { basedir :: FilePath, cmds :: [String], deps :: [Target] }
data Component = Component { getTarget :: Target,  getAction :: Action }
type Script    = [Component]


type ScriptT = StateT (Script, [Target], Options)


infix 1 *>>

(*>>) :: Target -> Action -> Component
t *>> r = Component t r

into :: FilePath -> [String] -> Action
into dir cs = Action dir cs []

(.|.) :: Action -> [Target] -> Action
action .|. ts = action{ deps = ts }


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
            then void ( lift $ do
                            putStrLn $ "cd " ++ base </> path
                            mapM putStrLn cmds'
                      )
            else void ( lift $ do
                            setCurrentDirectory $ base </> path
                            ec <- mapM system cmds'
                            unless (all (== ExitSuccess) ec) $
                                error ("Error: " ++ show target ++ " aborted!")
                       )


simpleBuilder :: Script -> [String] -> IO ()
simpleBuilder script args = do
    base <- getCurrentDirectory
    E.catch (case args of
            ("configure":xs) -> evalStateT (buildTarget (map Configure (mkTargets xs)) base 0) (script,[], Options ("--dry-run" `elem` xs)) >> putStrLn ( bold ++ "Done." ++ reset )
            ("build":xs)     -> evalStateT (buildTarget (map Build     (mkTargets xs)) base 0) (script,[], Options ("--dry-run" `elem` xs)) >> putStrLn ( bold ++ "Done." ++ reset )
            ("install":xs)   -> evalStateT (buildTarget (map Install   (mkTargets xs)) base 0) (script,[], Options ("--dry-run" `elem` xs)) >> putStrLn ( bold ++ "Done." ++ reset )
            ("clean":xs)     -> evalStateT (buildTarget (map Clean     (mkTargets xs)) base 0) (script,[], Options ("--dry-run" `elem` xs)) >> putStrLn ( bold ++ "Done." ++ reset )
            ("show":_)       -> showTargets script
            _                -> usage)
          (\e -> setCurrentDirectory base >> print (e :: E.SomeException))
    where mkTargets xs = let xs' = filter (/= "--dry-run") xs
                         in if null xs'
                            then ["*"]
                            else xs'

showTargets :: Script -> IO ()
showTargets script =
    putStrLn "targets:" >> mapM_ putStrLn (nub (map (\(Component t _) -> "    " ++ getTargetName t) script))


usage = putStrLn $ "usage: Build COMMAND [TARGET TARGET...] [--dry-run]\n\n" ++
                   "Commands:\n" ++
                   "    configure   Prepare to build PFQ framework.\n" ++
                   "    build       Build PFQ framework.\n" ++
                   "    install     Copy the files into the install location.\n" ++
                   "    clean       Clean up after a build.\n" ++
                   "    show        Show targets."


cabalConfigure = "runhaskell Setup configure --user"
cabalBuild     = "runhaskell Setup build"
cabalInstall   = "runhaskell Setup install"
cabalClean     = "runhaskell Setup clean"


{-# NOINLINE numberOfPhyCores #-}
numberOfPhyCores :: Int
numberOfPhyCores = unsafePerformIO $ readFile "/proc/cpuinfo" >>= \file ->
    return $ (length . filter (isInfixOf "processor") . lines) file


