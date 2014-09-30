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

module SimpleBuilder (
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
) where

import System.Console.ANSI
import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Exit

import Control.Monad
import Control.Monad.State

import qualified Control.Exception as E

bold    = setSGRCode [SetConsoleIntensity BoldIntensity]
reset   = setSGRCode []

data Target = Configure String |
              Build String |
              Install String |
              Clean String


instance Eq Target where
    (Configure a) == (Configure b) = a == b || a == "*" || b == "*"
    (Build a)     == (Build b)     = a == b || a == "*" || b == "*"
    (Install a)   == (Install b)   = a == b || a == "*" || b == "*"
    (Clean a)     == (Clean b)     = a == b || a == "*" || b == "*"
    _ == _ = False


instance Show Target where
    show (Configure t) = "Configuring " ++ t
    show (Build     t) = "Building " ++ t
    show (Install   t) = "Installing " ++ t
    show (Clean     t) = "Cleaning " ++ t


data Action = Action { basedir :: FilePath, cmds :: [String], deps :: [Target] }
data Component = Component Target Action
type Script    = [Component]


type ScriptT = StateT (Script,[Target])


infix 1 *>>

(*>>) :: Target -> Action -> Component
t *>> r = Component t r

into :: FilePath -> [String] -> Action
into dir cs = Action dir cs []

(.|.) :: Action -> [Target] -> Action
action .|. ts = action{ deps = ts }


buildTarget :: Target -> FilePath -> Int -> ScriptT IO ()
buildTarget tar base level = do
    (script, done) <- get
    let script' = filter (\(Component tar' _ ) -> tar' == tar) script
    if null script'
        then lift $ error ("Error: " ++ show tar ++ ": target not found!")
        else do
            let tot = length script'
            forM_ (zip [1..] script') $ \(n, Component target (Action path cmds' deps')) -> do
                (_, done') <- get
                unless (target `elem` done') $ do
                    put (script, target : done')
                    lift $ putStrLn $ replicate level '.' ++ bold ++ "[" ++ show n ++ "/" ++ show tot ++ "] " ++ show target ++ ":" ++ reset
                    -- satisfy dependencies
                    unless (null deps') $ do
                        lift $ putStrLn $ "Satisfying dependencies for " ++ show target ++ "..."
                        forM_ deps' $ \t -> when (t `notElem` done') $ buildTarget t base (level+1)
                    -- build target
                    lift $ setCurrentDirectory $ base </> path
                    ec <- lift $ mapM system cmds'
                    unless (all (== ExitSuccess) ec) $
                        lift $ error ("Error: " ++ show target ++ " aborted!")


simpleBuilder :: Script -> [String] -> IO ()
simpleBuilder script args = do
    base <- getCurrentDirectory
    E.catch (case args of
            ("configure":xs) -> evalStateT (buildTarget (Configure "*") base 0) (script,[]) >> putStrLn ( bold ++ "Done." ++ reset )
            ("build":xs)     -> evalStateT (buildTarget (Build "*")     base 0) (script,[]) >> putStrLn ( bold ++ "Done." ++ reset )
            ("install":xs)   -> evalStateT (buildTarget (Install "*")   base 0) (script,[]) >> putStrLn ( bold ++ "Done." ++ reset )
            ("clean":xs)     -> evalStateT (buildTarget (Clean "*")     base 0) (script,[]) >> putStrLn ( bold ++ "Done." ++ reset )
            _                -> usage)
          (\e -> setCurrentDirectory base >> print (e :: E.SomeException))


usage = putStrLn $ "usage: Setup COMMAND\n\n" ++
                   "Commands:\n" ++
                   "    configure   Prepare to build PFQ framework.\n" ++
                   "    build       Build PFQ framework.\n" ++
                   "    install     Copy the files into the install location.\n" ++
                   "    clean       Clean up after a build."


cabalConfigure = "runhaskell Setup configure --user"
cabalBuild     = "runhaskell Setup build"
cabalInstall   = "runhaskell Setup install"
cabalClean     = "runhaskell Setup clean"


