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

import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Exit

import Control.Monad
import qualified Control.Exception as E

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


infix 1 *>>

(*>>) :: Target -> Action -> Component
t *>> r = Component t r

into :: FilePath -> [String] -> Action
into dir cs = Action dir cs []

(.|.) :: Action -> [Target] -> Action
action .|. ts = action{ deps = ts }


buildTarget :: Target -> FilePath -> Script -> IO ()
buildTarget tar base script = do
    let script' = filter (\ (Component tar' _) -> tar' == tar) script
    if (null script')
    then error ("Error: " ++ show tar ++ ": target not found!")
    else do
        let tot = length script'
        forM_ (zip [1..] script') $ \(n, (Component target (Action path cmds' deps'))) -> do
            putStrLn $ "[" ++ show n ++ "/" ++ show tot ++ "] " ++ show target ++ ":"
            unless (null deps') $ do
                putStrLn $ "Satisfying dependencies for " ++ show target ++ ":"
                forM_ deps' $ \t -> buildTarget t base script
            setCurrentDirectory $ base </> path
            ec <- mapM system cmds'
            unless (all (== ExitSuccess) ec) $ error ("Error: " ++ show target ++ " aborted!")


simpleBuilder :: Script -> [String] -> IO ()
simpleBuilder script args = do
    base <- getCurrentDirectory
    E.catch (case args of
            ("configure":xs) -> buildTarget (Configure "*") base script >> putStrLn "Done."
            ("build":xs)     -> buildTarget (Build "*")     base script >> putStrLn "Done."
            ("install":xs)   -> buildTarget (Install "*")   base script >> putStrLn "Done."
            ("clean":xs)     -> buildTarget (Clean "*")     base script >> putStrLn "Done."
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


