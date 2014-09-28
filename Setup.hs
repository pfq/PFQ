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

import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Exit

import Control.Monad
import qualified Control.Exception as E

components =
    [
        ("pfq.ko", "kernel/",
            [ ],
            [ "make"],
            [ "make install" ],
            [ "make clean"   ]),

        ("C lib", "user/C/",
            [ "cmake ."],
            [ "make"],
            [ "make install" ],
            [ "make clean"   ]),

        ("C++ headers", "user/C++/",
            [ ],
            [ ],
            [ "make install" ],
            [ ]),

        ("Haskell lib", "user/Haskell/",
            [ cabalConfigure ],
            [ cabalBuild     ],
            [ cabalInstall   ],
            [ cabalClean     ]),

        ("pfq-counters", "user/Haskell/pfq-counters/",
            [ cabalConfigure ],
            [ cabalBuild     ],
            [ cabalInstall   ],
            [ cabalClean     ]),

        ("Haskell test", "user/Haskell/test/",
            [ ],
            [ "make" ],
            [ ],
            [ "make clean" ]),

        ("irq-affinity", "script/irq-affinity/",
            [ cabalConfigure ],
            [ cabalBuild     ],
            [ cabalInstall   ],
            [ cabalClean     ]),

        ("pfq-omatic", "script/pfq-omatic/",
            [ cabalConfigure ],
            [ cabalBuild     ],
            [ cabalInstall   ],
            [ cabalClean     ]),

        ("C/C++ tests", "user/test/",
            [ "cmake ."],
            [ "make"],
            [ ],
            [ "make clean"   ]),

        ("C/C++ tools", "user/tool/",
            [ "cmake ."],
            [ "make"],
            [ ],
            [ "make clean"   ])
    ]


type Component = (String, FilePath, [String], [String], [String], [String])

data Action = Configure | Build | Install | Clean

instance Show Action where
    show Configure = "Configuring"
    show Build     = "Building"
    show Install   = "Installing"
    show Clean     = "Cleaning"


main = do
    args <- getArgs
    base <- getCurrentDirectory
    E.catch (case args of
            ("configure":_) -> void $ mapM (runAction Configure base) components >> putStrLn "Done."
            ("build":_)     -> void $ mapM (runAction Build     base) components >> putStrLn "Done."
            ("install":_)   -> void $ mapM (runAction Install   base) components >> putStrLn "Done."
            ("clean":_)     -> void $ mapM (runAction Clean     base) components >> putStrLn "Done."
            _               -> usage)
          (\e -> setCurrentDirectory base >> print (e :: E.SomeException))


runAction :: Action -> FilePath -> Component -> IO ()
runAction action base (name, path, cs, bs, is, cns) = do
    let cmds = case action of
                Configure -> cs
                Build     -> bs
                Install   -> is
                Clean     -> cns
    unless (null cmds) $ do
        putStrLn $ show action ++ " " ++ name ++ ":"
        setCurrentDirectory $ base </> path
        ec <- mapM system cmds
        unless (all (== ExitSuccess) ec) $ error ("Error: " ++ show action ++ " " ++ name ++ " aborted!")


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

