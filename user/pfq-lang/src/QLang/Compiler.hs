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

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module QLang.Compiler
(
  compile
) where

import Language.Haskell.Interpreter
import Network.PFQ.Lang as Q

import Data.Maybe
import System.IO
import System.Directory
import System.FilePath.Posix (dropFileName, (</>), (<.>))
import Control.Exception
import Control.Monad.Reader
import Options

import QLang.Parser

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif


printIndent :: (Show a) => Int -> a -> IO ()
printIndent n x = putStrLn $ replicate n ' ' ++ show x


modToFilePath :: FilePath -> String -> String
modToFilePath base n = base </> fixPath n <.> "hs"
    where fixPath = map (\c -> if c == '.' then '/' else c)


compile :: String -> OptionT IO (Q.Function (SkBuff -> Action SkBuff))
compile raw = do
    let (code, localImports) = parseCode raw

    opt <- ask
    imports <- mkImportList localImports

    whenLevel 2 $ lift (hPutStrLn stderr "Code:" >> putStrLn code)
    whenLevel 1 $ lift (hPutStrLn stderr "Imports: " >> mapM_ (printIndent 4) imports)

    res <- lift $ runInterpreter $ do

        -- get the base path of the pfq-lang main file:

        basePath <- dropFileName <$> maybe (return ".") (lift . canonicalizePath) (file opt)
        when (verb opt >= 1) $ lift $ putStrLn ("SeachPath: " ++ basePath)

        -- get the list the module to load (from the import list):

        flags <- lift $ mapM (doesFileExist . modToFilePath basePath . fst) imports
        let mods = mapMaybe (\(i,b) -> if b then Just i else Nothing) $ zip (map fst imports) flags

        -- dump the list of local modules:
        -- when (verb opt >= 1) $ lift $ putStrLn ("Local modules: " ++ show mods)

        -- specify options:

        set [ languageExtensions := [ OverloadedStrings, RebindableSyntax ]
            , searchPath := [basePath] ]

        -- load local modules:
        loadModules mods

        -- import installed modules:
        setImportsQ imports

        -- print the list of modules loaded:
        when (verb opt >= 1) $ getLoadedModules >>= \xs -> unless (null xs) $ lift $ hPutStrLn stderr "\nModules:" >> mapM_ (printIndent 4) xs

        -- interpret the pfq-lang computation:

        interpret (mkMainFunction code) (as :: (Function (SkBuff -> Action SkBuff)))

    either throw return res

