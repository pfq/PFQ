-- Copyright (c) 2015 Nicola Bonelli <nicola@pfq.io>
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

module QLang.Parser
  ( defaultImports
  , parseCode
  , mkMainFunction
  , mkImportList
  ) where

import Language.Haskell.Interpreter
-- import Data.List

import Control.Monad.Reader
import Options
import Data.Maybe


defaultImports :: [(ModuleName, Maybe String)]
defaultImports =
    [ ("Prelude", Just "P")
    , ("Network.PFQ.Lang", Nothing)
    , ("Network.PFQ.Types", Nothing)
    , ("Network.PFQ.Lang.Prelude", Nothing)
    , ("Network.PFQ.Lang.Default", Nothing)
    , ("Network.PFQ.Lang.Experimental", Nothing)
    ]


parseCode :: String -> (String, [(ModuleName, Maybe String)])
parseCode code = (unlines (map snd xs), mapMaybe fst xs)
  where xs = map parseLine (lines code)


type Import = (ModuleName, Maybe String)


parseLine :: String -> (Maybe Import, String)
parseLine xs = case () of
  _  | "import": "qualified": modname: "as": alias : ys <- ws  -> (Just (modname, Just alias), unwords ys)
     | "import": "qualified": modname : ys <- ws               -> (Just (modname, Just modname), unwords ys)
     | "import": modname : ys <- ws                            -> (Just (modname, Nothing), unwords ys)
     |  otherwise                                              -> (Nothing, xs)
  where ws = words xs


mkMainFunction :: String -> String
mkMainFunction code = "(let " ++ code ++ " in main)"


mkImportList :: (Monad m) => [(ModuleName, Maybe String)] -> OptionT m [(ModuleName, Maybe String)]
mkImportList xs = do
  opt <- ask
  return $ defaultImports ++ map (, Nothing) (modules opt) ++ xs

