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
  , parseImports
  , parseCode
  , mkMainFunction
  , mkImportList
  ) where

import Language.Haskell.Interpreter
import Data.List

import Control.Monad.Reader
import Options


dropWhite :: String -> String
dropWhite = dropWhile (`elem` " \\\a\b\t\n\v\f\r")


isImport :: String -> Bool
isImport = ("import " `isPrefixOf`) . dropWhite


parseCode :: String -> (String, [(ModuleName, Maybe String)])
parseCode code =  (unlines (snd ps), map parseImports (fst ps))
  where ps = partition isImport (lines code)


defaultImports :: [(ModuleName, Maybe String)]
defaultImports = [("Prelude", Nothing)
                 ,("Network.PFQ.Lang", Nothing)
                 ,("Network.PFQ.Types", Nothing)
                 ,("Network.PFQ.Lang.Default", Nothing)
                 ,("Network.PFQ.Lang.Experimental", Nothing)
                 ]


parseImports :: String -> (ModuleName, Maybe String)
parseImports xs = case () of
  _  | l >= 3 && ws !! 1 == "qualified"
        -> if l >= 4 && (ws !! 3 ==  "as")
            then (ws !! 2, Just (ws !! 4))
            else (ws !! 2, Just (ws !! 2))
     | l >= 2    -> (ws !! 1, Nothing)
     | otherwise -> error "qlang: parse import error!"
  where ws = words xs
        l  = length ws


mkMainFunction :: String -> String
mkMainFunction code = "(let " ++ code ++ " in main)"

mkImportList :: (Monad m) => [(ModuleName, Maybe String)] -> OptionT m [(ModuleName, Maybe String)]
mkImportList xs = do
  opt <- ask
  return $ defaultImports ++  map (, Nothing) (modules opt) ++ xs

