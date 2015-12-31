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

module QLang.FDescr where

import Language.Haskell.Interpreter
import Network.PFq.Lang as Q

import Control.Exception (throw)

import QLang.Util
import Control.Monad.Trans.Reader
import Control.Monad(when)
import Options


compile :: String -> OptionT IO String
compile raw = do
    let (code, localImports) = parseCode raw
    opt <- ask
    imports <- mkImportList localImports
    lift $ do
      when (verb opt > 0) (putStrLn ("imports: " ++ show imports))
      res <- runInterpreter $ do
          setImportsQ imports
          set [languageExtensions := [ OverloadedStrings ]]
          interpret (mkMainFunction code) (as :: (Function (SkBuff -> Action SkBuff)))
      either throw (\comp -> return (show (fst $ Q.serialize comp 0)))  res


