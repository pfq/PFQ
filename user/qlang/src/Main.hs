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

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Language.Haskell.Interpreter
import Control.Monad.Trans.Reader
import System.Console.CmdArgs
import CmdLine
import Options
import System.IO

import Data.Maybe
import Control.Exception

import qualified QLang.JSON
import qualified QLang.FDescr

notNull = not . null


main = do
  opts <- cmdArgsRun options

  inHandle <- if notNull (files opts)
                then openFile (head (files opts)) ReadMode
                else return stdin

  outHandle <- if isJust (output opts)
                then openFile (fromJust $ output opts) WriteMode
                else return stdout

  code <- hGetContents inHandle

  result <- try $ case () of
          _ | json opts   -> runReaderT (QLang.JSON.compile code) opts
            | fdescr opts -> runReaderT (QLang.FDescr.compile code) opts
            | otherwise -> return ""

  case result of
    Left (WontCompile es)  ->  error $ unlines (map errMsg es)
    Left (NotAllowed xs)   ->  error xs
    Left (GhcException xs) ->  error xs
    Left (UnknownError xs) ->  error xs
    Right out -> hPutStr outHandle out

  hPutStr outHandle "\n"
  hClose outHandle
  hClose inHandle
