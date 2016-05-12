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

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Language.Haskell.Interpreter
import Control.Monad.Reader
import System.Console.CmdArgs
import CmdLine
import Options
import System.IO

import Data.Maybe
import Control.Exception

import qualified QLang.JSON
import qualified QLang.FDescr
import qualified QLang.Compiler
import qualified QLang.PFQ


main :: IO ()
main = do

  opt' <- cmdArgsRun options

  bracket (maybe (return stdin)  (`openFile` ReadMode)  (file opt')) hClose $ \inHandle ->
    bracket (maybe (return stdout) (`openFile` WriteMode) (output opt')) hClose $ \outHandle -> do

      code <- hGetContents inHandle

      result <- try $ runReaderT (QLang.Compiler.compile code) opt'

      case result of
        Left (WontCompile es)  ->  error $ unlines (map errMsg es)
        Left (NotAllowed xs)   ->  error xs
        Left (GhcException xs) ->  error xs
        Left (UnknownError xs) ->  error xs
        Right comp -> do
            (case () of
                _ | json opt'   -> runReaderT (QLang.JSON.compile comp) opt'
                  | fdescr opt' -> runReaderT (QLang.FDescr.compile comp) opt'
                  | otherwise   -> return "") >>= hPutStr outHandle
            when (isJust (gid opt')) $ runReaderT (QLang.PFQ.load comp) opt' >>= hPutStr stderr

      hPutStr outHandle "\n"

