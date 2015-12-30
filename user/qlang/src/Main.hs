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
          _ | json opts -> runReaderT (QLang.JSON.compile code) opts
            | fdescr opts -> runReaderT (QLang.FDescr.compile code) opts
            | otherwise -> error "qlang: operation not supported"

  case result of
    Left (WontCompile es)  ->  error $ unlines (map errMsg es)
    Left (NotAllowed xs)   ->  error xs
    Left (GhcException xs) ->  error xs
    Left (UnknownError xs) ->  error xs
    Right out -> hPutStr outHandle out

  hClose outHandle
  hClose inHandle
