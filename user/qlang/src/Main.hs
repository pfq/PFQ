module Main where

import Control.Monad.Trans.Reader
import System.Console.CmdArgs
import CmdLine
import Options
import System.IO

import Data.Maybe

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

  output <- case () of
            _ | json opts -> runReaderT (QLang.JSON.compile code) opts
            _ | fdescr opts -> runReaderT (QLang.FDescr.compile code) opts
              | otherwise -> error "qlang: operation not supported"

  hPutStr outHandle output

  hClose outHandle
  hClose inHandle
