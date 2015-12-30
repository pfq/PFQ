{-# LANGUAGE TupleSections #-}

module QLang.JSON where

import Language.Haskell.Interpreter
import Network.PFq.Lang

import qualified Data.Aeson as A
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as C

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
      either throw (\comp -> return (C.unpack $ A.encode comp))  res


