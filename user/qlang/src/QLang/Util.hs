{-# LANGUAGE TupleSections #-}

module QLang.Util
  ( defaultImports
  , parseImports
  , parseCode
  , mkMainFunction
  , mkImportList
  ) where

import Language.Haskell.Interpreter
import Data.List

import Control.Monad.Trans.Reader
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
                 ,("Network.PFq.Default", Nothing)
                 ,("Network.PFq.Experimental", Nothing)
                 ,("Network.PFq.Lang", Nothing)
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
