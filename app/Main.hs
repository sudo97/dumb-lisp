{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State (StateT (runStateT))
import Control.Monad.Writer (Writer, runWriter)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import JsAST
import LispAST (translate)
import Parser
import Text.Megaparsec (MonadParsec (eof), parseTest, runParser)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
  text <- TIO.readFile "./test.scm"
  case runParser lispParser "./test.scm" text of
    Left err -> putStrLn $ errorBundlePretty err
    Right expr -> do
      --   print expr
      case translate expr of
        Just js -> printJS js
        Nothing -> putStrLn "Error"
