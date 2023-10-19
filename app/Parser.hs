{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad (void)
import Data.Char (isAlphaNum)
import Data.Functor (($>))
import qualified Data.Text as T
import Data.Void (Void)
import qualified LispAST as Lisp
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void T.Text

lispParser :: Parser Lisp.LispVal
lispParser =
  choice
    [ stringP,
      boolP,
      listP,
      try numP,
      try identifierP,
      quoteP
    ]

listP :: Parser Lisp.LispVal
listP =
  label "list" . between (char '(' *> space) (char ')') $
    Lisp.List <$> (lispParser `sepEndBy` space1)

stringP :: Parser Lisp.LispVal
stringP = Lisp.String . T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

boolP :: Parser Lisp.LispVal
boolP = Lisp.Bool <$> (string "#t" $> True <|> string "#f" $> False)

numP :: Parser Lisp.LispVal
numP = Lisp.Number <$> L.signed empty (try L.float <|> L.decimal)

identifierP :: Parser Lisp.LispVal
identifierP =
  label "identifier" $
    Lisp.Atom . T.pack <$> do
      first <- letterChar <|> oneOf ("+-!$%&*/:<=>?@^_~" :: String)
      rest <- many (alphaNumChar <|> oneOf ("+-!$%&*/:<=>?@^_~" :: String))
      pure (first : rest)

quoteP :: Parser Lisp.LispVal
quoteP = Lisp.List . (Lisp.Atom "quote" :) . pure <$> (char '\'' *> lispParser)