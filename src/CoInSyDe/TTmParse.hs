----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.TTmParse
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains the CoInSyDe template language parser.
----------------------------------------------------------------------
module CoInSyDe.TTmParse (
  textToTm
  ) where

import Control.Applicative hiding ((<|>),many)
import Data.Char (isSpace)
import Data.Text (Text,split,pack,strip)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import CoInSyDe.Core (TTm(..))

type Parser = Parsec Text [TTm]

-- | Parses text to a list of CoInSyDe template terms.
textToTm name = getTempl . runParser parseText [] name
  where
    getTempl (Right d) = d
    getTempl (Left er) = error $ "Template parser error:\n" ++ show er

parseText :: Parser [TTm]
parseText = do
  spaces
  expr  <- parseExpr
  updateState (++[expr])
  state <- getState
  (eof >> return state) <|> parseText

parseExpr :: Parser TTm
parseExpr = portTm <|> funTm <|> try codeTm

codeTm :: Parser TTm
codeTm = let eoc = careful "(|" <|> careful "[|" <|> (eof >> string "")
         in  (TCode . strip . pack) <$> anyChar `manyTill` eoc

portTm :: Parser TTm
portTm = mkQuery TPort <$> between (symbol "(|") (symbol "|)") identity

funTm :: Parser TTm
funTm = mkQuery TFun <$> between (symbol "[|") (symbol "|]") identity

mkQuery c tx = let l = split (=='.') (pack tx)
               in  c (head l) (tail l)

--------------------------------------------------

whiteSpace = skipMany (skipMany1 (satisfy isSpace) <?> "")

lexeme p = do{ x <- p; whiteSpace; return x  }
symbol   = lexeme . string
careful  = try . lookAhead . string
identity = lexeme $ many1 $ alphaNum <|> oneOf "_'."
