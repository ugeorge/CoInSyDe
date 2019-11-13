{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Core.TTm
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains the CoInSyDe template language and the
-- template language parser.
----------------------------------------------------------------------
module CoInSyDe.Core.TTm (
  Name, Keyword, TTm(..), textToTm
  ) where

import Control.DeepSeq
import Data.Char (isSpace)
import Data.Text (Text,split,pack,unpack,strip)
import Text.Parsec

-- | Abstract terms to represent templates. The CoInSyDe template language consists in
-- a list (i.e. a sequence) of 'TTm' terms.
--
-- The list of 'Keyword's following 'TPort' and 'TFun' are queries telling CoInSyDe to
-- expand specific info. If the list is empty than the default expansion occurs.
data TTm = TCode Text            -- ^ target language code in text format
         | TPort Name [Keyword]  -- ^ placeholder for port identifier. Default expands
                                 --   to port name.
         | TFun  Name [Keyword]  -- ^ placeholder for functional template
                                 --   identifier. Default expands to functional code.
         deriving (Show,Read)

instance NFData TTm where
  rnf _ = ()

type Parser = Parsec Text [TTm]
type Name    = Text   -- ^ Generic name found in templates.
type Keyword = String -- ^ A "reserved" keyword used in template identifiers.

-- | Parses text to a list of CoInSyDe template terms.
textToTm name = getTempl . runParser parseText [] name
  where
    getTempl (Right d) = d
    getTempl (Left er) = error $ "Template parser error:\n" ++ show er

parseText :: Parser [TTm]
parseText = do
  spaces
  expr  <- parseExpr
  updateState (++expr)
  state <- getState
  (eof >> return state) <|> parseText

parseExpr :: Parser [TTm]
parseExpr = portTm <|> try funTm <|> try codeTm

codeTm :: Parser [TTm]
codeTm = let eoc = careful "(|" <|> careful "[|" <|> (eof >> string "")
         in  ((:[]) . TCode . strip . pack) <$> anyChar `manyTill` eoc

portTm :: Parser [TTm]
portTm = (:[]) . mkQuery TPort <$> between (symbol "(|") (symbol "|)") identity

funTm :: Parser [TTm]
funTm = (:[TCode "\n"]) . mkQuery TFun <$> between (symbol "[|") (symbol "|]") identity

mkQuery c tx = let l = split (=='.') (pack tx)
               in  c (head l) (map unpack $ tail l)

--------------------------------------------------
-- isSpace' '\n' = False
-- isSpace' c    = isSpace c
whiteSpace = skipMany (skipMany1 (satisfy isSpace) <?> "")

lexeme p = do{ x <- p; whiteSpace; return x  }
symbol   = lexeme . string
careful  = try . lookAhead . string
identity = lexeme $ many1 $ alphaNum <|> oneOf "_'."
