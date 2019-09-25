module CoinSyDe.C.Template where

import CoinSyDe.C.Core
import Text.Parsec 
import Text.Parsec.Char
import Text.Parsec.Combinator 
import Control.Applicative hiding ((<|>),many)
import Data.Char (isSpace)

type Parser = Parsec String [TTm]

textToTemplate name = getTempl . runParser parseText [] name
  where getTempl (Right d) = d
        getTempl (Left er) = error $ "parser error " ++ show er

parseText :: Parser [TTm]
parseText = do
  spaces
  expr  <- parseTTm
  updateState (++[expr])
  state <- getState
  (eof >> return state) <|> parseText

parseTTm :: Parser TTm
parseTTm =  try varQuery <|> try funQuery <|> codeChunk
-- parseExpr = code

varQuery :: Parser TTm
varQuery = VarTTm <$> between (symbol "(|") (symbol "|)") query

query :: Parser [String]
query = identity `sepBy` symbol "."
  
funQuery :: Parser TTm
funQuery = FunTTm <$> between (symbol "[|") (symbol "|]") identity

codeChunk :: Parser TTm
codeChunk = fmap CodeTTm $ anyChar `manyTill` endCode
  where
    endCode = careful "(|" <|> careful "[|" <|> (eof >> string "")

-------------------------------------------------- 
-- isSpace' '\n' = False
-- isSpace' c    = isSpace c
whiteSpace  = skipMany (skipMany1 (satisfy isSpace) <?> "")

lexeme p = do{ x <- p; whiteSpace; return x  }
symbol n = lexeme (string n)
identity = lexeme $ many1 $ alphaNum <|> oneOf "_'"
codeStr  = lexeme $ many1 $ satisfy (not . isSpace)
careful  = try . lookAhead . string

----------------------------------

