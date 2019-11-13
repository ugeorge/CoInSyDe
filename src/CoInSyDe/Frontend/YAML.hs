{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Frontend.YAML
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Instance of the YAML frontend parser.
----------------------------------------------------------------------
module CoInSyDe.Frontend.YAML where

import CoInSyDe.Frontend

import Control.Exception
import Control.Monad
import Data.Yaml
import Data.Aeson.Types 
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text, pack, unpack)
import Data.Vector as V (toList)
import Data.HashMap.Strict as H (toList)

-- | Hacky way to mimic XML nodes. YAML 'Value' does not have info about the node
-- name, but we store it in a record.
data YAML = Node   { yamlName :: !Text, yamlContent :: [YAML]}
          | Attrib { yamlName :: !Text, yamlValue :: !Text}
          deriving Show


instance FromJSON YAML where
  parseJSON v@(Object _) = go "root" v
    where
      go :: Text -> Value -> Parser YAML
      go n (Object q) = Node n <$> mapM (\(k,a) -> go k a) cList
        where cList = filter (not . isArray) (H.toList q) ++
                      concatMap fromArray (filter isArray $ H.toList q)
              isArray (_, Array _) = True
              isArray _ = False
              fromArray (k, Array a) = map ((,) k) $ V.toList a
              fromArray _ = error "Critical: I thought I filtered you!"
      go n  (String s)  = return $ Attrib n s
      go _ x = error $ "YAML Critical: cannot parse " ++ show x
  parseJSON _ = mzero


-- | YAML parser API
instance FNode YAML where
  getInfo _      = "YAML"  -- TODO: use Yaml.ParseException
  children str n = case n of
                     Node _ c   -> filter (\n -> yamlName n == pack str) c
                     Attrib n _ -> error $ show n ++ " is an attribute, not a node!"
  getTxt n       = case children "code" n of
                     [Attrib _ a]  -> a
                     _             -> pack "" 
  getName        = unpack . yamlName
  readDoc doc    = case decodeEither' (toStrict doc) of
                     Left err -> throw $ ParseException "" $
                                 prettyPrintParseException err
                     Right a  -> a
  getAttr attr n = case children attr n of
                     [Attrib _ a]  -> Right a
                     _ -> Left $ "cannot find attribute " ++ show attr ++
                          " in node of type " ++ show (yamlName n)

