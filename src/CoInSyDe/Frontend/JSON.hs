{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Frontend.JSON
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Instance of the JSON frontend parser.
----------------------------------------------------------------------
module CoInSyDe.Frontend.JSON where

import CoInSyDe.Frontend

import Control.Exception
import Data.Yaml
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (strip,pack)
import Data.Text.Lazy as TL (unpack)
import Data.Vector as V (toList)
import Data.ByteString.Lazy (toStrict)
import Data.HashMap.Strict as H (lookup)
import Text.Pretty.Simple (pShow)

type JSON = Object

-- | JSON parser API
instance FNode JSON where
  getInfo _      = "JSON"  -- Aeson does not have error reporting!
  -- readDoc        = fromMaybe (throw EmptyFile) . decode
  readDoc doc  =
    case decodeEither' (toStrict doc) of
      Left err -> throw $ ParseException "" $
                  prettyPrintParseException err
      Right a  -> a
  children str n =
    case H.lookup (pack str) n of
      Just (Object o) -> [o]
      Just (Array a)  -> getObjects $ V.toList a
      Just _          -> error $ show str ++ " is an attribute, not a node!"
      Nothing         -> []
  getTxt n =
    case H.lookup "code" n of
      Just (String a) -> strip a -- TODO: remove heading and trailing whitespaces
      _               -> pack ""
  getStrAttr str n =
    case H.lookup (pack str) n of
      Just (String a) -> Right a
      _               -> Left $ "cannot find attribute " ++ show str ++
                         " in node\n " ++ (TL.unpack $ pShow n)
  getBoolAttr str n =
    case H.lookup (pack str) n of
      Just (Bool a) -> Right a
      _             -> Left $ "cannot find boolean " ++ show str 

getObjects = map (\(Object c) -> c) . filter isObject
  where
    isObject (Object _) = True
    isObject _ = False
