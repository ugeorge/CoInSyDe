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

import           CoInSyDe.Frontend

import           Control.Exception (throw)
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.HashMap.Strict as H (lookup)
import           Data.Text as T (pack,unpack,strip,lines,unlines,null)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Lazy as TL (unpack)
import           Data.Vector as V (toList)
import           Data.Yaml
import           Data.Yaml.Pretty


type JSON = Object

-- | JSON parser API
instance FNode JSON where
  getInfo _      = ""  -- Aeson does not have error reporting!
  children str n =
    case H.lookup (pack str) n of
      Just (Object o) -> [o]
      Just (Array a)  -> getObjects $ V.toList a
      Just _          -> error $ show str ++ " is an attribute, not a node!"
      Nothing         -> []
  getTxt n =
    case H.lookup "code" n of
      Just (String a) ->  T.unlines . filter (not . T.null) . map T.strip . T.lines $ a
      _               -> ""
  getStrAttr str n =
    case H.lookup (pack str) n of
      Just (String a) -> Right a
      _               -> Left $ "cannot find attribute " ++ show str ++
                         " in node\n " ++ T.unpack (decodeUtf8 $ encodePretty defConfig n)
  getJsonAttr str n =
    case H.lookup (pack str) n of
      Just a -> Right a
      _      -> Left $ "cannot find boolean " ++ show str 

getObjects = map (\(Object c) -> c) . filter isObject
  where
    isObject (Object _) = True
    isObject _ = False

readJSON :: FilePath -> IO Object
readJSON path = BL.readFile path >>=
                either (throw . ParseException "") return . eitherDecode'

readYAML :: FilePath -> IO Object
readYAML path = BS.readFile path >>=
                either (throw . ParseException "" . prettyPrintParseException) return
                . decodeEither'
