{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
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

-- import CoInSyDe.Frontend

-- import Data.Yaml
-- import Data.Text (pack)
-- import Data.Text.Lazy as TL (unpack)
-- import Data.Vector as V (toList)
-- import Data.HashMap.Strict as H (lookup)
-- import Data.ByteString.Lazy (toStrict)
-- import Text.Pretty.Simple (pShow)
-- import Control.Exception (throw)

-- type YAML = Object

-- -- | YAML parser API
-- instance FNode YAML where
--   getInfo _ = "YAML"  -- TODO: use Yaml.ParseException
--   readDoc doc  =
--     case decodeEither' (toStrict doc) of
--       Left err -> throw $ ParseException "" $
--                   prettyPrintParseException err
--       Right a  -> a
--   children str n =
--     case H.lookup (pack str) n of
--       Just (Object o) -> [o]
--       Just (Array a)  -> getObjects $ V.toList a
--       Just _          -> error $ show str ++ " is an attribute, not a node!"
--       Nothing         -> []
--   getTxt n =
--     case H.lookup "code" n of
--       Just (String a) -> a
--       _               -> pack ""
--   getStrAttr str n =
--     case H.lookup (pack str) n of
--       Just (String a) -> Right a
--       _               -> Left $ "cannot find attribute " ++ show str ++
--                          " in node\n " ++ (TL.unpack $ pShow n)
--   getBoolAttr str n =
--     case H.lookup (pack str) n of
--       Just (Bool a) -> Right a
--       _             -> Left $ "cannot find boolean " ++ show str 

-- getObjects = map (\(Object c) -> c) . filter isObject
--   where
--     isObject (Object _) = True
--     isObject _ = False
