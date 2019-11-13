{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad
import Data.Aeson
import Data.Aeson.Types 
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Vector as V (toList)
import Data.HashMap.Strict as H (toList)

-- | Hacky way to mimic XML nodes. JSON 'Value' does not have info about the node
-- name, but we store it in a record.
data JSON = Node   { jsonName :: !Text, jsonContent :: [JSON]}
          | Attrib { jsonName :: !Text, jsonValue :: !Text}
          deriving Show


instance FromJSON JSON where
  parseJSON v@(Object _) = go "root" v
    where
      go :: Text -> Value -> Parser JSON
      go n (Object q) = Node n <$> mapM (\(k,a) -> go k a) cList
        where cList = filter (not . isArray) (H.toList q) ++
                      concatMap fromArray (filter isArray $ H.toList q)
              isArray (_, Array _) = True
              isArray _ = False
              fromArray (k, Array a) = map ((,) k) $ V.toList a
              fromArray _ = error "Critical: I thought I filtered you!"
      go n  (String s)  = return $ Attrib n s
      go _ x = error $ "JSON Critical: cannot parse " ++ show x
  parseJSON _ = mzero


-- | JSON parser API
instance FNode JSON where
  getInfo _      = "JSON"  -- Aeson does not have error reporting!
  children str n = case n of
                     Node _ c   -> filter (\n -> jsonName n == pack str) c
                     Attrib n _ -> error $ show n ++ " is an attribute, not a node!"
  getTxt n       = case children "code" n of
                     [Attrib _ a]  -> a
                     _             -> pack "" 
  getName        = unpack . jsonName
  readDoc        = fromMaybe (throw EmptyFile) . decode
  getAttr attr n = case children attr n of
                     [Attrib _ a]  -> Right a
                     _ -> Left $ "cannot find attribute " ++ show attr ++
                          " in node of type " ++ show (jsonName n)

-- -- OLD STUFF
-- instance FromJSON JSON1 where
--   parseJSON v@(Object _) = go "root" v
--     where
--       go :: Text -> Value -> Parser JSON1
--       go n o@(Object q) = withObject "Children" (\obj ->
--          fmap (Node n) . forM (H.toList obj) $ \(n,v) -> go n v) o
--       go n  (String s)  = return $ Attrib n s
--       go n a@(Array _)  = withArray "Children" (\obj ->
--         fmap (Node  n) . forM (V.toList obj) $ \v -> go n v) a
--       go _ x = error $ "JSON Critical: cannot parse " ++ show x
--   parseJSON _ = mzero

