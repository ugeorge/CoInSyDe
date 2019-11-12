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
import Data.Text hiding (filter,map)
import Data.Typeable
import Data.Aeson
import Data.Aeson.Types 
import Control.Exception
import Control.Monad
import qualified Data.Vector as V
import Data.Maybe (fromMaybe,fromJust)
import Data.Map.Lazy as M hiding (map,foldr,filter)
import qualified Data.HashMap.Strict as H

data JSON1 = Node !Text [JSON1]
           | Attrib !Text !Text deriving Show

instance FromJSON JSON1 where
  parseJSON v@(Object _) = go "root" v
    where
      go :: Text -> Value -> Parser JSON1
      go n o@(Object _) = withObject "Children" (\obj ->
        fmap (Node n) . forM (H.toList obj) $ \(n,v) -> go n v) o
      -- go n (Object o)   = Parent <$> H.mapWithKey (\k v -> go k v) o
      go n  (String s)  = return $ Attrib n s
      -- go n (Array a)  = go n $ Object $ H.fromList $ map ((,) n) $ V.toList a
      go n a@(Array _)  = withArray "Children" (\obj ->
        fmap (Node  n) . forM (V.toList obj) $ \v -> go n v) a
      go _ x = error $ "JSON Critical: cannot parse " ++ show x
  parseJSON _ = mzero







-- | Hacky way to mimic XML nodes. JSON 'Value' not have info about the node name, but
-- we store it in a record.
data JSON = LXML { nName   :: !Text
                 , content :: Either Text [JSON]
                 } deriving (Show)
                                                
instance FromJSON JSON where
  parseJSON v@(Object _) = go "root" v
    where
      go :: Text -> Value -> Parser JSON
      go n o@(Object _) = withObject "Attributes" (\obj ->
        fmap (LXML n . Right) . forM (H.toList obj) $ \(n, v) -> go n v) o
      go n (String s) = return $ LXML n (Left s)
      go n a@(Array _)  = withArray "Children" (\obj ->
        fmap (LXML n . Right) . forM (V.toList obj) $ \v -> go n v) a
      go _ x = error $ "JSON Critical: cannot parse " ++ show x
  parseJSON _ = mzero

-- | 
instance FNode JSON where
  getInfo _      = "JSON"  -- Aeson does not have error reporting!
  getTxt n       = case children "code" n of
                     [LXML _ (Left a)]  -> a
                     _ -> pack "" 
  getName        = unpack . nName
  children str   = filter (\n -> nName n == (pack str)) . getCh . content
    where
      getCh (Left _) = error $ show str ++ " is an attribute name, not a child node!"
      getCh (Right ns) = ns
  readDoc        = fromMaybe (throw EmptyFile) . decode
  getAttr attr n = case children attr n of
                     [LXML _ (Left a)]  -> Right a
                     _ -> Left $ "cannot find attribute " ++ show attr ++
                          " in node of type " ++ show (nName n)
