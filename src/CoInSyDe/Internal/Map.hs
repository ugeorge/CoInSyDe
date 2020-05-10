{-# LANGUAGE DeriveGeneric, TypeSynonymInstances, FlexibleInstances #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Internal.Map
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains a generic container for a "dictionary", which is to be used as
-- a database of components or types, along with their load history.
----------------------------------------------------------------------
module CoInSyDe.Internal.Map (
  -- * CoInSyDe 'Map' Type
  Id, Map(..), (!?), (!~),
  -- * History-Bookkeeping Map
  MapH, Info(..), Policy(..), prettyInfo,
  mkDb, (!*), (!^), mapDb, dbUpdate
  ) where

import           Data.Binary
import qualified Data.HashMap.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.Text as T (Text, pack, append)
import qualified Data.Text.Lazy as TL (unpack)
import           Data.YAML (ToYAML(..), mapping, (.=))
import           GHC.Generics
import           Text.Pretty.Simple (pShow)

import           Text.Pandoc.Builder
import           CoInSyDe.Internal.Docs

-------------------------------------------------------------

-- | Generic identifier used as library search key
type Id = Text 

-- | Dictionary type used throughout CoInSyDe
type Map t =  M.HashMap Id t

instance Binary v => Binary (Map v) where
  put = put . M.toList
  get = M.fromList <$> get

instance ToYAML v => ToYAML (Map v) where
  toYAML = mapping . map (\(k,v) -> k .= toYAML v) . M.toList

-- | Infix selector
(!?) :: Show t => Map t -> Id -> Maybe t
(!?) d k =  M.lookup k d

-- | Infix selector with detailed error message
(!~) :: Show t => Map t -> Id -> Either String t
(!~) d k =  maybe (Left msg) Right $ M.lookup k d
  where msg = "Key " ++ show k ++ " not found in dictionary with " ++ show (M.keys d)
-------------------------------------------------------------

-- | Stores information about the current entry (e.g. component).
data Info = Info { ldFile  :: FilePath
                 , ldLine  :: Int
                 , ldCol   :: Int
                 , comment :: Text
                 } deriving (Read,Show,Generic)

instance Binary Info

prettyInfo :: Info -> String
prettyInfo (Info f (-1) (-1) _) = f
prettyInfo (Info f l c _)  = f ++ " (" ++ show l ++ ":" ++ show c ++ ")"

-- | Dictionary associating an 'Id' with an entry (e.g. component), along with its
-- load history (newest to oldest).

type MapH t = Map (t,[Info])

-- | Dictionary entry insertion policy
data Policy = Keep | Replace deriving (Show, Eq)

infixl 9 !*, !^

-- | 'MapH' selector. Returs the entry with a given 'Id'.
(!*) :: (Show t) => MapH t -> Id -> Maybe t
(!*) d k = fst <$> d !? k

-- | 'MapH' selector. Returs the history for the entry with a given 'Id'.
(!^) :: (Show t) => MapH t -> Id -> Maybe [Info]
(!^) d k = snd <$> d !? k

-- | Makes a 'MapH' from a list of entries with their history.
mkDb :: [(Id, t, [Info])] -> MapH t
mkDb = M.fromList . map (\(i,c,h) -> (i,(c,h)))

mapDb f = fmap (\(e,i) -> (f e, i))
   
-- | Inserts an entry into a 'MapH'. Depending on the 'Policy', if the entry exists:
--
-- * 'Replace' : replaces it and updates the history as the \"newest\" entry.
-- * 'Keep' : ignores it and keeps the existing one, and updates the history as the
--            \"oldest\" entry.
dbUpdate :: Policy -> Id -> t -> Info -> MapH t -> MapH t
dbUpdate Replace name c info = M.insertWith fReplace name (c,[info])
dbUpdate Keep    name c info = M.insertWith fKeep name (c,[info])

fReplace (a,newh) (_,oldh) = (a,newh++oldh)
fKeep    (_,newh) (a,oldh) = (a,oldh++newh)

