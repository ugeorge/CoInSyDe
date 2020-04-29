{-# LANGUAGE DeriveGeneric #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Core.Dict
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
  Id, Map(..), mkMap, mkMapWith,
  isEmpty, ids, entries, idEntries,
  insertWith, union, (!?), (!?!),
  -- * Re-wrapped utilities
  emptyMap, liftMap, liftMap2,
  -- * History-Bookkeeping Map
  MapH, Info(..), Policy(..), prettyInfo,
  mkDict, (!*), (!^), mapDict, 
  dictUpdate, dictUnion
  ) where

import           Data.Maybe (fromMaybe)
import           Data.Binary
import           Data.Text as T (Text, pack, append)
import qualified Data.HashMap.Strict as M
import           GHC.Generics
import           Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as TL (unpack)

import Text.Pandoc.Builder
import CoInSyDe.Internal.Docs

-------------------------------------------------------------

-- | Generic identifier used as library search key
type Id = Text 

-- | Dictionary type used throughout CoInSyDe
newtype Map t = Map { getMap :: M.HashMap Id t } deriving (Show, Read, Generic)

instance Functor Map where
  fmap f  = Map . fmap f . getMap

instance Foldable Map where
  foldr f i = M.foldr f i . getMap

instance Traversable Map where
  traverse f x = Map <$> traverse f (getMap x)

instance Binary v => Binary (Map v) where
  put = put . M.toList . getMap
  get = (Map . M.fromList) <$> get

mkMap :: [(Id,t)] -> Map t
mkMap = Map . M.fromList

mkMapWith f = Map . M.fromListWith f

emptyMap  = Map M.empty
liftMap f = Map . f . getMap
liftMap2 f (Map a) (Map b) = Map (f a b)

isEmpty = M.null . getMap
ids = M.keys . getMap
entries = M.elems . getMap
idEntries = M.toList . getMap

insertWith f k v = liftMap (M.insertWith f k v)
union = liftMap2 M.union

-- | Genric 'Map' selector. Throws a more meaningful error message than the default
-- one.
(!?!) :: Show t => Map t -> Id -> t
(!?!) d k = fromMaybe (error $ dictErr k d) (M.lookup k $ getMap d)

(!?) :: Show t => Map t -> Id -> Maybe t
(!?) d k =  M.lookup k $ getMap d

-------------------------------------------------------------

-- | Stores information about the current entry (e.g. component).
data Info = Info { ldFile  :: FilePath
                 , ldLine  :: Int
                 , ldCol   :: Int
                 , comment :: Text
                 } deriving (Read,Show,Generic)

instance Binary Info

prettyInfo (Info f (-1) (-1) _) = f
prettyInfo (Info f l c _)  = f ++ " (" ++ show l ++ ":" ++ show c ++ ")"

-- | Dictionary associating an 'Id' with an entry (e.g. component), along with its
-- load history (newest to oldest).

type MapH t = Map (t,[Info])

-- | Dictionary entry insertion policy
data Policy = Keep | Replace deriving (Show, Eq)

infixl 9 !*, !^, !?!

-- | 'MapH' selector. Returs the entry with a given 'Id'.
(!*) :: (Show t) => MapH t -> Id -> Maybe t
(!*) d k = fst <$> d !? k

-- | 'MapH' selector. Returs the history for the entry with a given 'Id'.
(!^) :: (Show t) => MapH t -> Id -> [Info]
(!^) d k = snd $ d !?! k

-- | Makes a 'MapH' from a list of entries with their history.
mkDict :: [(Id, t, [Info])] -> MapH t
mkDict = Map . M.fromList . map (\(i,c,h) -> (i,(c,h)))

mapDict f = fmap (\(e,i) -> (f e, i))
   
-- | Inserts an entry into a 'MapH'. Depending on the 'Policy', if the entry exists:
--
-- * 'Replace' : replaces it and updates the history as the \"newest\" entry.
-- * 'Keep' : ignores it and keeps the existing one, and updates the history as the
--            \"oldest\" entry.
dictUpdate :: Policy -> Id -> t -> Info -> MapH t -> MapH t
dictUpdate Replace name c info = liftMap (M.insertWith fReplace name (c,[info]))
dictUpdate Keep    name c info = liftMap (M.insertWith fKeep name (c,[info]))

fReplace (a,newh) (_,oldh) = (a,newh++oldh)
fKeep    (_,newh) (a,oldh) = (a,oldh++newh)

dictUnion :: MapH t -> MapH t -> MapH t
dictUnion = liftMap2 (M.unionWithKey err)
  where
    err k (_,i1) (_,i2) = error $ "Found two components with the same ID " ++ show k
                          ++ " defined at:\n+++ " ++ prettyInfo (head i1)
                          ++ "\n+++ " ++ prettyInfo (head i2)

dictErr k d = "ID " ++ show k ++ " does not exist in the database with elements: "
              ++ (show $ ids d)
              -- ++ (TL.unpack $ pShow d)

