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
module CoInSyDe.Core.Dict (
  -- * CoInSyDe 'Map' Type
  Id, Map, mkMap, ids, entries, idEntries, (!?!),
  -- * History-Bookkeeping Map
  MapH, Info(..), Policy(..), prettyInfo,
  emptyDict, mkDict, mkInfoNode, (!*), (!^),
  dictUpdate, dictTransfer
  ) where

-- import Data.Typeable
import Data.Maybe(fromMaybe)
import Control.DeepSeq
import Data.Text as T (Text,unpack)
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)

import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as TL (unpack)

import CoInSyDe.Frontend (FNode,getInfo)

-- | Generic identifier used as library search key
type Id = Text 

-- | Dictionary type used throughout CoInSyDe
type Map k t = M.HashMap k t

-- | Dictionary associating an 'Id' with an entry (e.g. component), along with its
-- load history (newest to oldest).
type MapH t = Map Id (t,[Info])

-- | Stores information about the current entry (e.g. component).
data Info = Info {ldFile :: FilePath, ldInfo :: String} deriving (Read,Show)

prettyInfo (Info f "") = f
prettyInfo (Info f i)  = f ++ " (" ++ i ++ ")"

instance NFData Info where
  rnf _ = ()

-- | Dictionary entry insertion policy
data Policy = Keep | Replace deriving (Show, Eq)

infixl 9 !*, !^, !?!

-- | Genric 'Map' selector. Throws a more meaningful error message than the default
-- one.
(!?!) :: (Show t,Show k,Eq k,Hashable k) => Map k t -> k -> t
(!?!) d k = fromMaybe (error $ dictErr k d) (M.lookup k d)


-- | 'MapH' selector. Returs the entry with a given 'Id'.
(!*) :: (Show t) => MapH t -> Id -> t
(!*) d k = fst $ d !?! k

-- | 'MapH' selector. Returs the history for the entry with a given 'Id'.
(!^) :: (Show t) => MapH t -> Id -> [Info]
(!^) d k = snd $ d !?! k

ids = M.keys

entries = M.elems

idEntries = M.toList

mkMap :: (Eq k, Hashable k) => [(k,t)] -> Map k t
mkMap = M.fromList

-- | Makes a 'MapH' from a list of entries with their history.
mkDict :: [(Id, t, [Info])] -> MapH t
mkDict = M.fromList . map (\(i,c,h) -> (i,(c,h)))

emptyDict = M.empty

mkInfoNode :: FNode n => FilePath -> n -> Info
mkInfoNode path n = Info path (getInfo n)

-- | Inserts an entry into a 'MapH'. Depending on the 'Policy', if the entry exists:
--
-- * 'Replace' : replaces it and updates the history as the \"newest\" entry.
-- * 'Keep' : ignores it and keeps the existing one, and updates the history as the
--            \"oldest\" entry.
dictUpdate :: Policy -> Id -> t -> Info -> MapH t -> MapH t
dictUpdate Replace name c info = M.insertWith fReplace name (c,[info])
dictUpdate Keep    name c info = M.insertWith fKeep name (c,[info])

fReplace (a,newh) (_,oldh) = (a,newh++oldh)
fKeep    (_,newh) (a,oldh) = (a,oldh++newh)

-- | Transfers an entry from a dictionary to another.
dictTransfer :: Show t => Id -> MapH t -> MapH t -> MapH t
dictTransfer name src = M.insertWith (\_ a -> a) name (src !?! name)

dictErr k d = "ID " ++ show k ++ " does not exist in the database with elements: "
              -- ++ (show $ ids d)
              ++ (TL.unpack $ pShow d)

