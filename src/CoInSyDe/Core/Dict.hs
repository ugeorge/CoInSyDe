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
  -- * Types
  Id, Dict, Info(..), Policy(..),
  -- * Constructors
  emptyDict, mkDict, mkInfoNode,
  -- * Utilities
  (!*), (!^), (!?!), ids,
  dictUpdate, dictTransfer
  ) where

import Data.Typeable
import Data.Maybe(fromMaybe)
import Control.DeepSeq
import Data.Text as T (Text,unpack)
import Data.Map.Lazy as M hiding (map,foldr,filter)

import CoInSyDe.Frontend (FNode,getInfo)

type Id = Text -- ^ Generic identifier used as library search key

-- | Dictionary associating an 'Id' with an entry (e.g. component), along with its
-- load history (newest to oldest).
type Dict t = Map Id (t,[Info])

-- type History = [Info]

-- | Stores information about the current entry (e.g. component).
data Info = Info {ldFile :: FilePath, ldInfo :: String} deriving (Show,Read)
instance NFData Info where
  rnf _ = ()

data Policy = Keep | Replace deriving (Show, Eq)

emptyDict = empty

mkInfoNode :: FNode n => FilePath -> n -> Info
mkInfoNode path n = Info path (getInfo n)

infixl 9 !*, !^, !?!

-- | 'Dict' selector. Returs the entry with a given 'Id'.
(!*) :: Typeable t => Dict t -> Id -> t
(!*) d k = fst $ fromMaybe (error $ dictErr k d) (d !? k)

-- | 'Dict' selector. Returs the history for the entry with a given 'Id'.
(!^) :: Typeable t => Dict t -> Id -> [Info]
(!^) d k = snd $  fromMaybe (error $ dictErr k d) (d !? k)

-- | Genric 'Map' selector. Throws a more meaningful error message than the default
-- one.
(!?!) :: (Show t, Typeable k, Ord k, Show k) => Map k t -> k -> t
(!?!) d k = fromMaybe (error $ dictErr1 k d) (d !? k)

ids = keys

-- | Makes a 'Dict' from a list of entries with their history.
mkDict :: [(Id, t, [Info])] -> Dict t
mkDict = M.fromList . map (\(i,c,h) -> (i,(c,h)))

-- | Inserts an entry into a 'Dict'. Depending on the 'Policy', if the entry exists:
--
-- * 'Replace' : replaces it and updates the history as the \"newest\" entry.
-- * 'Keep' : ignores it and keeps the existing one, and updates the history as the
--            \"oldest\" entry.
dictUpdate :: Policy -> Id -> t -> Info -> Dict t -> Dict t
dictUpdate Replace name c info = insertWith fReplace name (c,[info])
dictUpdate Keep    name c info = insertWith fKeep name (c,[info])

fReplace (a,newh) (_,oldh) = (a,newh++oldh)
fKeep    (_,newh) (a,oldh) = (a,oldh++newh)

-- | Transfers an entry from a dictionary to another.
dictTransfer :: Typeable t => Id -> Dict t -> Dict t -> Dict t
dictTransfer name src = insertWith (\_ a -> a) name entry
  where entry = fromMaybe (error $ dictErr name src) (src !? name)

dictErr k d = "ID " ++ show k ++ " does not exist in the database of type: "
              ++ show (typeOf d)
dictErr1 k d = "ID " ++ show k ++ " does not exist in the database of type: "
              ++ show d

