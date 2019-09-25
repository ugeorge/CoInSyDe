----------------------------------------------------------------------
-- |
-- Module      :  CoinSyDe.C.CLib
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This file contains the type and pattern libraries for the C backend.
----------------------------------------------------------------------
module CoinSyDe.C.CLib where

import CoinSyDe.C.Core
import Data.Map.Lazy as M

-- library of pre-defined types
tyLib :: Map TypeId Ty
tyLib = fromList
  [ ("int",    PrimTy "int")
  , ("bool_t", BoolTy "bool_t" "true" "false") 
  ]

-- library of pre-defined pattern templates
patLib :: Map FunId [TTm]
patLib = fromList 
  [ ("comb",  [FunTTm "func"])
  , ("moore", [FunTTm "ns_func", FunTTm "od_func"])
  , ("mealy", [FunTTm "ns_func", FunTTm "od_func"])
  ]
