{-# LANGUAGE TypeFamilies #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoinSyDe.Frontend
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains a common API for reading different tree-based frontend
-- languages, such as XML or JSON.
----------------------------------------------------------------------
module CoInSyDe.Frontend where

import Data.Map.Lazy as M
import CoInSyDe.Core

class Frondent f where
  type Element f
