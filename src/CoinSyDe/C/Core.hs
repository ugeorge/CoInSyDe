----------------------------------------------------------------------
-- |
-- Module      :  CoinSyDe.C.Core
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This file contains the core types and their instances.
----------------------------------------------------------------------
module CoinSyDe.C.Core where

import Data.Text
import Data.Map.Lazy as M
import Data.Text.Prettyprint.Doc (Doc)

-- some aliases
type C = Doc ()
type TypeId = String
type FunId  = String
type VarId  = String
type Name   = String
  

-- containers for all the supported types
data Ty = PrimTy  {tyName :: TypeId}
        | BoolTy  {tyName :: TypeId, true :: String, false :: String}
        | EnumTy  {tyName :: TypeId, vals :: [(String, Maybe String)]}
        | Struct  {tyName :: TypeId, entries :: Map String Ty}
        | Array   {tyName :: TypeId, tyCore :: Ty, size :: Int}
        | CircBuf {tyName :: TypeId, tyCore :: Ty, size :: Int, position :: Int}
        | NoTy
        deriving (Show, Eq)

-- identifier for the "port" kinds.
data PortTy = Input | Output | State | Variable deriving (Show,Eq)

-- container for variable
data Var = Var { varName :: VarId
               , varPort :: PortTy
               , varTy   :: Ty
               , varVal  :: Maybe String
               } deriving (Show, Eq)

-- container for functions
data Fun = Fun { funName   :: FunId
               , inline    :: Bool
               , ports     :: Map Name Var
               , bindings  :: Map Name (FunId, Map Name Var)
               , funTempl  :: [TTm]
               } deriving (Show)

-- abstract terms to represent template "code" 
data TTm = CodeTTm String
         | VarTTm  String
         | FunTTm  String
         deriving (Show)
