{-# LANGUAGE GADTs #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Core
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains the core types.
----------------------------------------------------------------------
module CoInSyDe.Core where

import Data.Text (Text)
import Data.Map.Lazy as M

import CoInSyDe.Frontend (FNode)

-- some aliases
type Id      = Text -- ^ Generic identifier used as library search key
type Name    = Text -- ^ Generic (e.g. variable) name, read from the user input
type Keyword = Text -- ^ A "reserved" keyword used in template identifiers, see 'TTm'
type Target  = String -- ^ An identifier for target language

type PortMap l = Map Name (Port l) -- ^ Alias for a port dictionary
type FunMap l  = Map Id (Fun l)    -- ^ Alias for a template dictionary
type Dict t    = Map Id t          -- ^ Alias for a generic dictionary

class Target l where
  type Type l
  type Glue l
  mkType :: FNode f => l -> Id -> f -> Type l
  mkGlue :: FNode f => l -> Id -> Dict t -> f -> Glue l

-- -- | Generic class for types. Each language family needs to instantiate this class
-- -- with native type representations.
-- class Ty t where
--   tyName :: t -> Id

-- -- | Generic class for port kinds. Each language needs to instantiate this class with
-- -- different port identifiers pointing to specific, relevant glue operators.
-- class Glue p where
--   mkGlue :: (FNode f, Ty t) => Id -> Dict t -> f -> p

-- | Port container pointing to some kind of glue mechanism (e.g. variables)
data Port l where
    Port :: Target l => {pName :: Id, pGlue :: Glue l} -> Port l

-- | Container for functionals
data Fun l where
  -- | Template functional. Contains template code managed by CoInSyDe
  TmFun :: Target l =>
           { funName   :: Id        -- ^ unique function ID
           , inline    :: Bool
                       -- ^ True if template expanded inline. False is wrapped
                       --   according to language syntax (e.g. function call)
           , ports     :: PortMap l -- ^ maps user port names to glue containers
           , bindings  :: Map Name (Id, PortMap l)
                       -- ^ maps a template functional identifier 'TFun' to an
                       --   existing (parsed) functional 'Fun', along with its new
                       --   port bindings
           , funTempl  :: [TTm]     -- ^ list of template terms
           } -> Fun
  -- | Native functional. Code used \"as-is\", no manipulation done.
  NvFun :: Target l =>
           { funName   :: Id        -- ^ unique function ID
           , ports     :: PortMap l -- ^ maps user port names to glue containers
           , funCode   :: Either FilePath Text -- ^ points to/contains native code
           } -> Fun
deriving instance Show Fun

-- | Abstract terms to represent templates. The CoInSyDe template language consists in
-- a list (i.e. a sequence) of 'TTm' terms.
--
-- The list of 'Keyword's following 'TPort' and 'TFun' are queries telling CoInSyDe to expand specific info. If the list is empty than the default expansion occurs.
data TTm = TCode Text            -- ^ target language code in textual format
         | TPort Name [Keyword]  -- ^ port identifier. By default expands the port name.
         | TFun  Name [Keyword]  -- ^ identifier for functionals. By default expands
                                 --   the functional code.
         deriving (Show)
