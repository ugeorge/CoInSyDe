{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving, DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
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
import Data.Binary

import CoInSyDe.Frontend (FNode)

-- some aliases
type Id      = Text -- ^ Generic identifier used as library search key
type Name    = Text -- ^ Generic (e.g. variable) name, read from the user input
type Keyword = Text -- ^ A "reserved" keyword used in template identifiers, see 'TTm'

type PortMap l = Map Name (Port l) -- ^ Alias for a port dictionary
type Dict t    = Map Id t          -- ^ Alias for a generic dictionary

-- | Class for providing a common API for different target languages, where @l@ is
-- mainly a proxy type.
class ( Show (Glue l), Read (Glue l)
      , Show (Type l), Read (Type l)) => Target l where
  -- | A set of data type definitions, relevant to the target language.
  data Type l :: * 
  -- | A set of glue operation definitions, relevant to the target language.
  data Glue l :: *
  -- | Constructor for data types used in common methods
  mkType :: FNode f => Id -> f -> Type l
  -- | Constructor for glue operations used in common methods. 
  mkGlue :: FNode f => Id -> Dict (Type l) -> f -> Glue l

-- | Port container pointing to some kind of glue mechanism (e.g. variables)
data Port l where
    Port :: Target l => {pName :: Id, pGlue :: Glue l} -> Port l
deriving instance Target l => Show (Port l)
deriving instance Target l => Read (Port l)

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
           } -> Fun l
  -- | Native functional. Code used \"as-is\", no manipulation done.
  NvFun :: Target l =>
           { funName   :: Id        -- ^ unique function ID
           , ports     :: PortMap l -- ^ maps user port names to glue containers
           , funCode   :: Either FilePath Text -- ^ points to/contains native code
           } -> Fun l
deriving instance Target l => Show (Fun l)
deriving instance Target l => Read (Fun l)

-- | Abstract terms to represent templates. The CoInSyDe template language consists in
-- a list (i.e. a sequence) of 'TTm' terms.
--
-- The list of 'Keyword's following 'TPort' and 'TFun' are queries telling CoInSyDe to
-- expand specific info. If the list is empty than the default expansion occurs.
data TTm = TCode Text            -- ^ target language code in textual format
         | TPort Name [Keyword]  -- ^ placeholder for port identifier. Default expands
                                 --   to port name.
         | TFun  Name [Keyword]  -- ^ placeholder for functional template
                                 --   identifier. Default expands to functional code.
         deriving (Show,Read)
