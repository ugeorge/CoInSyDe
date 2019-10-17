{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
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

-- some aliases
type Id      = Text -- ^ Generic identifier used as library search key
type Name    = Text -- ^ Generic (e.g. variable) name, read from the user input
type Keyword = Text -- ^ A "reserved" keyword used in template identifiers, see 'TTm'

-- | Short alias for a port mapping
type PortMap = Map Name Port

-- | Short alias for a dictionary
type Dict t = Map Id t


-- | Generic class for types. Each language family needs to instantiate this class
-- with native type representations.
class Ty t where
  tyName :: t -> Id

-- | Generic class for port kinds. Each language needs to instantiate this class with
-- different port identifiers pointing to specific, relevant glue operators.
class Glue p where
  glName :: p -> Id
  glTy   :: Ty t => p -> t

-- | Port container pointing to some kind of glue mechanism (e.g. variables)
data Port where
    Port :: (Glue p, Show p, Eq p) => {pName :: Id, pGlue :: p} -> Port
deriving instance Show Port

-- | Container for functionals
data Fun where
  -- | Template functional. Contains template code managed by CoInSyDe
  TmFun :: { funName   :: Id        -- ^ unique function ID
           , inline    :: Bool
                       -- ^ True if template expanded inline. False is wrapped
                       --   according to language syntax (e.g. function call)
           , ports     :: PortMap   -- ^ maps user port names to glue containers
           , bindings  :: Map Name (Id, PortMap)
                       -- ^ maps a template functional identifier 'TFun' to an
                       --   existing (parsed) functional 'Fun', along with its new
                       --   port bindings
           , funTempl  :: [TTm]     -- ^ list of template terms
           } -> Fun
  -- | Native functional. Code used \"as-is\", no manipulation done.
  NvFun :: { funName   :: Id        -- ^ unique function ID
           , include   :: Bool      -- ^ True if external include
           , ports     :: PortMap   -- ^ maps user port names to glue containers
           , funCode   :: (FilePath, Text) -- ^ points to/contains native code
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
