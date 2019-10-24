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
-- This module contains the core types and generic methods to build them using the
-- "CoInSyDe.Frontend" API.
----------------------------------------------------------------------
module CoInSyDe.Core where

import Data.Text (Text,unpack)
import Data.Map.Lazy as M hiding (map,fold,filter)

import CoInSyDe.Frontend
import CoInSyDe.TTmParse

------------- ALIASES -------------

type Id      = Text -- ^ Generic identifier used as library search key
type Name    = Text -- ^ Generic (e.g. variable) name, read from the user input
type Keyword = Text -- ^ A "reserved" keyword used in template identifiers, see 'TTm'

type IfMap l = Map Name (If l) -- ^ Alias for a if dictionary
type Dict t  = Map Id t        -- ^ Alias for a generic dictionary

------------- CORE TYPES -------------

-- | Class for providing a common API for different target languages, where @l@ is
-- mainly a proxy type.
class ( Show (If l),   Read (If l)
      , Show (Type l), Read (Type l)
      , Show (Requ l), Read (Requ l)) => Target l where
  -- | A set of data type definitions, relevant to the target language.
  data Type l :: * 
  -- | A set of interface definitions, relevant to the target language.
  data If l   :: *
  -- | A set of special requirements relevant to the target language.
  data Requ l :: *
  -- | Constructor for data types used in common methods
  mkType :: FNode f => Id -> Dict (Type l) -> f -> Type l
  -- | Constructor for interfaces used in common methods. 
  mkIf   :: FNode f => Id -> Dict (Type l) -> f -> If l
  -- | constructor for requirement type
  mkRequ :: FNode f => f -> Requ l

-- | Container for functional components or glue operators
data Fun l where
  -- | Template functional. Contains template code managed by CoInSyDe
  TmFun :: Target l =>
           { funName  :: Id       -- ^ unique function ID
           , ifs      :: IfMap l  -- ^ maps user port names to if containers
           , reqs     :: [Requ l] -- ^ special requirements
           , inline   :: Bool
                      -- ^ True if template expanded inline. False is wrapped
                      --   according to language syntax (e.g. function call)
           , refs     :: Map Name (Instance l)
                      -- ^ maps a template functional identifier 'TFun' to an
                      --   existing (parsed) functional 'Fun', along with its new
                      --   port bindings
           , template :: [TTm]    -- ^ list of template terms
           } -> Fun l
  -- | Native functional. Code used \"as-is\", no manipulation done.
  NvFun :: Target l =>
           { funName :: Id        -- ^ unique function ID
           , ifs     :: IfMap l   -- ^ maps user port names to if containers
           , reqs    :: [Requ l]  -- ^ special requirements
           , funCode :: Either FilePath Text -- ^ points to/contains native code
           } -> Fun l
deriving instance Target l => Show (Fun l)
deriving instance Target l => Read (Fun l)

isInline (TmFun _ _ _ i _ _) = i
isInline NvFun{} = False

-- | Container used for storing a reference to a functional component. 
data Instance l where
  Bind :: (Target l) =>
          { theFun   :: Id      -- ^ functional component ID
          , bindings :: IfMap l -- ^ bindings between parent and component ports
          } -> Instance l
deriving instance Target l => Show (Instance l)
deriving instance Target l => Read (Instance l)
-- deriving instance Target l => Eq (Instance l)

------------- DICTIONARY BUILDERS -------------

-- | Builds a dictionary of functionals from all child nodes
--
-- > <root>/pattern|composite|template|native[@name=*,...]
--
-- Uses pretty much all of the other maker functions in this module as well from the
-- 'Target' API.
mkFunDict :: (Target l, FNode f)
          => (Id -> f -> [TTm]) -- ^ Function for creating composite templates,
                                --   relevant to the target language.
          -> Dict (Type l)      -- ^ Existing dictionary of types
          -> Dict (Fun l)       -- ^ Existing (library) dictionary of templates
          -> f                  -- ^ Root node.
          -> Dict (Fun l)       -- ^ Dictionary of templates created from this root.
mkFunDict mkComposite typeLib patternLib root =
  M.fromList $ patterns ++ composites ++ natives ++ templates
  where
    patterns   = map (mkTmFun mkLibTempl)  $ root |= "pattern"
    composites = map (mkTmFun mkComposite) $ root |= "composite"
    templates  = map (mkTmFun mkTextTempl) $ root |= "template"
    natives    = map mkNvFun               $ root |= "native"
    -----------------------------------
    mkNvFun node = (name, NvFun name ifs reqmnts code)
      where
        name    = node @! "name" 
        ifs   = mkIfDict typeLib name node
        reqmnts = mkRequirements node
        code    = getCode $ node @? "fromFile"
        getCode Nothing  = Right $ txtContent node
        getCode (Just a) = Left $ unpack a
    -----------------------------------
    mkTmFun mkTempl node = (name, TmFun name ifs reqmnts inline binds templ)
      where
        name    = node @! "name"
        inline  = ("call" `hasValue` "inline") node
        templ   = mkTempl name node
        ifs     = mkIfDict typeLib name node
        binds   = mkInstances ifs node
        reqmnts = mkRequirements node
    -----------------------------------       
    mkLibTempl  name node = template $ patternLib ! (node @! "type")
    mkTextTempl name node = textToTm (unpack name) (txtContent node)
    -----------------------------------       


-- | Builds a type dictionaty from all nodes
--
--  > <root>/type
-- 
-- Uses 'mkType' from the 'Target' API.
mkTypeDict :: (Target l, FNode f)
           => Dict (Type l)  -- ^ existing library of types
           -> f              -- ^ @\<root\>@ node
           -> Dict (Type l)  -- ^ new types defined by this @\<root\>@ node
mkTypeDict tyLib =  M.fromList . map mkType1 . children "type"
  where 
    mkType1 n = let name = n @! "name"
                in  (name, mkType name tyLib n) 

-- | Makes a dictionary of if operations from all the child nodes
--
-- > <parent>/interface[@name=*]
--
-- Uses 'mkIf' from the 'Target' API.
mkIfDict :: (Target l, FNode f)
           => Dict (Type l)  -- ^ library of types
           -> Id             -- ^ parent ID
           -> f              -- ^ @\<parent\>@ node
           -> IfMap l
mkIfDict typeLib parentId = M.fromList . map mkEntry . ifNodes
  where
    ifNodes = childrenOf ["port","intern","parameter"]
    mkEntry n = let name = n @! "name"
                in (name, mkIf parentId typeLib n)

-- | Makes a container for 'refs' (check definition of 'TmFun') from all child nodes
--
-- > <parent>/instance[@placeholder=*,@component=*]
--
-- Uses 'mkBindings'.
mkInstances :: (Target l, FNode f)
            => IfMap l      -- ^ parent's if map
            -> f              -- ^ @<parent>@ node
            -> Map Name (Instance l)
mkInstances parentIf = M.fromList . map mkInst . children "instance"
  where
    mkInst n = let to    = n @! "placeholder"
                   from  = n @! "component"
               in (to, Bind from (mkBindings parentIf n))

-- | Makes a dicionary of if operations associated based on the name bindings
-- infereed from all the child nodes
--
-- > instance/bind[@replace=*,@with=*]
--
-- The new if dictionary will contain the referred component's port names but with
-- the parent component's if types.
mkBindings :: (Target l, FNode f)
           => IfMap l -- ^ parent component's if dictionary
           -> f         -- ^ @instance@ node
           -> IfMap l -- ^ new if dictionary
mkBindings parentIfs = M.fromList . map mkBind . children "bind"
  where
    mkBind n = let from = n @! "with"
                   to   = n @! "replace"
                in (to, parentIfs ! from)

-- | Makes a list of requirements from all child nodes
--
-- > <parent>/requirement
--
-- Uses constructor 'mkRequ' from the 'Target' API.
mkRequirements :: (Target l, FNode f)
               => f
               -> [Requ l]
mkRequirements = map mkRequ . children "requirement"




