{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, OverloadedStrings #-}
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
module CoInSyDe.Core (
  Dict, Id, Name, IfMap, InstMap,
  Target(..), Comp(..), Instance(..),
  mkTypeLib, mkNativeLib, mkTemplateLib, mkPatternLib,
  mkCompositeLib, getTopModules,
  -- * Only internal

  -- | These are exported only for documentation purpose. Not to be used as such.
  mkIfDict,mkInstances,mkBindings,mkRequirements
  ) where

import Data.Typeable
import Data.Text as T (Text,unpack,strip)
import Data.Map.Lazy as M hiding (map,foldr,filter)
import Control.DeepSeq

import CoInSyDe.Frontend
import CoInSyDe.Core.Dict
import CoInSyDe.Core.TTm

------------- ALIASES -------------

type IfMap l    = Map Name (If l) -- ^ Alias for a if dictionary
type InstMap l  = Map Name (Instance l)

------------- CORE TYPES -------------

-- | Class for providing a common API for different target languages, where @l@ is
-- mainly a proxy type.
class ( Typeable l
      , Show (If l),   Read (If l),   NFData (If l)
      , Show (Type l), Read (Type l), NFData (Type l)
      , Show (Requ l), Read (Requ l), NFData (Requ l)
      ) => Target l where
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
  -- | Constructor for requirement type
  mkRequ :: FNode f => f -> Requ l
  -- | Constructor for a composite template
  mkComposite :: FNode f => l -> f -> [TTm]

-- | Container for functional components or glue operators
data Comp l where
  -- | Template functional. Contains template code managed by CoInSyDe
  TmComp :: Target l =>
           { funName  :: Id       -- ^ unique component ID
           , ifs      :: IfMap l  -- ^ maps (template) names to component interfaces
           , reqs     :: [Requ l] -- ^ special requirements for component
           , refs     :: InstMap l-- ^ maps a (template) function placeholder 'TFun'
                                  --   to an existing component, along with its new
                                  --   interface bindings
           , template :: [TTm]    -- ^ template code
           } -> Comp l
  -- | Native functional. Code used \"as-is\", no manipulation done.
  NvComp :: Target l =>
           { funName :: Id         -- ^ unique component ID
           , ifs     :: IfMap l    -- ^ maps port names to (caller) interfaces
           , reqs    :: [Requ l]   -- ^ special requirements for component
           , funCode :: Maybe Text -- ^ maybe native code
           } -> Comp l
deriving instance Target l => Show (Comp l)
deriving instance Target l => Read (Comp l)
instance  Target l => NFData (Comp l) where
  rnf (TmComp n i r f t) = rnf n `seq` rnf i `seq` rnf r `seq` rnf f `seq` rnf t
  rnf (NvComp n i r f) = rnf n `seq` rnf i `seq` rnf r `seq` rnf f

-- | Container used for storing a reference to a functional component. 
data Instance l where
  Bind :: (Target l) =>
          { refId    :: Id      -- ^ functional component ID
          , inline   :: Bool    -- ^ True if expanded inline, False if abstracted away
          , bindings :: IfMap l -- ^ bindings between parent and component ports
          } -> Instance l
deriving instance Target l => Show (Instance l)
deriving instance Target l => Read (Instance l)
instance  Target l => NFData (Instance l) where
  rnf (Bind n i b) = rnf n `seq` rnf i `seq` rnf b

------------- EXPORTED DICTIONARY BUILDERS -------------

-- | Builds a type dictionaty and load history from nodes
--
--  > <root>/type[@name=*,...]
-- 
-- Replaces existing entries if their IDs match. Made to be used with the CoInSyDe
-- library load scheme, see "CoInSyDe.LibManage". Uses 'mkType' from the 'Target' API.
mkTypeLib :: (Target l, FNode f)
          => Dict (Type l) -- ^ existing library of types
          -> FilePath      -- ^ file being loaded, for history bookkeeping
          -> f             -- ^ @\<root\>@ node
          -> Dict (Type l) -- ^ updated library of types
mkTypeLib tyLib fPath = foldr load tyLib . children "type"
  where
    load n lib
      = let name    = n @! "name"
            info    = mkInfoNode fPath n
        in dictReplace name (mkType name lib n) info lib

-- | Builds a component dictionaty and load history from nodes
--
--  > <root>/native[@name=*]CTEXT?
-- 
-- These nodes /might/ contain a @CTEXT@ field with the source code for the native
-- function. If it does not, then a @requirement@ child node pointing to the header
-- where the function is defined is necessary.
--
-- Does __not__ replace entry if ID exists, see "CoInSyDe.LibManage" for load scheme.
mkNativeLib :: (Target l, FNode f)
            => FilePath       -- ^ file being loaded, for history bookkeeping
            -> Dict (Type l)  -- ^ (fully-loaded) library of types
            -> Dict (Comp l)  -- ^ existing library of components
            -> f              -- ^ @\<root\>@ node
            -> Dict (Comp l)  -- ^ updated library of components
mkNativeLib fPath typeLib compLib = foldr load compLib . children "native"
  where
    load n lib
      = let name    = n @! "name"
            reqmnts = mkRequirements n
            interfs = mkIfDict typeLib name n
            code    = case (strip $ getTxt n,reqmnts) of
                        ("",[]) -> error $ "Native node " ++ show name ++ " in file "
                                   ++ show fPath ++ ": code or requirement missing!"
                        ("",_)  -> Nothing
                        (c,_)   -> Just c
            info    = mkInfoNode fPath n
            newComp = NvComp name interfs reqmnts code
        in dictKeep name newComp info lib 

-- | Builds a component dictionaty and load history from nodes
--
--  > <root>/template[@name=*]CTEXT
-- 
-- The @CTEXT@ needs to be written in a template langiage, see 'TTm'.
--
-- Does __not__ replace entry if ID exists, see "CoInSyDe.LibManage" for load scheme.
mkTemplateLib :: (Target l, FNode f)
              => FilePath       -- ^ file being loaded, for history bookkeeping
              -> Dict (Comp l)  -- ^ existing library of components
              -> f              -- ^ @\<root\>@ node
              -> Dict (Comp l)  -- ^ updated library of components
mkTemplateLib fPath compLib = foldr load compLib . children "template"
  where
    load n lib
      = let name    = n @! "name"
            reqmnts = mkRequirements n
            templ   = textToTm (unpack name) (getTxt n)
            info    = mkInfoNode fPath n
            newComp = TmComp name empty reqmnts empty templ
        in dictKeep name newComp info lib 

-- | Builds a component dictionaty and load history from all nodes
--
--  > <root>/pattern[@name=*,@type=*]
-- 
-- __OBS:__ Patterns are usually found in project files, and require that all template
-- libraries have been fully loaded. Replaces any previously-loaded component with the
-- same name. TODO: is this right, or should it throw error?
mkPatternLib :: (Target l, FNode f)
             => FilePath      -- ^ file being loaded, for history bookkeeping
             -> Dict (Type l) -- ^ (fully-loaded) library of types
             -> Dict (Comp l) -- ^ existing library of components
             -> f             -- ^ @\<root\>@ node
             -> Dict (Comp l) -- ^ updated library of components
mkPatternLib fPath typeLib compLib = foldr load compLib . children "pattern"
  where
    load n lib
      = let name    = n @! "name"
            interfs = mkIfDict typeLib name n
            reqmnts = mkRequirements n
            binds   = mkInstances interfs n
            templ   = template $ lib !* (n @! "type")
            info    = mkInfoNode fPath n
            newComp = TmComp name interfs reqmnts binds templ
        in dictReplace name newComp info lib 

-- | Builds a component dictionaty and load history from nodes
--
--  > <root>/composite[@name=*,@type=*]CTEXT?
-- 
-- The @CTEXT@ might be template code, see 'TTm', in which case it overrides the
-- default composite code template of the target language.
--
-- __OBS:__ Composites are usually found in project files, and require that all
-- template libraries have been fully loaded. Replaces any previously-loaded component
-- with the same name. TODO: is this right, or should it throw error?
mkCompositeLib :: (Target l, FNode f)
               => l             -- ^ proxy to determine language
               -> FilePath      -- ^ file being loaded, for history bookkeeping
               -> Dict (Type l) -- ^ (fully-loaded) library of types
               -> Dict (Comp l) -- ^ existing library of components
               -> f             -- ^ @\<root\>@ node
               -> Dict (Comp l) -- ^ updated library of components
mkCompositeLib lang fPath typeLib compLib = foldr load compLib . children "composite"
  where
    load n
      = let name    = n @! "name"
            interfs = mkIfDict typeLib name n
            reqmnts = mkRequirements n
            binds   = mkInstances interfs n
            templ   = case strip $ getTxt n of
                        "" -> mkComposite lang n
                        tm -> textToTm (unpack name) tm
            info    = mkInfoNode fPath n
            newComp = TmComp name interfs reqmnts binds templ
        in dictReplace name newComp info

getTopModules :: FNode f => String -> f -> [Id]
getTopModules tName node = map (@!"name") tops
  where tops = filterByAttr "type" tName $ node |= "composite"


------------- INTERNAL DICTIONARY BUILDERS -------------

-- | Makes a dictionary of interfaces operations from all the child nodes
--
-- > <parent>/iport|oport|intern[@name=*]
--
-- Uses 'mkIf' from the 'Target' API.
mkIfDict :: (Target l, FNode f)
           => Dict (Type l)  -- ^ library of types
           -> Id             -- ^ parent ID
           -> f              -- ^ @\<parent\>@ node
           -> IfMap l
mkIfDict typeLib parentId = M.fromList . map mkEntry . ifNodes
  where
    ifNodes = childrenOf ["iport","oport","intern"]
    mkEntry n = let name = n @! "name"
                in (name, mkIf parentId typeLib n)

-- | Makes a container for 'refs' (check definition of 'TmFun') from all child nodes
--
-- > <parent>/instance[@placeholder=*,@component=*,call="inline"|*]
--
-- Uses 'mkBindings'.
mkInstances :: (Target l, FNode f)
            => IfMap l   -- ^ parent's interface dictionary
            -> f         -- ^ @<parent>@ node
            -> Map Name (Instance l)
mkInstances parentIfs = M.fromList . map mkInst . children "instance"
  where
    mkInst n = let to   = n @! "placeholder"
                   from = n @! "component"
                   inln = ("call" `hasValue` "inline") n
               in (to, Bind from inln (mkBindings parentIfs n))

-- | Makes a dicionary of interfaces based on the name bindings infereed from all the
-- child nodes
--
-- > instance/bind[@replace=*,@with=*]
--
-- The new if dictionary will contain the referred component's interface names but
-- with the parent component's interface types.
mkBindings :: (Target l, FNode f)
           => IfMap l -- ^ parent component's interface dictionary
           -> f              -- ^ @instance@ node
           -> IfMap l -- ^ new interface dictionary
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

