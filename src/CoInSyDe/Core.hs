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
  -- * Aliases and convenience types
  Id, Name, Map, MapH, IfMap, InstMap,
  mkMap, ids, entries, (!?!),
  -- * Core Types
  Target(..), Comp(..), Instance(..),
  -- * Core Type Constructors
  mkTypeLib, mkNativeLib, mkTemplateLib, mkPatternLib,
  -- * Only internal

  -- | These are exported only for documentation purpose. Not to be used as such.
  mkIfs,mkInstances,mkBindings,mkRequirements
  ) where

import Data.Typeable
import Data.Text as T (Text,unpack,strip)
import Control.DeepSeq

import CoInSyDe.Frontend
import CoInSyDe.Core.Dict

------------- ALIASES -------------

type Name = Text
type IfMap l    = Map Name (If l)
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
  mkType :: FNode f => Id -> MapH (Type l) -> f -> Type l
  -- | Constructor for interfaces used in common methods. 
  mkIf   :: FNode f => Id -> MapH (Type l) -> f -> If l
  -- | Constructor for requirement type
  mkRequ :: FNode f => f -> Requ l
  -- | Constructor for a text/macro
  mkMacro :: Text -> If l

-- | Container for functional components or glue operators
data Comp l where
  -- | Template functional. Contains template code managed by CoInSyDe
  TmComp :: Target l =>
           { cpName :: Id       -- ^ unique component ID
           , ifs    :: IfMap l  -- ^ maps (template) names to component interfaces
           , reqs   :: [Requ l] -- ^ special requirements for component
           , refs   :: InstMap l-- ^ maps a (template) function placeholder 'TFun' to
                                -- an existing component, along with its new interface
                                -- bindings
           , template :: String -- ^ template code
           } -> Comp l
  -- | Native functional. Code used \"as-is\", no manipulation done.
  NvComp :: Target l =>
           { cpName  :: Id         -- ^ unique component ID
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
  Ref :: (Target l) =>
         { refId    :: Id      -- ^ functional component ID
         , inline   :: Bool    -- ^ True if expanded inline, False if abstracted away
         , bindings :: IfMap l -- ^ bindings between parent and component ports
         } -> Instance l
deriving instance Target l => Show (Instance l)
deriving instance Target l => Read (Instance l)
instance  Target l => NFData (Instance l) where
  rnf (Ref n i b) = rnf n `seq` rnf i `seq` rnf b

------------- EXPORTED DICTIONARY BUILDERS -------------

-- | Builds a type dictionaty and load history from nodes
--
--  > <root>/type[@name=*,...]
-- 
-- Replaces existing entries if their IDs match. Made to be used with the CoInSyDe
-- library load scheme, see "CoInSyDe.LibManage". Uses 'mkType' from the 'Target' API.
mkTypeLib :: (Target l, FNode f)
          => MapH (Type l) -- ^ existing library of types
          -> FilePath      -- ^ file being loaded, for history bookkeeping
          -> f             -- ^ @\<root\>@ node
          -> MapH (Type l) -- ^ updated library of types
mkTypeLib tyLib fPath = foldr load tyLib . children "type"
  where
    load n lib
      = let name    = n @! "name"
            info    = mkInfoNode fPath n
        in dictUpdate Replace name (mkType name lib n) info lib

-- | Builds a component dictionaty and load history from nodes
--
--  > <root>/native[@name=*]CTEXT?
-- 
-- These nodes /might/ contain a @CTEXT@ field with the source code for the native
-- function. If it does not, then a @requirement@ child node pointing to the header
-- where the function is defined is necessary.
mkNativeLib :: (Target l, FNode f)
            => Policy         -- ^ update policy in case of name clashes
            -> FilePath       -- ^ file being loaded, for history bookkeeping
            -> MapH (Type l)  -- ^ (fully-loaded) library of types
            -> MapH (Comp l)  -- ^ existing library of components
            -> f              -- ^ @\<root\>@ node
            -> MapH (Comp l)  -- ^ updated library of components
mkNativeLib policy fPath typeLib compLib = foldr load compLib . children "native"
  where
    load n lib
      = let name    = n @! "name"
            reqmnts = mkRequirements n
            interfs = mkIfs typeLib name n
            code    = case (getTxt n,reqmnts) of
                        ("",[]) -> error $ "Native node " ++ show name ++ " in file "
                                   ++ show fPath ++ ": code or requirement missing!"
                        ("",_)  -> Nothing
                        (c,_)   -> Just c
            info    = mkInfoNode fPath n
            newComp = NvComp name interfs reqmnts code
        in dictUpdate policy name newComp info lib 

-- | Builds a component dictionaty and load history from nodes
--
--  > <root>/template[@name=*]CTEXT
-- 
-- The @CTEXT@ needs to be written in a template langiage, see 'TTm'.
mkTemplateLib :: (Target l, FNode f)
              => Policy         -- ^ update policy in case of name clashes
              -> FilePath       -- ^ file being loaded, for history bookkeeping
              -> MapH (Type l)  -- ^ (fully-loaded) library of types
              -> MapH (Comp l)  -- ^ existing library of components
              -> f              -- ^ @\<root\>@ node
              -> MapH (Comp l)  -- ^ updated library of components
mkTemplateLib policy fPath typeLib compLib = foldr load compLib . children "template"
  where
    load n lib
      = let name    = n @! "name"
            interfs = mkIfs typeLib name n
            binds   = mkInstances interfs n
            reqmnts = mkRequirements n
            templ   = unpack $ getTxt n
            info    = mkInfoNode fPath n
            newComp = TmComp name interfs reqmnts binds templ
        in dictUpdate policy name newComp info lib 

-- | Builds a component dictionaty and load history from all nodes
--
--  > <root>/pattern[@name=*,@type=*]
mkPatternLib :: (Target l, FNode f)
             => Policy         -- ^ update policy in case of name clashes
             -> FilePath      -- ^ file being loaded, for history bookkeeping
             -> MapH (Type l) -- ^ (fully-loaded) library of types
             -> MapH (Comp l) -- ^ existing library of components
             -> f             -- ^ @\<root\>@ node
             -> MapH (Comp l) -- ^ updated library of components
mkPatternLib policy fPath typeLib compLib = foldr load compLib . children "pattern"
  where
    load n lib
      = let name    = n @! "name"
            interfs = mkIfs typeLib name n
            reqmnts = mkRequirements n
            binds   = mkInstances interfs n
            templ   = template $ lib !* (n @! "type")
            info    = mkInfoNode fPath n
            newComp = TmComp name interfs reqmnts binds templ
        in dictUpdate policy name newComp info lib 

------------- INTERNAL DICTIONARY BUILDERS -------------

-- | Makes a dictionary of interfaces operations from all the child nodes
--
-- > <parent>/interface[@name=*]
--
-- Uses 'mkIf' from the 'Target' API.
--
-- __OBS!__ Interface names are /not allowed/ to bear the same name as the  <https://jinja.palletsprojects.com/en/2.10.x/templates/#list-of-builtin-filters built-in Jinja functions>!
--
-- TODO: find out which other keywords are illegal (e.g. @in@).
mkIfs :: (Target l, FNode f)
           => MapH (Type l)  -- ^ library of types
           -> Id             -- ^ parent ID
           -> f              -- ^ @\<parent\>@ node
           -> IfMap l
mkIfs typeLib parentId = mkMap . map mkEntry . children "interface"
  where
    mkEntry n = let name = n @! "name"
                in (name, mkIf parentId typeLib n)

-- | Makes a container for 'refs' (check definition of 'TmFun') from all child nodes
--
-- > <parent>/instance[@placeholder=*,@component=*,inline=true|false]
--
-- Uses 'mkBindings'.
mkInstances :: (Target l, FNode f)
            => IfMap l   -- ^ parent's interface dictionary
            -> f         -- ^ @<parent>@ node
            -> Map Name (Instance l)
mkInstances parentIfs = mkMap . map mkInst . children "instance"
  where
    mkInst n = let to   = n @! "placeholder"
                   from = n @! "component"
                   inln = n @^ "inline"
               in (to, Ref from inln (mkBindings parentIfs n))

-- | Makes a dicionary of interfaces based on the name bindings infereed from all the
-- child nodes
--
-- > instance/bind[@replace=*,@with=*|@withText=*]
--
-- The new if dictionary will contain the referred component's interface names but
-- with the parent component's interface types.
mkBindings :: (Target l, FNode f)
           => IfMap l -- ^ parent component's interface dictionary
           -> f       -- ^ @instance@ node
           -> IfMap l -- ^ new interface dictionary
mkBindings parentIfs = mkMap . map mkBind . children "bind"
  where
    mkBind n = case (n @? "with",n @? "withText") of
                 (Just from, Nothing) -> (n @! "replace", parentIfs !?! from)
                 (Nothing, Just val)  -> (n @! "replace", mkMacro val)
                 _ -> error $ "<bind> node malformed: " ++ show n

-- | Makes a list of requirements from all child nodes
--
-- > <parent>/requirement
--
-- Uses constructor 'mkRequ' from the 'Target' API.
mkRequirements :: (Target l, FNode f)
               => f
               -> [Requ l]
mkRequirements = map mkRequ . children "requirement"

