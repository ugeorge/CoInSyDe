{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving, DeriveAnyClass #-}
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
module CoInSyDe.Core where

import Data.Text as T (Text,unpack,null)
import Data.Map.Lazy as M hiding (map,foldr,filter)

import CoInSyDe.Frontend
import CoInSyDe.TTm

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
  -- | Constructor for requirement type
  mkRequ :: FNode f => f -> Requ l
  -- | Constructor for a composite template
  mkComposite :: FNode f => l -> f -> [TTm]

-- | Container for functional components or glue operators
data Comp l where
  -- | Template functional. Contains template code managed by CoInSyDe
  TmComp :: Target l =>
           { funName  :: Id       -- ^ unique function ID
           , ifs      :: IfMap l  -- ^ maps user port names to interface containers
           , reqs     :: [Requ l] -- ^ special requirements
           , refs     :: Map Name (Instance l)
                      -- ^ maps a template functional identifier 'TFun' to an
                      --   existing (parsed) functional 'Comp', along with its new
                      --   port bindings
           , template :: [TTm]    -- ^ list of template terms
           } -> Comp l
  -- | Native functional. Code used \"as-is\", no manipulation done.
  NvComp :: Target l =>
           { funName :: Id         -- ^ unique function ID
           , ifs     :: IfMap l    -- ^ maps user port names to interface containers
           , reqs    :: [Requ l]   -- ^ special requirements
           , funCode :: Maybe Text -- ^ points to/contains native code
           } -> Comp l
deriving instance Target l => Show (Comp l)
deriving instance Target l => Read (Comp l)

-- | Container used for storing a reference to a functional component. 
data Instance l where
  Bind :: (Target l) =>
          { theComp  :: Id      -- ^ functional component ID
          , inline   :: Bool    -- ^ True if expanded inline, False if abstracted away
          , bindings :: IfMap l -- ^ bindings between parent and component ports
          } -> Instance l
deriving instance Target l => Show (Instance l)
deriving instance Target l => Read (Instance l)
-- deriving instance Target l => Eq (Instance l)

------------- EXPORTED DICTIONARY BUILDERS -------------

type LdHistory d = (Dict [FilePath], Dict d)
emptyHistory = (empty,empty)

-- | Builds a type dictionaty and load history from nodes
--
--  > <root>/type[@name=*,...]
-- 
-- Replaces existing entries if their IDs match. Made to be used with the CoInSyDe
-- library load scheme, see "CoInSyDe.LibManage". Uses 'mkType' from the 'Target' API.
mkTypeLib :: (Target l, FNode f)
          => LdHistory (Type l)  -- ^ existing library of types
          -> FilePath            -- ^ file being loaded, for history bookkeeping
          -> f                   -- ^ @\<root\>@ node
          -> LdHistory (Type l)  -- ^ updated library of types
mkTypeLib tyLibH fPath =  foldr ldRepl tyLibH . children "type"
  where
    ldRepl n (hist,lib)
      = let name    = n @! "name"
            newHist = insertWith (++) name [fPath] hist
            newLib  = insert name (mkType name lib n) lib
        in (newHist,newLib)

-- | Builds a component dictionaty and load history from nodes
--
--  > <root>/native[@name=*]CTEXT?
-- 
-- These nodes /might/ contain a @CTEXT@ field with the source code for the native
-- function. If it does not, then a @requirement@ child node pointing to the library
-- file where the function is defined is necessary.
--
-- Does __not__ replace entry if ID exists, see "CoInSyDe.LibManage" for load scheme.
mkNativeLib :: (Target l, FNode f)
            => FilePath            -- ^ file being loaded, for history bookkeeping
            -> Dict (Type l)       -- ^ (fully-loaded) library of types
            -> LdHistory (Comp l)  -- ^ existing library of components
            -> f                   -- ^ @\<root\>@ node
            -> LdHistory (Comp l)  -- ^ updated library of components
mkNativeLib fPath typeLib compLibH = foldr ldRepl compLibH . children "native"
  where
    ldRepl n (hist,lib)
      = let name    = n @! "name"
            reqmnts = mkRequirements n
            interfs = mkIfDict typeLib name n
            code    = case (txtContent n,reqmnts) of
                        ("",[]) -> error $ "Native node " ++ show name ++ " in file "
                                   ++ show fPath ++ ": code or requirement missing!"
                        ("",_)  -> Nothing
                        (c,_)   -> Just c
            newComp = NvComp name interfs reqmnts code
            newHist = insertWith (++) name [fPath] hist
            newLib  | name `member` lib = lib
                    | otherwise         = insert name newComp lib
        in (newHist,newLib)

-- | Builds a component dictionaty and load history from nodes
--
--  > <root>/template[@name=*]CTEXT
-- 
-- The @CTEXT@ needs to be written in a template langiage, see 'TTm'.
--
-- Does __not__ replace entry if ID exists, see "CoInSyDe.LibManage" for load scheme.
mkTemplateLib :: (Target l, FNode f)
              => FilePath            -- ^ file being loaded, for history bookkeeping
              -> LdHistory (Comp l)  -- ^ existing library of components
              -> f                   -- ^ @\<root\>@ node
              -> LdHistory (Comp l)  -- ^ updated library of components
mkTemplateLib fPath compLibH = foldr ldRepl compLibH . children "template"
  where
    ldRepl n (hist,lib)
      = let name    = n @! "name"
            reqmnts = mkRequirements n
            templ   = textToTm (unpack name) (txtContent n)
            newComp = TmComp name empty reqmnts empty templ
            newHist = insertWith (++) name [fPath] hist
            newLib  | name `member` lib = lib
                    | otherwise         = insert name newComp lib
        in (newHist,newLib)

-- | Builds a component dictionaty and load history from all nodes
--
--  > <root>/pattern[@name=*,@type=*]
-- 
-- __OBS:__ Patterns are usually found in project files, and require that all template
-- libraries have been fully loaded. Replaces any previously-loaded component with the
-- same name. TODO: is this right, or should it throw error?
mkPatternLib :: (Target l, FNode f)
             => FilePath            -- ^ file being loaded, for history bookkeeping
             -> Dict (Type l)       -- ^ (fully-loaded) library of types
             -> LdHistory (Comp l)  -- ^ existing library of components
             -> f                   -- ^ @\<root\>@ node
             -> LdHistory (Comp l)  -- ^ updated library of components
mkPatternLib fPath typeLib compLibH = foldr ldRepl compLibH . children "pattern"
  where
    ldRepl n (hist,lib)
      = let name    = n @! "name"
            interfs = mkIfDict typeLib name n
            reqmnts = mkRequirements n
            binds   = mkInstances interfs n
            templ   = template $ lib ! (n @! "type")
            newComp = TmComp name interfs reqmnts binds templ
            newHist = insertWith (++) name [fPath] hist
            newLib  = insert name newComp lib
        in (newHist,newLib)

-- | Builds a component dictionaty and load history from nodes
--
--  > <root>/composite[@name=*,@type=*]CTEXT?
-- 
-- __OBS:__ Patterns are usually found in project files, and require that all template
-- libraries have been fully loaded. Replaces any previously-loaded component with the
-- same name. TODO: is this right, or should it throw error?
mkCompositeLib :: (Target l, FNode f)
               => l                   -- ^ proxy to determine language
               -> FilePath            -- ^ file being loaded, for history bookkeeping
               -> Dict (Type l)       -- ^ (fully-loaded) library of types
               -> LdHistory (Comp l)  -- ^ existing library of components
               -> f                   -- ^ @\<root\>@ node
               -> LdHistory (Comp l)  -- ^ updated library of components
mkCompositeLib lang fPath typeLib compLibH =
  foldr ldRepl compLibH . children "composite"
  where
    ldRepl n (hist,lib)
      = let name    = n @! "name"
            interfs = mkIfDict typeLib name n
            reqmnts = mkRequirements n
            binds   = mkInstances interfs n
            templ   = case txtContent n of
                        "" -> mkComposite lang n
                        tm -> textToTm (unpack name) tm
            newComp = TmComp name interfs reqmnts binds templ
            newHist = insertWith (++) name [fPath] hist
            newLib  = insert name newComp lib
        in (newHist,newLib)

------------- INTERNAL DICTIONARY BUILDERS -------------

-- | Makes a dictionary of interfaces operations from all the child nodes
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
    ifNodes = childrenOf ["port","intern"]
    mkEntry n = let name = n @! "name"
                in (name, mkIf parentId typeLib n)

-- | Makes a container for 'refs' (check definition of 'TmFun') from all child nodes
--
-- > <parent>/instance[@placeholder=*,@component=*]
--
-- Uses 'mkBindings'.
mkInstances :: (Target l, FNode f)
            => IfMap l   -- ^ parent's interface dictionary
            -> f         -- ^ @<parent>@ node
            -> Map Name (Instance l)
mkInstances parentIfs = M.fromList . map mkInst . children "instance"
  where
    mkInst n = let to     = n @! "placeholder"
                   from   = n @! "component"
                   inline = ("call" `hasValue` "inline") n
               in (to, Bind from inline (mkBindings parentIfs n))

-- | Makes a dicionary of interfaces based on the name bindings infereed from all the
-- child nodes
--
-- > instance/bind[@replace=*,@with=*]
--
-- The new if dictionary will contain the referred component's interface names but
-- with the parent component's interface types.
mkBindings :: (Target l, FNode f)
           => IfMap l -- ^ parent component's interface dictionary
           -> f       -- ^ @instance@ node
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

