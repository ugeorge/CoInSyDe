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
import Data.Map.Lazy as M hiding (map,foldr,filter)

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
           { funName :: Id        -- ^ unique function ID
           , ifs     :: IfMap l   -- ^ maps user port names to interface containers
           , reqs    :: [Requ l]  -- ^ special requirements
           , funCode :: Either FilePath Text -- ^ points to/contains native code
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
          -> Dict (Comp l)      -- ^ Existing (library) dictionary of templates
          -> f                  -- ^ Root node.
          -> Dict (Comp l)      -- ^ Dictionary of templates created from this root.
mkFunDict mkComposite typeLib patternLib root =
  M.fromList $ patterns ++ composites ++ natives ++ templates
  where
    patterns   = map (mkTmComp mkLibTempl)  $ root |= "pattern"
    composites = map (mkTmComp mkComposite) $ root |= "composite"
    templates  = map (mkTmComp mkTextTempl) $ root |= "template"
    natives    = map mkNvComp               $ root |= "native"
    -----------------------------------
    mkNvComp node = (name, NvComp name interfs reqmnts code)
      where
        name    = node @! "name" 
        interfs = mkIfDict typeLib name node
        reqmnts = mkRequirements node
        code    = getCode $ node @? "fromFile"
        getCode Nothing  = Right $ txtContent node
        getCode (Just a) = Left $ unpack a
    -----------------------------------
    mkTmComp mkTempl node = (name, TmComp name interfs reqmnts binds templ)
      where
        name    = node @! "name"
        templ   = mkTempl name node
        interfs = mkIfDict typeLib name node
        binds   = mkInstances interfs node
        reqmnts = mkRequirements node
    -----------------------------------       
    mkLibTempl  name node = template $ patternLib ! (node @! "type")
    mkTextTempl name node = textToTm (unpack name) (txtContent node)
    -----------------------------------       


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

---------------------------------------------------------------------

type LdHistory d = (Dict [FilePath], Dict d)

-- | Builds a type dictionaty and load history from all nodes
--
--  > <root>/type[@name=*,...]
-- 
-- Replaces existing entries if their IDs match. Made to be used with the CoInSyDe
-- library load scheme, see "CoInSyDe.LibManage". Uses 'mkType' from the 'Target' API.
loadTypeLib :: (Target l, FNode f)
            => LdHistory (Type l)  -- ^ existing library of types
            -> FilePath            -- ^ file being loaded, for history bookkeeping
            -> f                   -- ^ @\<root\>@ node
            -> LdHistory (Type l)  -- ^ updated library of types
loadTypeLib tyLibH fPath =  foldr ldRepl tyLibH . children "type"
  where
    ldRepl n (hist,lib)
      = let name    = n @! "name"
            newHist = insertWith (++) name [fPath] hist
            newLib  = insert name (mkType name lib n) lib
        in (newHist,newLib)

-- | Builds a component dictionaty and load history from all nodes
--
--  > <root>/template[@name=*,...]
-- 
-- Replaces existing entries if their IDs match. Made to be used with the CoInSyDe
-- library load scheme, see "CoInSyDe.LibManage". Uses 'mkRequirements'.
loadTemplLib :: (Target l, FNode f)
             => LdHistory (Comp l)  -- ^ existing library of components
             -> FilePath            -- ^ file being loaded, for history bookkeeping
             -> f                   -- ^ @\<root\>@ node
             -> LdHistory (Comp l)  -- ^ updated library of components
loadTemplLib compLibH fPath = foldr ldRepl compLibH . children "template"
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
--  > <root>/template[@name=*,...]
-- 
-- Replaces existing entries if their IDs match. Made to be used with the CoInSyDe
-- library load scheme, see "CoInSyDe.LibManage". Uses 'mkRequirements'.
loadNativeLib :: (Target l, FNode f)
              => LdHistory (Comp l)  -- ^ existing library of components
              -> FilePath            -- ^ file being loaded, for history bookkeeping
              -> f                   -- ^ @\<root\>@ node
              -> LdHistory (Comp l)  -- ^ updated library of components
loadNativeLib compLibH fPath = foldr ldRepl compLibH . children "native"
  where
    ldRepl n (hist,lib)
      = let name    = n @! "name"
            reqmnts = mkRequirements n
            code    = case n @? "fromFile" of
                        Nothing -> Right $ txtContent n
                        Just a  -> Left $ unpack a
            newComp = NvComp name empty reqmnts code
            newHist = insertWith (++) name [fPath] hist
            newLib  | name `member` lib = lib
                    | otherwise         = insert name newComp lib
        in (newHist,newLib)
        
-- | Returns a list with the names for only the top-level components defined by the
-- main project file. This is needed to start the recursive library search for code
-- generation.
getCompProj :: FNode n => n -> [Id]
getCompProj = map (@! "name") . childrenOf ["composite","template","pattern"]
