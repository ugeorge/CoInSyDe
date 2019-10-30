{-# LANGUAGE TypeFamilies, OverloadedStrings, FlexibleInstances #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Core.C.Core
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains the core types specific to the family of C-based tagets, and
-- their constructors.
----------------------------------------------------------------------
module CoInSyDe.Backend.C.Core where

import CoInSyDe.Core
import CoInSyDe.TTm
import CoInSyDe.Dictionary
import CoInSyDe.Frontend

import Control.DeepSeq (NFData, rnf)
import Data.Text (Text,pack,append,snoc)
import Data.Text.Read
import Data.Map.Lazy as M hiding (map,filter,take)

-- | Defines the family of C target languages. In particular it defines a set of types
-- (see 'Type C'), a set of interfaces (see 'If C') and a set of requirements (see
-- 'Requ C'), by instantiating the 'Target' type class.
data C = C

instance Target C where
  data Type C = PrimTy  {tyName :: Id} -- ^ primitive type
              | BoolTy  {tyName :: Id, boolTrue :: Text, boolFalse :: Text}
              | EnumTy  {tyName :: Id, enumVals :: [(Text, Maybe Text)]}
              | Struct  {tyName :: Id, sEntries :: Map Text (Type C, Maybe Text)}
              | Array   {tyName :: Id, arrBaseTy :: Type C, arrSize :: Int}
              -- | CircBuf {tyName :: Id, bufArr :: Type C,  bufPos :: Int}
              | NoTy
              deriving (Read, Show, Eq)                       
  mkType _ typeLib node =
    case node @! "class" of
      "primitive" -> mkPrimTy targetName
      "bool"   -> mkBoolTy targetName parameters
      "enum"   -> mkEnumTy targetName parameters
      "struct" -> mkStruct typeLib targetName parameters
      "array"  -> mkArray  typeLib targetName parameters
      -- "circular_buffer" -> mkCircBuf targetName parameters
      x -> error $ "Type class " ++ show x ++ " is not recognized!"
    where targetName = node @! "targetName"
          parameters = node |= "parameter"
          
  data If C = Param   {glName :: Text, paramVal :: Text}
            | LocVar  {glName :: Text, glTy :: Type C, glVal    :: Maybe Text}
            | GlobVar {glName :: Text, glTy :: Type C, stateVal :: Text} 
            | InArg   {glName :: Text, glTy :: Type C, glVal    :: Maybe Text}
            | RetArg  {glName :: Text, glTy :: Type C} 
            | Get     {glName :: Text, glTy :: Type C, glVal    :: Maybe Text} 
            | Put     {glName :: Text, glTy :: Type C, glVal    :: Maybe Text} 
            deriving (Read, Show)
  mkIf pId typeLib node =
    case (getName node, node @! "class") of
      ("port","iArg")        -> mkInArg typeLib node
      ("port","oArg")        -> mkRetArg typeLib node
      ("port","get")         -> mkGet typeLib node
      ("port","put")         -> mkPut typeLib node
      ("intern","var")       -> mkVar typeLib node
      ("intern","state")     -> mkState typeLib pId node
      ("intern","parameter") -> mkParam node
      x -> error $ "Glue of type " ++ show x ++ " is not recognized!"

  data Requ C = Include Text deriving (Read, Show, Eq)
  mkRequ node = Include (node @! "include")

  mkComposite _ node =
    map (TCode . pack . show) $ take (length $ node |= "instance") [1::Int ..]

instance NFData (Type C) where
  rnf (PrimTy n) = rnf n
  rnf (BoolTy n t f) = rnf n `seq` rnf t `seq` rnf f
  rnf (EnumTy n v) = rnf n `seq` rnf v
  rnf (Struct n _) = rnf n 
  rnf (Array n _ s) = rnf n `seq` rnf s
  rnf _ = ()

instance NFData (If C) where
  rnf (Param n v)    = rnf n `seq` rnf v
  rnf (LocVar n t v) = rnf n `seq` rnf t `seq` rnf v
  rnf (GlobVar n t v) = rnf n `seq` rnf t `seq` rnf v
  rnf (InArg n t v) = rnf n `seq` rnf t `seq` rnf v
  rnf (RetArg n t) = rnf n `seq` rnf t 
  rnf (Get n t v) = rnf n `seq` rnf t `seq` rnf v
  rnf (Put n t v) = rnf n `seq` rnf t `seq` rnf v
  -- rnf _ = ()

instance NFData (Requ C) where
  rnf (Include x) = rnf x

------ TYPE CONSTRUCTORS ------

-- | Makes a 'PrimTy' from a node
--
-- > type[@name=*,@class="primitive",@targetName=*]
mkPrimTy = PrimTy

-- | Makes a 'BoolTy' from a node
--
-- > type[@name=*,@class="bool",@targetName=*]
-- > - parameter[@name="true",@value=*]
-- > - parameter[@name="false",@value=*]
mkBoolTy tName pNodes = BoolTy tName true false
  where true  = getParam "true" pNodes
        false = getParam "false" pNodes

-- | Makes a 'EnumTy' from a node
--
-- > type[@name=*,@class="enum",@targetName=*]
-- > + parameter[@name=*,@?value=*]
mkEnumTy tName pNodes = EnumTy tName (map extract pNodes)
  where extract n = (n @! "name", n @? "value")

-- | Makes a 'Struct' from a node
--
-- > type[@name=*,@class="struct",@targetName=*]
-- > + parameter[@name=*,@type=*,@?value=*]
mkStruct tyLib tName pNodes = Struct tName (M.fromList $ map extract pNodes)
  where extract n = (n @! "name", (tyLib !* (n @! "type"), n @? "value"))

-- | Makes an 'Array' from a node
--
-- > type[@name=*,@class="array",@targetName=*]
-- > - parameter[@name="baseType",@value=*]
-- > - parameter[@name="size",@value=*]
mkArray tyLib tName pNodes = Array tName baseTy size
  where baseTy  = tyLib !* getParam "baseType" pNodes
        size    = fst $ either error id $ decimal $ getParam "size" pNodes

getParam name nodes = head (filterByAttr "name" name nodes) @! "value"

------ INTERFACE CONSTRUCTORS ------

-- | Makes an 'InArg' from a node
--
-- > port[@class="iArg",@name=*,@type=*,@?value=*]
mkInArg tyLib node = InArg name ty val
  where name = node @! "name"
        ty   = tyLib !* (node @! "type")
        val  = node @? "value"
        
-- | Makes a 'RetArg' from a node
--
-- > port[@class="oArg",@name=*,@type=*]
mkRetArg tyLib node = RetArg name ty
  where name = node @! "name"
        ty   = tyLib !* (node @! "type")

-- | Makes a 'Get' from a node
--
-- > port[@class="get",@name=*,@type=*,@?value=*]
--
-- The glue mechanism associated with this port will be instantiated as any other
-- template instance, by using bindings defined in a sibling @instance@ node.
mkGet tyLib node = Get name ty val
  where name = node @! "name"
        ty   = tyLib !* (node @! "type")
        val  = node @? "value"

-- | Makes a 'Get' from a node
--
-- > port[@class="put",@name=*,@type=*,@?value=*]
--
-- The glue mechanism associated with this port will be instantiated as any other
-- template instance, by using bindings defined in a sibling @instance@ node.
mkPut tyLib node = Put name ty val
  where name = node @! "name"
        ty   = tyLib !* (node @! "type")
        val  = node @? "value"
               
-- | Makes a 'LocVar' from a node
--
-- > intern[@class="var",@name=*,@type=*,@?value=*]
mkVar tyLib node = LocVar name ty val
  where name = node @! "name"
        ty   = tyLib !* (node @! "type")
        val  = node @? "value"

-- | Makes a 'GlobVar' from a node
--
-- > intern[@class="state",@name=*,@type=*,@value=*]
mkState tyLib parentId node = GlobVar name ty val
  where name = (parentId `snoc` '_') `append` (node @! "name")
        ty   = tyLib !* (node @! "type")
        val  = node @! "value"

-- | Makes a 'Param' from a node
--
-- > intern[@class="state",@name=*,@type=*,@value=*]
mkParam node = Param name val
  where name = node @! "name"
        val  = node @! "value"

isInput InArg{}       = True
isInput _             = False
isOutput RetArg{}     = True
isOutput _            = False
isState GlobVar{}     = True
isState _             = False
isVariable LocVar{}   = True
isVariable _          = False

getTypeOf Param{} = Nothing
getTypeOf interf  = Just $ glTy interf

------ SPECIFIC DICTIONARIES ------


-- mkStateDict :: FNode n => Dict (Type C) -> n -> IfMap C
-- mkStateDict tyLib root = M.fromList states
--   where
--     pNodes = concatMap groupByName $ childrenOf ["pattern","composite"] root
--     states = map mkStateVar $ filter (hasValue "class" "state" . snd) pNodes
--     -----------------------------------
--     groupByName n = map (\p-> (n @! "name", p)) (n |= "port")
--     mkStateVar (parentId, n) = let stVar = mkState tyLib parentId n
--                                in (glName stVar, stVar)

-- mkRequList :: Dict (Comp C) -> [Requ C]
-- mkRequList = nub . concatMap (reqs . snd) . M.toList

-- getInitVal node = case node |= "parameter" of
--                     []  -> Nothing
--                     [n] -> getParamVal "initValue" n
--                     _   -> error $ "Node " ++ show (node @! "name")
--                            ++ "cannot have multiple initial values!"

