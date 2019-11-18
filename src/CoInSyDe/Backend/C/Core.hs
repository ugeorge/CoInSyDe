{-# LANGUAGE TypeFamilies, OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Backend.C.Core
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
module CoInSyDe.Backend.C.Core (
  C(..), Type(..), If(..), Requ(..), Value(..),
  -- * 'Type' constructors
  mkPrimTy, mkEnumTy, mkStruct, mkArray, mkForeign,
  -- * 'If' (interface) constructors
  mkGeneric, mkState, mkParam,
  -- * Utilities
  isPrimitive,isForeign,isVoid,isArray,
  isInput,isOutput,isState,isVar,isMacro,isGet,isPut,
  getTypeOf,getOutput
  ) where

import CoInSyDe.Core
import CoInSyDe.Core.Dict
import CoInSyDe.Frontend

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Text (Text,append,snoc)
import Data.Text.Read

import Data.Aeson hiding (Array,Value)

-- | Defines the family of C target languages. In particular it defines a set of types
-- (see 'Type C'), a set of interfaces (see 'If C') and a set of requirements (see
-- 'Requ C'), by instantiating the 'Target' type class.
data C = C

-- | Captures the different kinds of syntax used for initializing variables. 
data Value = NoVal      -- ^ no initialization
           | Val Text   -- ^ initialization value in textual format
           | Cons Name  -- ^ points to constructor referenced in the parent
                        -- component's 'InstMap'
           deriving (Show, Read, Generic, NFData)

-----------------------------------------------------------------
instance Target C where
  data Type C = PrimTy  {tyName :: Id} 
              | EnumTy  {tyName :: Id, enumVals  :: [(Text, Maybe Text)]}
              | Struct  {tyName :: Id, sEntries  :: Map Text (Type C)}
              | Array   {tyName :: Id, arrBaseTy :: Type C, arrSize :: Int}
              | Foreign {tyName :: Id, tyRequ    :: [Requ C]}
              | NoTy    {tyName :: Id} -- ^ will always be void
              deriving (Read, Show, Eq, Generic, NFData)
  mkType _ typeLib node =
    case node @! "class" of
      "primitive" -> mkPrimTy targetName
      "enum"      -> mkEnumTy targetName parameters
      "struct"    -> mkStruct typeLib targetName parameters
      "array"     -> mkArray  typeLib targetName parameters
      "foreign"   -> mkForeign targetName requirements
      x -> error $ "Type class " ++ show x ++ " is not recognized!"
    where targetName   = node @! "targetName"
          parameters   = node |= "parameter"
          requirements = node |= "requirement"
          
  data If C = Macro   {ifName :: Id, macroVal :: Text}
            | LocVar  {ifName :: Id, ifTy :: Type C, ifVal :: Value}
            | GlobVar {ifName :: Id, ifTy :: Type C, ifVal :: Value} 
            | InArg   {ifName :: Id, ifTy :: Type C, ifVal :: Value}
            | RetArg  {ifName :: Id, ifTy :: Type C, ifVal :: Value} 
            | Get     {ifName :: Id, ifTy :: Type C, ifVal :: Value} 
            | Put     {ifName :: Id, ifTy :: Type C, ifVal :: Value} 
            deriving (Read, Show, Generic, NFData)
  mkIf pId typeLib node =
    case node @! "class" of
      "iarg"  -> mkGeneric InArg  typeLib node
      "oarg"  -> mkGeneric RetArg typeLib node
      "iport" -> mkGeneric Get typeLib node
      "oport" -> mkGeneric Put typeLib node
      "var"   -> mkGeneric LocVar typeLib node
      "state" -> mkState typeLib pId node
      "macro" -> mkParam node
      x -> error $ "Glue of type " ++ show x ++ " is not recognized!"
  mkMacro = Macro "__intern__"

  data Requ C = Include Text deriving (Read, Show, Eq, Generic, NFData)
  mkRequ node = Include (node @! "include")

instance ToJSON Value     where toEncoding = genericToEncoding defaultOptions
instance ToJSON (Requ C)  where toEncoding = genericToEncoding defaultOptions
instance ToJSON (Type C)  where toEncoding = genericToEncoding defaultOptions
instance ToJSON (If C)    where toEncoding = genericToEncoding defaultOptions

-----------------------------------------------------------------


------ TYPE CONSTRUCTORS ------

-- | Makes a 'PrimTy' or 'NoTy' (void) from a node
--
-- > type[@name=*,@class="primitive",@targetName=*]
mkPrimTy "void" = NoTy "void"
mkPrimTy tName  = PrimTy tName

-- | Makes a 'EnumTy' from a node
--
-- > type[@name=*,@class="enum",@targetName=*]
-- > + parameter[@name=*,@?value=*]
mkEnumTy tName pNodes = EnumTy tName (map extract pNodes)
  where extract n = (n @! "name", n @? "value")

-- | Makes a 'Struct' from a node
--
-- > type[@name=*,@class="struct",@targetName=*]
-- > + parameter[@name=*,@type=*]
mkStruct tyLib tName pNodes = Struct tName (mkMap $ map extract pNodes)
  where extract n = (n @! "name", tyLib !* (n @! "type"))

-- | Makes an 'Array' from a node
--
-- > type[@name=*,@class="array",@targetName=*]
-- > - parameter[@name="baseType",@value=*]
-- > - parameter[@name="size",@value=*]
mkArray tyLib tName pNodes = Array tName baseTy size
  where baseTy  = tyLib !* getParam "baseType" pNodes
        size    = fst $ either error id $ decimal $ getParam "size" pNodes

-- | Makes a 'Foreign' type from a node
--
-- > type[@name=*,@class="foreign",@targetName=*]
-- > + requirement[@include=*]
mkForeign tName = Foreign tName . map mkRequ

-- -- | Makes a 'PtrTy' type from a node
-- --
-- -- > type[@name=*,@class="pointer",@targetName=*]
-- -- > + parameter[@baseType=*]
-- mkPtrTy tyLib pNodes = PtrTy tName baseTy
--   where baseTy  = tyLib !* getParam "baseType" pNodes
--         tName   = tyName baseTy
                
getParam name nodes = head (filterByAttr "name" name nodes) @! "value"

------ INTERFACE CONSTRUCTORS ------

-- | Can make an 'InArg', 'RetArg', 'Get', 'Put' or 'LocVar' respectively from
--
-- > interface[@class="iarg"|"oarg"|"iport"|"oport"|"var",@name=*,@type=*,@?value=*,@?constructor=*]
mkGeneric cons tyLib node = cons name ty val
  where name = node @! "name"
        ty   = tyLib !* (node @! "type")
        val  = case (node @? "value", node @? "constructor") of
                 (Nothing,Nothing) -> NoVal
                 (Just a, Nothing) -> Val a
                 (_, Just a)       -> Cons a

-- | Makes a 'GlobVar' from a node
--
-- > intern[@class="state",@name=*,@type=*,@value=*]
mkState tyLib parentId node = GlobVar name ty val
  where name = (parentId `snoc` '_') `append` (node @! "name")
        ty   = tyLib !* (node @! "type")
        val  = maybe NoVal Val $ node @? "value"

-- | Makes a 'Macro' from a node
--
-- > intern[@class="macro",@name=*,@value=*]
mkParam node = Macro name val
  where name = node @! "name"
        val  = node @! "value"


isPrimitive PrimTy{}  = True
isPrimitive _         = False
isForeign Foreign{}   = True
isForeign _           = False
isVoid NoTy{}         = True
isVoid _              = False
isArray Array{}       = True
isArray _             = False

isInput InArg{}   = True
isInput _         = False
isOutput RetArg{} = True
isOutput _        = False
isState GlobVar{} = True
isState _         = False
isVar LocVar{}    = True
isVar _           = False
isMacro Macro{}   = True
isMacro _         = False
isGet Get{}       = True
isGet _           = False
isPut Put{}       = True
isPut _           = False


getTypeOf Macro{} = Nothing
getTypeOf interf  = Just $ ifTy interf

-- | Assures that a component has at most one 'RetArg'. If none is found, it creates a
-- @void@ 'RetArg', used in function definition headers.
getOutput n ps = case filter isOutput ps of
                   []  -> RetArg {ifName = "__OUT_",
                                  ifTy   = NoTy "void",
                                  ifVal  = NoVal} 
                   [a] -> a
                   xs  -> error $ "Gen: Function " ++ show n ++ " has more than " ++
                          "one return argument:\n" ++ show xs
