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
  C(..), Type(..), If(..), Requ(..), CVal(..), Kind(..),
  -- * 'Type' constructors
  mkPrimTy, mkEnumTy, mkStruct, mkArray, mkForeign,
  -- * 'If' (interface) constructors
  mkGeneric, mkState, mkParam,
  -- * Utilities
  isPrimitive,isForeign,isVoid,isArray,isMacro,isGlobal,getTypeOf
  ) where

import CoInSyDe.Core
import CoInSyDe.Core.Dict
import CoInSyDe.Frontend

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Text (Text,append,snoc)
import Data.Text.Read

import qualified Data.Aeson as JSON

-- | Defines the family of C target languages. In particular it defines a set of types
-- (see 'Type C'), a set of interfaces (see 'If C') and a set of requirements (see
-- 'Requ C'), by instantiating the 'Target' type class.
data C = C

-- | Captures the different kinds of syntax used for initializing variables. 
data CVal = NoVal      -- ^ no initialization
          | Val Text   -- ^ initialization value in textual format
          | Cons Name  -- ^ points to constructor referenced in the parent
                        -- component's 'InstMap'
           deriving (Show, Read, Generic, NFData)

data Kind = LocVar | GlobVar | InArg | OutArg | RetArg | Get | Put
          deriving (Show,Read,Eq,Ord,Generic,NFData)

-----------------------------------------------------------------
instance Target C where
  data Type C = PrimTy  {tyName :: Id} 
              | EnumTy  {tyName :: Id, enumVals  :: [(Text, Maybe Text)]}
              | Struct  {tyName :: Id, sEntries  :: Map Text (Type C)}
              | ArrTy   {arrBaseTy :: Type C, arrSize :: Int}
              | Foreign {tyName :: Id, oCallPrefix :: Text, oBindPrefix :: Text,
                         tyRequ :: [Requ C]}
              | NoTy    {tyName :: Id} -- ^ will always be void
              deriving (Read, Show, Eq, Generic, NFData)
  mkType _ typeLib node =
    case node @! "class" of
      "primitive" -> mkPrimTy targetName
      "enum"      -> mkEnumTy targetName parameters
      "struct"    -> mkStruct typeLib targetName parameters
      "array"     -> mkArray  typeLib parameters
      "foreign"   -> mkForeign targetName requirements
      x -> error $ "Type class " ++ show x ++ " is not recognized!"
    where targetName   = node @! "targetName"
          parameters   = node |= "parameter"
          requirements = node |= "requirement"
          
  data If C = Macro    {ifName :: Id, macroVal :: JSON.Value}
            | Variable {ifName :: Id, ifKind :: Kind, ifTy :: Type C, ifVal :: CVal}
            deriving (Read, Show, Generic, NFData)
  mkIf pId typeLib node =
    case node @! "class" of
      "iarg"  -> mkGeneric InArg  typeLib node
      "oarg"  -> mkGeneric OutArg typeLib node
      "ret"   -> mkGeneric RetArg typeLib node
      "iport" -> mkGeneric Get typeLib node
      "oport" -> mkGeneric Put typeLib node
      "var"   -> mkGeneric LocVar typeLib node
      "state" -> mkState typeLib pId node
      "param" -> mkParam node
      x -> error $ "Glue of type " ++ show x ++ " is not recognized!"
  mkMacro = Macro "__intern__" . JSON.String

  data Requ C = Include Text deriving (Read, Show, Eq, Generic, NFData)
  mkRequ node = Include (node @! "include")

-----------------------------------------------------------------

instance JSON.ToJSON CVal      where toEncoding = JSON.genericToEncoding JSON.defaultOptions
instance JSON.ToJSON (Requ C)  where toEncoding = JSON.genericToEncoding JSON.defaultOptions
instance JSON.ToJSON (Type C)  where toEncoding = JSON.genericToEncoding JSON.defaultOptions

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

-- | Makes an 'ArrTy' from a node
--
-- > type[@name=*,@class="array"]
-- > - parameter[@name="baseType",@value=*]
-- > - parameter[@name="size",@value=*]
mkArray tyLib pNodes = ArrTy baseTy size
  where baseTy  = tyLib !* getParam "baseType" pNodes
        size    = fst $ either error id $ decimal $ getParam "size" pNodes

-- | Makes a 'Foreign' type from a node
--
-- > type[@name=*,@class="foreign",@targetName=*]
-- > + requirement[@include=*]
mkForeign tName pNodes = Foreign tName cPrefix bPrefix $ map mkRequ pNodes
  where cPrefix = getParam' "callPrefix" pNodes
        bPrefix = getParam' "bindPrefix" pNodes

    
-- -- | Makes a 'PtrTy' type from a node
-- --
-- -- > type[@name=*,@class="pointer",@targetName=*]
-- -- > + parameter[@baseType=*]
-- mkPtrTy tyLib pNodes = PtrTy tName baseTy
--   where baseTy  = tyLib !* getParam "baseType" pNodes
--         tName   = tyName baseTy
                
getParam name nodes = head (filterByAttr "name" name nodes) @! "value"
getParam' name nodes = case filterByAttr "name" name nodes of
  []  -> ""
  [x] -> x @! "value"


------ INTERFACE CONSTRUCTORS ------

-- | Can make an 'InArg', 'RetArg', 'Get', 'Put' or 'LocVar' respectively from
--
-- > interface[@class=<class>,@name=*,@type=*,@?value=*,@?constructor=*]
--
-- where @<class>@ can be @"iarg"|"oarg"|"ret"|"iport"|"oport"|"var"@
mkGeneric kind tyLib node = Variable name kind ty val
  where name = node @! "name"
        ty   = tyLib !* (node @! "type")
        val  = case (node @? "value", node @? "constructor") of
                 (Nothing,Nothing) -> NoVal
                 (Just a, Nothing) -> Val a
                 (_, Just a)       -> Cons a

-- | Makes a 'GlobVar' from a node
--
-- > intern[@class="state",@name=*,@type=*,@value=*]
mkState tyLib parentId node = Variable name GlobVar ty val
  where name = (parentId `snoc` '_') `append` (node @! "name")
        ty   = tyLib !* (node @! "type")
        val  = maybe NoVal Val $ node @? "value"

-- | Makes a 'Macro' from a node
--
-- > intern[@class="param",@name=*,@value=*]
mkParam node = Macro name val
  where name = node @! "name"
        val  = node @: "value"


isPrimitive PrimTy{}  = True
isPrimitive _         = False
isForeign Foreign{}   = True
isForeign _           = False
isVoid NoTy{}         = True
isVoid _              = False
isArray ArrTy{}       = True
isArray _             = False

isMacro Macro{} = True
isMacro _       = False
isGlobal v@Variable{} = ifKind v == GlobVar
isGlobal _ = False

getTypeOf Macro{} = Nothing
getTypeOf interf  = Just $ ifTy interf


