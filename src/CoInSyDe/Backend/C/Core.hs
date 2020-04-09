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
import CoInSyDe.Internal.Dict
import CoInSyDe.Internal.YAML

import Control.Monad (liftM)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Text (Text,append,snoc)

import Data.YAML
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
          deriving (Show, Eq, Read, Generic, NFData)

data Kind = LocVar | GlobVar | InArg | OutArg | RetArg | Port
          deriving (Show,Read,Eq,Ord,Generic,NFData)

-----------------------------------------------------------------
instance Target C where
  data Type C = PrimTy  {tyName :: Id} 
              | EnumTy  {tyName :: Id, enumVals  :: [(Text, Maybe Text)]}
              | Struct  {tyName :: Id, sEntries  :: [(Text, Type C)]}
              | ArrTy   {tyName :: Id, arrBaseTy :: Type C, arrSize :: Int}
              | Foreign {tyName :: Id, oCallPrefix :: Text, oBindPrefix :: Text,
                         tyRequ :: [Requ C]}
              | NoTy    {tyName :: Id} -- ^ will always be void
              deriving (Read, Show, Eq, Generic, NFData)
  mkType _ typeLib node = do
    tClass       <- node @! "class" :: Parser Text
    targetName   <- node @! "targetName"
    parameters   <- node |= "parameter"
    requirements <- node |= "requirement"
    case tClass of
      "primitive" -> mkPrimTy targetName
      "enum"      -> mkEnumTy targetName parameters
      "struct"    -> mkStruct typeLib targetName parameters
      "array"     -> mkArray  typeLib parameters
      "foreign"   -> mkForeign targetName parameters requirements
      x -> failAtNode node $ "Type class " ++ show x ++ " is not recognized!"
    where 
          
  data If C = Macro    {ifName :: Id, macroVal :: YamlNode}
            | Variable {ifName :: Id, ifKind :: Kind, ifTy :: Type C, ifVal :: CVal}
            deriving (Read, Show, Generic, NFData)
  mkIf pId typeLib node = do
    iClass <- node @! "class" :: Parser Text
    case iClass of
      "iarg"  -> mkGeneric InArg  typeLib node
      "oarg"  -> mkGeneric OutArg typeLib node
      "ret"   -> mkGeneric RetArg typeLib node
      "port"  -> mkGeneric Port typeLib node
      "var"   -> mkGeneric LocVar typeLib node
      "state" -> mkState typeLib pId node
      "param" -> mkMacro node
      x -> error $ "Interface of type " ++ show x ++ " is not recognized!"
  mkParam = return . Macro "__internal__"

  data Requ C = Include Text deriving (Read, Show, Eq, Generic, NFData)
  mkRequ node = liftM Include (node @! "include")

-----------------------------------------------------------------
-- TODO: temporary. Will migrate everything to YAML.
-- JSON conversions used for Ginger maps

instance JSON.ToJSON CVal      where toEncoding = JSON.genericToEncoding JSON.defaultOptions
instance JSON.ToJSON (Requ C)  where toEncoding = JSON.genericToEncoding JSON.defaultOptions
instance JSON.ToJSON (Type C)  where toEncoding = JSON.genericToEncoding JSON.defaultOptions

-----------------------------------------------------------------

------ TYPE CONSTRUCTORS ------

-- | Makes a 'PrimTy' or 'NoTy' (void) from a node
--
-- > type[@name=*,@class="primitive",@targetName=*]
mkPrimTy :: Text -> Parser (Type C)
mkPrimTy "void" = return $ NoTy "void"
mkPrimTy tName  = return $ PrimTy tName

-- | Makes a 'EnumTy' from a node
--
-- > type[@name=*,@class="enum",@targetName=*]
-- > + parameter[@name=*,@?value=*]
mkEnumTy :: Text -> [YamlNode] -> Parser (Type C)
mkEnumTy tName pNodes = fmap (EnumTy tName) (mapM extract pNodes)
  where extract n = (,) <$> n @! "name" <*> n @? "value"

-- | Makes a 'Struct' from a node
--
-- > type[@name=*,@class="struct",@targetName=*]
-- > - parameter: [{@name=*,@type=*}]
mkStruct :: MapH (Type C) -> Text -> [YamlNode] -> Parser (Type C)
mkStruct tyLib tName pNodes = fmap (Struct tName) (mapM extract pNodes)
  where extract n = do
          name <- n @! "name"
          ty   <- n @! "type"
          return (name, tyLib !* ty)

-- | Makes an 'ArrTy' from a node
--
-- > type[@name=*,@class="array"]
-- > - parameter: {@baseType=*, @size=*}
mkArray :: MapH (Type C)  -> [YamlNode] -> Parser (Type C)
mkArray tyLib pNodes = do
  baseTy <- (head pNodes) @! "baseType"
  size   <- (head pNodes) @! "size"
  return $ ArrTy baseTy (tyLib !* baseTy) size

-- | Makes a 'Foreign' type from a node
--
-- > type[@name=*,@class="foreign",@targetName=*]
-- > - parameter {@?callPrefix=*, @?bindPrefix=*}
-- > + requirement[@include=*]
mkForeign :: Text -> [YamlNode] -> [YamlNode] -> Parser (Type C)
mkForeign tName params requs = do
  cPrefix <- if null params then return "" else (head params) @! "callPrefix"
  bPrefix <- if null params then return "" else (head params) @! "bindPrefix"
  requmnt <- mapM mkRequ requs
  return $ Foreign tName cPrefix bPrefix requmnt
    
-- -- | Makes a 'PtrTy' type from a node
-- --
-- -- > type[@name=*,@class="pointer",@targetName=*]
-- -- > + parameter[@baseType=*]
-- mkPtrTy tyLib pNodes = PtrTy tName baseTy
--   where baseTy  = tyLib !* getParam "baseType" pNodes
--         tName   = tyName baseTy

------ INTERFACE CONSTRUCTORS ------

-- | Can make an 'InArg', 'RetArg', 'Port' or 'LocVar' respectively from
--
-- > interface[@class=<class>,@name=*,@type=*,@?value=*,@?constructor=*]
--
-- where @<class>@ can be @"iarg"|"oarg"|"ret"|"port"|"var"@
mkGeneric :: Kind -> MapH (Type C) -> YamlNode -> Parser (If C)
mkGeneric kind tyLib node = do
  name <- node @! "name"
  ty   <- node @! "type"
  val  <- node @? "value"
  cstr <- node @? "constructor"
  let value = case (val, cstr) of
                (Nothing,Nothing) -> NoVal
                (Just a, Nothing) -> Val a
                (_, Just a)       -> Cons a
  return $ Variable name kind (tyLib !* ty) value

-- | Makes a 'GlobVar' from a node
--
-- > intern[@class="state",@name=*,@type=*,@value=*]
mkState :: MapH (Type C) -> Id -> YamlNode -> Parser (If C)
mkState tyLib parentId node = do
  name <- node @! "name"
  ty   <- node @! "type"
  val  <- node @? "value"
  cstr <- node @? "constructor"
  let value = case (val, cstr) of
                (Nothing,Nothing) -> NoVal
                (Just a, Nothing) -> Val a
                (_, Just a)       -> Cons a
  return $ Variable ((parentId `snoc` '_') `append` name) GlobVar (tyLib !* ty) value

-- | Makes a 'Macro' from a node
--
-- > intern[@class="param",@name=*,@value=*]
mkMacro :: YamlNode -> Parser (If C)
mkMacro node = Macro <$> node @! "name" <*> node @! "value"

--------------------------------------------------

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


