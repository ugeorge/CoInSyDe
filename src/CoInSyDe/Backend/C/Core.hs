{-# LANGUAGE TypeFamilies, OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
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

import GHC.Generics (Generic)
import Control.DeepSeq (NFData, rnf)
import Data.Text (Text,pack,append,snoc)
import Data.Text.Read
import Data.Map.Lazy as M hiding (map,filter,take)

-- | Defines the family of C target languages. In particular it defines a set of types
-- (see 'Type C'), a set of interfaces (see 'If C') and a set of requirements (see
-- 'Requ C'), by instantiating the 'Target' type class.
data C = C

data Value = NoVal
           | Val Text   -- ^ initialization value in textual format
           | Cons Name  -- ^ instance key in the component's 'InstMap'
           deriving (Show, Read, Generic, NFData)


-----------------------------------------------------------------
instance Target C where
  data Type C = PrimTy  {tyName :: Id} 
              | EnumTy  {tyName :: Id, enumVals  :: [(Text, Maybe Text)]}
              | Struct  {tyName :: Id, sEntries  :: Map Text (Type C)}
              | Array   {tyName :: Id, arrBaseTy :: Type C, arrSize :: Int}
              | Foreign {tyName :: Id, tyRequ    :: [Requ C]}
              | NoTy
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
            | Get     {ifName :: Id, ifTy :: Type C, ifVal :: Value, ifGlue :: Name} 
            | Put     {ifName :: Id, ifTy :: Type C, ifVal :: Value, ifGlue :: Name} 
            deriving (Read, Show, Generic, NFData)
  mkIf pId typeLib node =
    case (getName node, node @! "class") of
      ("port","iArg")    -> mkGeneric InArg  typeLib node
      ("port","oArg")    -> mkGeneric RetArg typeLib node
      ("intern","var")   -> mkGeneric LocVar typeLib node
      ("port","get")     -> mkGlued Get typeLib node
      ("port","put")     -> mkGlued Put typeLib node
      ("intern","state") -> mkState typeLib pId node
      ("intern","macro") -> mkParam node
      x -> error $ "Glue of type " ++ show x ++ " is not recognized!"

  data Requ C = Include Text deriving (Read, Show, Eq, Generic, NFData)
  mkRequ node = Include (node @! "include")

  mkComposite _ node =
    map (TCode . pack . show) $ take (length $ node |= "instance") [1::Int ..]
-----------------------------------------------------------------

------ TYPE CONSTRUCTORS ------

-- | Makes a 'PrimTy' from a node
--
-- > type[@name=*,@class="primitive",@targetName=*]
mkPrimTy = PrimTy

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
mkStruct tyLib tName pNodes = Struct tName (M.fromList $ map extract pNodes)
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
         
getParam name nodes = head (filterByAttr "name" name nodes) @! "value"

------ INTERFACE CONSTRUCTORS ------

-- | Can make an 'InArg', 'RetArg', 'LocVar' respectively from
--
-- > port[@class="iArg"|"oArg",@name=*,@type=*,@?value=*,@?constructor=*]
--
-- or
--
-- > intern[@class="var",@name=*,@type=*,@?value=*,@?constructor=*]
mkGeneric cons tyLib node = cons name ty val
  where name = node @! "name"
        ty   = tyLib !* (node @! "type")
        val  = case (node @? "value", node @? "constructor") of
                 (Nothing,Nothing) -> NoVal
                 (Just a, Nothing) -> Val a
                 (_, Just a)       -> Cons a

-- | Can make a 'Get', 'Put' respectively from
--
-- > port[@class="get"|"put",@name=*,@type=*,@mechanism=*,@?value=*,@?constructor=*]
mkGlued cons tyLib node = cons name ty val glue
  where name = node @! "name"
        ty   = tyLib !* (node @! "type")
        val  = case (node @? "value", node @? "constructor") of
                 (Nothing,Nothing) -> NoVal
                 (Just a, Nothing) -> Val a
                 (_, Just a)       -> Cons a
        glue = node @! "mechanism"

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

isInput InArg{}     = True
isInput _           = False
isOutput RetArg{}   = True
isOutput _          = False
isState GlobVar{}   = True
isState _           = False
isVariable LocVar{} = True
isVariable _        = False
isMacro Macro{}     = True
isMacro _           = False


getTypeOf Macro{} = Nothing
getTypeOf interf  = Just $ ifTy interf

