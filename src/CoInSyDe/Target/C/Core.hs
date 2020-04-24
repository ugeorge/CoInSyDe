{-# LANGUAGE TypeFamilies, OverloadedStrings, FlexibleInstances, DeriveGeneric #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Target.C.Core
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
module CoInSyDe.Target.C.Core (
  C(..), Type(..), Port(..), Requ(..), Kind(..),
  -- * 'Type' constructors
  -- mkPrimTy, mkEnumTy, mkStruct, mkArray, mkForeign,
  -- -- * 'If' (interface) constructors
  -- mkGeneric, mkState, mkParam,
  -- -- * Utilities
  -- isPrimitive,isForeign,isVoid,isArray,isMacro,isGlobal,getTypeOf
  ) where

import GHC.Generics (Generic)
import Data.Text (Text, append, pack)
import Data.Binary (Binary)
import Text.Pandoc.Builder hiding (Target)

import CoInSyDe.Core
import CoInSyDe.Internal.Docs
import CoInSyDe.Internal.Map
import CoInSyDe.Internal.YAML

-- | Defines the family of C target languages. In particular it defines a set of types
-- (see 'Type C'), a set of interfaces (see 'If C') and a set of requirements (see
-- 'Requ C'), by instantiating the 'Target' type class.
data C = C

data Kind = LocVar | GlobVar | InArg | OutArg | RetArg
          deriving (Show, Eq, Ord, Generic)

-----------------------------------------------------------------
instance Target C where
  data Type C
    =  PrimTy {tyId :: Id, tyName :: Text} 
    |  EnumTy {tyId :: Id, tyName :: Text, enumVals  :: [(Id, Maybe Text)]}
    | StrucTy {tyId :: Id, tyName :: Text, sEntries  :: [(Id, Type C)]}
    |   ArrTy {tyId :: Id, tyName :: Text, arrBaseTy :: Type C, arrSize :: Int}
    | Foreign {tyId :: Id, tyName :: Text, oCallPrefix :: Text, oBindPrefix :: Text,
               tyRequ :: [Requ C]}
    |    NoTy {tyName :: Text} -- ^ will always be void
    deriving (Show, Generic)
  mkType tyLib node = do
    idx  <- node @! "name"
    kind <- node @! "class" :: YParse Text
    name <- node @! "targetName"
    case kind of 
      "primitive" -> case name of
                       "void" -> return $ NoTy "void"
                       n      -> return $ PrimTy idx n
                       
      "enum"   -> do vals <- node |= "parameter" >>= mapM 
                       (\p -> do nm <- p @! "name"
                                 vl <- p @? "value"
                                 return (nm, vl))
                     return $ EnumTy idx name vals
                     
      "struct" -> do entries <- node |= "parameter" >>= mapM
                       (\p -> do nm <- p @! "name"
                                 ty <- (tyLib !*) <$> (p @! "type") >>=
                                       maybe (yamlError p "Type not loaded!") return 
                                 return (nm, ty))
                     return $ StrucTy idx name entries
                     
      "array"  -> do size <- node @! "size"
                     bty  <- (tyLib !*) <$> (node @! "type") >>=
                             maybe (yamlError node "Type not loaded!") return
                     return $ ArrTy idx (tyName bty) bty size
                     
      "foreign"-> do cprefix <- node @! "callPrefix"
                     bprefix <- node @! "bindPrefix"
                     requs   <- node |= "requirement" >>= mapM mkRequ
                     return $ Foreign idx name cprefix bprefix requs
                     
      x -> yamlError node $ "Type class " ++ show x ++ " is not recognized!"
          
  data Port C = Var {pName :: Id, pKind :: Kind, pTy :: Type C, pVal :: Maybe Text}
              deriving (Show, Generic)
  mkPort tyLib node = do
    name <- node @! "name"
    kind <- node @! "kind" :: YParse Text
    ty   <- (tyLib !*) <$> (node @! "type") >>=
            maybe (yamlError node "Type not loaded!") return
    val  <- node @? "value"
    case kind of
      "iarg"  -> return $ Var name InArg ty val
      "oarg"  -> return $ Var name OutArg ty val
      "ret"   -> return $ Var name RetArg ty val
      "var"   -> return $ Var name LocVar ty val
      "state" -> return $ Var name GlobVar ty val
      x -> yamlError node "Port kind not recognized!"

  data Requ C = Include Text deriving (Show, Eq, Generic)
  mkRequ node = Include <$> (node @! "include")

-----------------------------------------------------------------

instance Binary Kind
instance Binary (Type C)
instance Binary (Port C)
instance Binary (Requ C)

instance ToDoc (Type C) where
  toDoc _ (PrimTy i n) = plain $ code n <> text ": primitive"
  toDoc _ (EnumTy i n m) = definitionList [
    (code n <> text ": enum", [catMap showElems m]) ]
    where showElems (p,v) = plain $ text p <> text (maybe "" (append "=") v)
  toDoc _ (StrucTy i n m) = definitionList [
    (code n <> text ": struct", [catMap showEntries m]) ]
    where showEntries (p,t) = plain $ text p <> text ":" <> tylink t
  toDoc _ (ArrTy i n b s) = plain $ code n <> text ": array"
    <> iangles (tylink b) <> ibracks (text $ pack $ show s)
  toDoc _ (Foreign i n c b r) = definitionList [
    (code n <> text ": foreign", map (toDoc "") r)]

instance ToDoc (Port C) where
  toDoc _ (Var name kind ty val) = (plain $ text ":" <> tylink ty)
    <> (plain $ text (pack $ show kind) <> code (maybe "" (append "=") val))

instance ToDoc (Requ C) where
  toDoc _ (Include r) = plain $ text "include: " <> code r

tylink NoTy{} = ilink "#" "void"
tylink ty     = ilink "ty" (tyId ty)

-- isPrimitive PrimTy{}  = True
-- isPrimitive _         = False
-- isForeign Foreign{}   = True
-- isForeign _           = False
-- isVoid NoTy{}         = True
-- isVoid _              = False
-- isArray ArrTy{}       = True
-- isArray _             = False
 
-- -- isMacro Macro{} = True
-- -- isMacro _       = False
-- isGlobal v@Variable{} = ifKind v == GlobVar
-- isGlobal _ = False

-- getTypeOf Macro{} = Nothing
-- getTypeOf interf  = Just $ ifTy interf


