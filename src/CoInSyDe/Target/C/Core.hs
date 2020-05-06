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
module CoInSyDe.Target.C.Core where

import Data.Binary (Binary)
import Data.HashMap.Strict as M hiding (map,filter,foldr, null)
import Data.Text (Text, append, pack, unpack, breakOn, tail)
import Data.Text.Read (decimal)
import Data.Maybe (fromMaybe)
import Data.YAML
import GHC.Generics (Generic)
import Text.Pandoc.Builder hiding (Target)

import CoInSyDe.Core
import CoInSyDe.Internal.Docs
import CoInSyDe.Internal.Map
import CoInSyDe.Internal.YAML

-- | Defines the family of C target languages. In particular it defines a set of types
-- (see 'Type C'), a set of interfaces (see 'If C') and a set of requirements (see
-- 'Requ C'), by instantiating the 'Target' type class.
data C = C

data Kind = LocVar | GlobVar | InArg Int | OutArg Int | RetArg
          deriving (Eq, Ord, Generic)
instance Show Kind where
  show LocVar = "local variable"
  show GlobVar = "global variable"
  show RetArg = "return argument"
  show (InArg _) = "input argument"
  show (OutArg _) = "output argument"
  
-----------------------------------------------------------------
instance Target C where
  data Type C
    =  PrimTy {tyId :: Id, tyName :: Text} 
    |  EnumTy {tyId :: Id, tyName :: Text, enumVals  :: [(Id, Maybe Text)]}
    | StrucTy {tyId :: Id, tyName :: Text, sEntries  :: [(Id, Type C)]}
    |   ArrTy {tyId :: Id, tyName :: Text, arrBaseTy :: Type C,
               arrSize :: Either Id Int}
    | Foreign {tyId :: Id, tyName :: Text, inUsage :: Text, outUsage :: Text,
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
                     
      "array"  -> do sNum <- node @? "size"
                     sVar <- node @? "sizeFrom"
                     size <- case (sNum,sVar) of
                               (Just s,Nothing) -> return $ Right s
                               (Nothing,Just i) -> return $ Left i
                               _ -> yamlError node "Cannot deduce array size!"
                     bty  <- (tyLib !*) <$> (node @! "baseType") >>= maybe
                             (yamlError node "Array base type not loaded!") return
                     return $ ArrTy idx (tyName bty) bty size
                     
      "foreign"-> do ouse  <- node @? "outUsage" @= ""
                     iuse  <- node @? "inUsage"  @= ""
                     requs <- node |= "requirement" >>= mapM mkRequ
                     return $ Foreign idx name iuse ouse requs
                     
      x -> yamlError node $ "Type class " ++ show x ++ " is not recognized!"
          
  data Port C = Var { pName :: Id, pKind :: Kind, pTy :: Type C, pVal :: Maybe Text }
              deriving (Show, Generic)
  mkPort tyLib node = do
    name <- node @! "name"
    kind <- breakOn "." <$> node @! "kind"
    ty   <- (tyLib !*) <$> (node @! "type") >>=
            maybe (yamlError node "Type not loaded!") return
    val  <- node @? "value"
    case (fst kind, decimal $ Data.Text.tail $ snd kind) of
      ("iarg",Left err)   -> yamlError node $ "Position for input argument: " ++ err
      ("oarg",Left err)   -> yamlError node $ "Position for output argument: " ++ err
      ("iarg",Right(p,_)) -> return $ Var name (InArg p) ty val
      ("oarg",Right(p,_)) -> return $ Var name (OutArg p) ty val
      ("ret",_)   -> return $ Var name RetArg ty val
      ("var",_)   -> return $ Var name LocVar ty val
      ("state",_) -> maybe (yamlError node "State needs to have initial value")
                     (return . Var name GlobVar ty . Just) val
      x -> yamlError node "Port kind not recognized!"

  data Requ C = Include Text deriving (Show, Eq, Generic)
  mkRequ node = Include <$> (node @! "include")

-----------------------------------------------------------------

isPrim PrimTy{}     = True
isPrim _            = False
isEnum EnumTy{}     = True
isEnum _            = False
isStruct StrucTy{}  = True
isStruct _          = False
isArray ArrTy{}     = True
isArray _           = False
isForeign Foreign{} = True
isForeign _         = False
isVoid NoTy{}       = True
isVoid _            = False
 
isGlobal v = pKind v == GlobVar

instance Binary Kind
instance Binary (Type C)
instance Binary (Port C)
instance Binary (Requ C)

instance ToYAML (Type C) where
  toYAML (PrimTy i n)    = mapping [ "_name" .= n]
  toYAML (EnumTy i n m)  = mapping [ "_name" .= n, "_val" .= map fst m ]
  toYAML (StrucTy i n m) = mapping $
    [ "_name" .= n, "_constructor" .= ("_mk_" `append` n) ]
    ++ map (\(n,t) ->  n .= toYAML t) m
  toYAML (ArrTy i n b s) = mapping
    [ "_name" .= n, "_base" .= toYAML b,  "_size" .= either id (pack . show) s ]
  toYAML (Foreign i n iu ou r) = mapping [ "_name" .= n  ]

ifToYAML :: Id -> If C -> Node ()
ifToYAML i (TPort (Var n k t v)) = mapping $
  [ "_name" .= n, "_type" .= toYAML t ]  ++
  maybe [] (\x -> [ "_val" .= x ]) v
ifToYAML _ (Param n) = toYAML n
  
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
  toDoc _ (Var name kind ty val) = (plain $ code (pack $ show kind))
    <> (plain $ text name <> text ":" <> tylink ty <> code (maybe "" (append "=") val))

instance ToDoc (Requ C) where
  toDoc _ (Include r) = plain $ text "include: " <> code r

tylink NoTy{} = ilink "#" "void"
tylink ty     = ilink "ty" (tyId ty)

