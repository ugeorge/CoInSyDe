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
import Data.List (sortOn)
import Data.Text (Text, append, pack, unpack, breakOn, tail)
import Data.Text.Read (decimal)
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
          deriving (Show, Eq, Ord, Generic)

-----------------------------------------------------------------
instance Target C where
  data Type C
    =  PrimTy {tyId :: Id, tyName :: Text} 
    |  EnumTy {tyId :: Id, tyName :: Text, enumVals  :: [(Id, Maybe Text)]}
    | StrucTy {tyId :: Id, tyName :: Text, sEntries  :: [(Id, Type C)]}
    |   ArrTy {tyId :: Id, tyName :: Text, arrBaseTy :: Type C,
               arrSize :: Either Int Id}
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
                     
      "array"  -> do sNum <- node @? "size"
                     sVar <- node @? "sizeFrom"
                     size <- case (sNum,sVar) of
                               (Just s,Nothing) -> return $ Left s
                               (Nothing,Just i) -> return $ Right i
                               _ -> yamlError node "Cannot deduce array size!"
                     bty  <- (tyLib !*) <$> (node @! "baseType") >>= maybe
                             (yamlError node "Array base type not loaded!") return
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
    kind <- (breakOn ".") <$> node @! "kind"
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
      ("state",_) -> return $ Var name GlobVar ty val
      x -> yamlError node "Port kind not recognized!"

  data Requ C = Include Text deriving (Show, Eq, Generic)
  mkRequ node = Include <$> (node @! "include")

-----------------------------------------------------------------

instance Binary Kind
instance Binary (Type C)
instance Binary (Port C)
instance Binary (Requ C)

tylink NoTy{} = ilink "#" "void"
tylink ty     = ilink "ty" (tyId ty)

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

-- Interface separator. Does a lot of plumbing when it comes to maniputaling the
-- interfaces.
data IfSeparator = Sep {
  iarg  :: [Port C], oarg  :: [Port C], args ::[Port C], ret ::  Port C,
  var :: [Port C], state :: [Port C], param :: [YNode]
  } deriving (Show)
categorize ifmap = case ret' of
  []    -> Right $ Sep iarg oarg args voidret var state param
  [ret] -> Right $ Sep iarg oarg args ret var state param
  xs    -> Left  $ "C cannot return more than one argument: " ++ show xs
  where
    ifs   = entries ifmap
    param = [ x | Param x <- ifs ]
    args  = sortOn (getPos . pKind) [ x | TPort x <- ifs
                                        , isInArg (pKind x) || isOutArg (pKind x) ]
    iarg  = [ x | x <- args, isInArg (pKind x) ]
    oarg  = [ x | x <- args, isOutArg (pKind x) ]
    var   = [ x | TPort x@(Var _ LocVar _ _) <- ifs ]
    state = [ x | TPort x@(Var _ GlobVar _ _) <- ifs ]
    ret'  = [ x | TPort x@(Var _ RetArg _ _) <- ifs ]
    --------------------------------------------
    voidret = Var "_out_" RetArg (NoTy "void") Nothing
    getPos (InArg x)  = x
    getPos (OutArg x) = x
    isInArg  (InArg _)  = True
    isInArg  _          = False
    isOutArg (OutArg _) = True
    isOutArg _          = False


