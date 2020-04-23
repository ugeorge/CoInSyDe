{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings #-}
module CoInSyDe.Target.C.Frontend where

import Data.Binary
import Data.Maybe
import Data.Text as T (Text,append,pack,intercalate)
import Data.YAML
import GHC.Generics
import Text.Pandoc.Builder

-- import CoInSyDe.Frontend (YPt,YTm,YNv)
import CoInSyDe.Internal.Map
import CoInSyDe.Internal.YAML
import CoInSyDe.Internal.Docs

--------------------------------------------------------

data CRequ = CInclude Text deriving (Read, Show, Eq, Generic)

instance Binary CRequ

instance FromYAML CRequ where
  parseYAML = withMap "Requirement" $ \r -> do
    incl <- r .: "include"
    return $ CInclude incl 

instance ToDoc CRequ where
  toDoc _ (CInclude r) = plain $ text "include: " <> code r

--------------------------------------------------------

data CType = TyPrim    {ytyName :: Id} 
           | TyEnum    {ytyName :: Id, ytyVals    :: [(Text, Maybe Text)]}
           | TyStruct  {ytyName :: Id, ytyMembers :: [(Text, Id)]}
           | TyArray   {ytyName :: Id, ytyBaseTy  :: Id, ytySize :: Maybe Int}
           | TyForeign {ytyName :: Id, ytyCons :: Id, ytyRequ :: [CRequ]}
            -- TODO: spec usage
            -- | YTyVoid    {ytyName :: Id} -- ^ will always be void
            deriving (Read, Show, Eq, Generic)

instance Binary CType

instance FromYAML CType where
  parseYAML p = forMap "Type" p $ \r -> do
    tclass  <- r .: "class" :: Parser Text
    trgname <- r .: "targetName"
    case tclass of
      "primitive" -> return $ TyPrim trgname
      "enum" -> do
        vals  <- r .: "parameter" >>= parseMapWith "parameter" "Enum values"
                 (\p -> (,) <$> p .: "name" <*> p .:? "value")
        return $ TyEnum trgname vals
      "struct"-> do
        membs <- r .: "parameter" >>= parseMapWith "parameter" "Struct members"
                 (\p -> (,) <$> p .: "name" <*> p .: "type")
        return $ TyStruct trgname membs                                             
      "array" -> do
        baset <- r .: "baseType"
        size  <- r .:? "size"
        return $ TyArray trgname baset size
      "foreign" -> do
        cons  <- r .:  "constructor"
        requs <- r .:? "requirements" >>=? withSeq "Foreign" (mapM parseYAML) .!= []
        return $ TyForeign trgname cons requs
      _ -> typeMismatch "C type" p

instance ToDoc CType where
  toDoc _ (TyPrim n) = plain $ code n <> text ": primitive"
  toDoc _ (TyEnum n m) = definitionList [
    (code n <> text ": enum",
     [foldr1 (<>) $ map (\(p,v) -> plain $ text p <> text (maybe "" (append "=") v)) m])]
  toDoc _ (TyStruct n m) = definitionList [
    (code n <> text ": struct",
     map (\(p,t) -> plain $ text p <> text ":" <> ilink "ty" t ) m)]
  toDoc _ (TyArray n b s) = plain $ code n <> text ": array"
    <> iangles (ilink "ty" b) <> ibracks (text $ pack $ fromMaybe "" $ show <$> s)
  toDoc _ (TyForeign n c r) = definitionList [
    (code n <> text ": foreign" <> iparens (ilink "cp" c), map (toDoc "") r)]

--------------------------------------------------------

-- kind can only be "iarg", "oarg", "retarg", "state", "var"
data CPort = CPort {pName :: Id, pKind :: Text, pType :: Id, ifVal :: [Text]}
  deriving (Show, Generic)

instance Binary CPort

instance FromYAML CPort where
  parseYAML p = forMap "Port" p $ \r -> do
    name <- r .: "name"
    kind <- r .: "kind"
    ty   <- r .: "type"
    val  <- r .:? "init" >>=? withSeq "init values" (mapM parseYAML) .!= []  
    return $ CPort name kind ty val
                  
instance ToDoc CPort where
  toDoc _ (CPort name kind ty val) = (plain $ text ":" <> ilink "ty" ty)
    <> (plain $ text kind <> text "=" <> ibracks (text $ T.intercalate "," val))
