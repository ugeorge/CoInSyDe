{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings #-}
module CoInSyDe.Frontend where

import Control.Monad (foldM)
import Data.Binary
import Data.Maybe (fromMaybe)
import Data.Text as T (Text,unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.YAML
import Data.YAML.Event (Tag)
import GHC.Generics
import Text.Pandoc.Builder

import CoInSyDe.Internal.Map
import CoInSyDe.Internal.YAML
import CoInSyDe.Internal.Docs

-----------------------------------------------------------

data YPt req port = YPt { yptName   :: Id
                        , yptType   :: Id
                        , yptPorts   :: Map port
                        , yptParams :: Map YNode
                        , yptRefs   :: Map YRef
                        , yptReqs   :: [req]
                        } deriving (Show, Generic)

instance (Binary req, Binary port) => Binary (YPt req port)

instance (FromYAML req, FromYAML port) => FromYAML (YPt req port) where
  parseYAML = withMap "Pattern component" $ \r -> do
    name  <- r .:  "name"
    kind  <- r .:  "type"
    ports <- r .:? "ports"        >>=? parseMap "name" "Port list"       .!= emptyMap
    param <- r .:? "parameters"   >>=? parseMap "name" "Parameter list"  .!= emptyMap
    refs  <- r .:? "instances"    >>=? parseMap "placeholder" "Refs list".!= emptyMap
    requs <- r .:? "requirements" >>=? parseYAML .!= []
    return $ YPt name kind ports param refs requs

instance (ToDoc req, ToDoc port) => ToDoc (YPt req port) where
  toDoc _ cp = definitionList
    [ (text "template: " <>  ilink "cp" (yptType cp), [btext ""])
    , (text "ports:", map portList $ idEntries $ yptPorts cp)
    , (text "parameters:", map paramList $ idEntries $ yptParams cp)
    , (text "extra parameters:", map rowTab unameBRows)
    , (text "requirements:", [rowTabWith (toDoc "") $ yptReqs cp]) -- TODO
    ]
    where
      fbinds      = formattedBinds (yptRefs cp)
      nameBRows n = maybe [] (map fst) $ fbinds !? n
      unameBRows  = maybe [] (map (\(bl, YParam x) -> [toDoc "" x, bl]))
                    $ fbinds !? "_internal_"
      ------------------------------------------------------
      portList (n,p) = rowTab
        [ simpleTable [] [[plain $ ibold n <> strong ": ",  toDoc "" p]]
        , bulletList (nameBRows n) ]
      paramList (n,p) = rowTab
        [ definitionList [(ibold n <> strong ": ", [toDoc "" p])]
        , bulletList (nameBRows n) ]

-----------------------------------------------------------

data YTm req port = YTm { ytmName   :: Id
                        , ytmPorts  :: Map YNode  -- for documentation only
                        , ytmParams :: Map YNode  -- same
                        , ytmReqs   :: [req]
                        , ytmTpl    :: Text
                        } deriving (Show, Generic)

instance (Binary req, Binary port) => Binary (YTm req port)

instance (FromYAML req, FromYAML port) => FromYAML (YTm req port) where
  parseYAML = withMap "Template component" $ \r -> do
    name  <- r .:  "name"
    ports <- r .:? "ports"        >>=? parseMap "name" "Port list"      .!= emptyMap
    param <- r .:? "parameters"   >>=? parseMap "name" "Parameter list" .!= emptyMap
    requs <- r .:? "requirements" >>=? parseYAML .!= []
    templ <- r .:  "template"
    return $ YTm name ports param requs templ

instance (ToDoc req, ToDoc port) => ToDoc (YTm req port) where
  toDoc _ cp = definitionList
    [ (text "ports:", map portList $ idEntries $ ytmPorts cp)
    , (text "parameters:", map paramList $ idEntries $ ytmParams cp)
    , (text "requirements:", [rowTabWith (toDoc "") $ ytmReqs cp]) -- TODO
    , (text "template code:", [codeBlock $ ytmTpl cp])
    ]
    where
      portList (n,p)  = simpleTable [] [[plain $ ibold n <> strong ": ",  (toDoc "") p]]
      paramList (n,p) = definitionList [(ibold n <> strong ": ", [(toDoc "") p])]

-----------------------------------------------------------

data YNv req port = YNv { ynvName  :: Id
                        , ynvPorts :: Map port
                        , ynvReqs  :: [req]
                        , ynvCode  :: Id
                        } deriving (Show, Generic)

instance (Binary req, Binary port) => Binary (YNv req port)

instance (FromYAML req, FromYAML port) => FromYAML (YNv req port) where
  parseYAML = withMap "Native component" $ \r -> do
    name  <- r .: "name"
    ports <- r .:? "ports"        >>=? parseMap "name" "Port list" .!= emptyMap
    requs <- r .:? "requirements" >>=? parseYAML .!= []
    code  <- r .:? "code" .!= ""
    return $ YNv name ports requs code

instance (ToDoc req, ToDoc port) => ToDoc (YNv req port) where
  toDoc _ cp = definitionList
    [ (text "ports:", map portList $ idEntries $ ynvPorts cp)
    , (text "requirements:", [rowTabWith (toDoc "") $ ynvReqs cp]) -- TODO
    , (text "code:", [codeBlock $ ynvCode cp])
    ]
    where
      portList (n,p)  = simpleTable [] [[plain $ ibold n <> strong ": ",  toDoc "" p]]

-----------------------------------------------------------

data YBind = YPort  Id
           | YIter  Id
           | YParam YNode
           deriving (Show, Generic)
instance Binary YBind

getBindName (YPort id) = id
getBindName (YIter id) = id
getBindName (YParam _) = "_internal_"

data YRef = YRef { yrefId     :: Id
                 , yrefInline :: Bool
                 , yrefBinds  :: Map YBind
                 } deriving (Show, Generic)
instance Binary YRef

instance FromYAML YRef where
  parseYAML = withMap "Reference" $ \r -> do
    from <- r .: "component"
    inln <- r .:? "inline" .!= False
    bind <- r .:? "bindings" >>=? parseMap "replace" "Binding list" .!= emptyMap
    return $ YRef from inln bind

instance FromYAML YBind where
  parseYAML r = withMap "Binding"
    (\b -> do
        port <- b .:? "with"
        val  <- b .:? "withValue"
        iter <- b .:? "withIterator"
        case (port, val, iter) of
          (Just with, Nothing, Nothing) -> return $ YPort with
          (Nothing, Just with, Nothing) -> return $ YParam with
          (Nothing, Nothing, Just with) -> return $ YIter with
          _ -> failAtNode r "Binding not valid"
    ) r
-----------------------------------------------------------

parseMap :: FromYAML v => Text -> String -> YNode -> Parser (Map v)     
parseMap name desc = withSeq desc (fmap mkMap . mapM mkPair)
  where mkPair a = (,) <$> getId a <*> parseYAML a
        getId  = withMap ("Node with "++ unpack name ++" attribute") $ \r -> r .: name

infixl 9 >>=?
(>>=?) :: Monad m => m (Maybe a) -> (a -> m b) -> m (Maybe b) 
a >>=? b = a >>= maybe (return Nothing) (fmap Just . b)

formattedBinds :: Map YRef -> Map [(Blocks,YBind)]
formattedBinds = groupAll . map format . foldr flatten [] . idEntries
  where
    flatten (pl, YRef id il bs) flist =
      map (\(rpl,wt) -> (getBindName wt, (wt,id,rpl,il,pl))) (idEntries bs) ++ flist
    format (n,(wt,id,rpl,il,pl)) =
      let formInline i = if i then ibold "!" else text ""
          formBind (YPort _)  = text
          formBind (YIter _)  = ibracks . text
          formBind (YParam _) = iangles . text
      in (n, ( plain $ formInline il <> iparens (code pl) <>  math "\\rightarrow"
               <> ilink "cp" id <> text ":" <> formBind wt rpl
             , wt))
    groupAll xs = mkMapWith (++) [ (k, [v]) | (k, v) <- xs ]

-----------------------------------------------------------

instance Binary Pos
instance Binary Scalar
instance Binary Tag
instance Binary (Node Pos)

instance ToDoc (Node Pos) where
  toDoc _ = codeBlock . toStrict . decodeUtf8 . encode1
