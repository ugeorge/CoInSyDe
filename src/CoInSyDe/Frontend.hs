{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings #-}
module CoInSyDe.Frontend where

import Control.Monad (foldM)
import Data.Binary
import Data.Maybe (fromMaybe)
import Data.Text as T (Text,unpack,splitOn,append)
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

data Comp req port = PtComp (Pt req port)
                   | TmComp (Tm req port)
                   | NvComp (Nv req port)
                   deriving (Show)

-----------------------------------------------------------

data Pt req port = Pt { yptName   :: Id
                      , yptType   :: Id
                      , yptPorts  :: Map port
                      , yptParams :: Map YNode
                      , yptRefs   :: Map Ref
                      , yptReqs   :: [req]
                      } deriving (Show, Generic)

instance (Binary req, Binary port) => Binary (Pt req port)

instance (FromYAML req, FromYAML port) => FromYAML (Pt req port) where
  parseYAML = withMap "Pattern component" $ \r -> do
    name  <- r .:  "name"
    kind  <- r .:  "type"
    ports <- r .:? "port"        >>=? parseMap "name" "Port list"       .!= emptyMap
    param <- r .:? "param"       >>=? parseMap "name" "Parameter list"  .!= emptyMap
    refs  <- r .:? "instance"    >>=? parseMap "placeholder" "Refs list".!= emptyMap
    requs <- r .:? "requirement" >>=? parseYAML .!= []
    return $ Pt name kind ports param refs requs

instance (ToDoc req, ToDoc port) => ToDoc (Pt req port) where
  toDoc _ cp = definitionList
    [ (text "template: " <>  ilink "cp" (yptType cp), [btext ""])
    , (text "ports:", map portList $ idEntries $ yptPorts cp)
    , (text "parameter:", map paramList $ idEntries $ yptParams cp)
    , (text "extra parameter:", map rowTab unameBRows)
    , (text "requirements:", [rowTabWith (toDoc "") $ yptReqs cp]) -- TODO
    ]
    where
      fbinds      = formattedBinds (yptRefs cp)
      nameBRows n = maybe [] (map fst) $ fbinds !? n
      unameBRows  = maybe [] (map (\(bl, Param x) -> [toDoc "" x, bl]))
                    $ fbinds !? "_internal_"
      ------------------------------------------------------
      portList (n,p) = rowTab
        [ simpleTable [] [[plain $ ibold n <> strong ": ",  toDoc "" p]]
        , bulletList (nameBRows n) ]
      paramList (n,p) = rowTab
        [ definitionList [(ibold n <> strong ": ", [toDoc "" p])]
        , bulletList (nameBRows n) ]

-----------------------------------------------------------

data Tm req port = Tm { ytmName   :: Id
                      , ytmPorts  :: Map YNode  -- for documentation only
                      , ytmParams :: Map YNode  -- same
                      , ytmReqs   :: [req]
                      , ytmTpl    :: (Text,YPos)
                      } deriving (Show, Generic)

instance (Binary req, Binary port) => Binary (Tm req port)

instance (FromYAML req, FromYAML port) => FromYAML (Tm req port) where
  parseYAML = withMap "Template component" $ \r -> do
    name  <- r .:  "name"
    ports <- r .:? "port"        >>=? parseMap "name" "Port list"      .!= emptyMap
    param <- r .:? "param"       >>=? parseMap "name" "Parameter list" .!= emptyMap
    requs <- r .:? "requirement" >>=? parseYAML .!= []
    templ <- r .:  "template"
    tmpos <- getTextPos <$> r .:  "template"
    return $ Tm name ports param requs (templ, tmpos)

instance (ToDoc req, ToDoc port) => ToDoc (Tm req port) where
  toDoc _ cp = definitionList
    [ (text "ports:", map portList $ idEntries $ ytmPorts cp)
    , (text "parameter:", map paramList $ idEntries $ ytmParams cp)
    , (text "requirements:", [rowTabWith (toDoc "") $ ytmReqs cp]) -- TODO
    , (text "template code:", [codeBlock $ fst $ ytmTpl cp])
    ]
    where
      portList (n,p)  = simpleTable [] [[plain $ ibold n <> strong ": ",  (toDoc "") p]]
      paramList (n,p) = definitionList [(ibold n <> strong ": ", [(toDoc "") p])]

-----------------------------------------------------------

data Nv req port = Nv { ynvName  :: Id
                      , ynvPorts :: Map port
                      , ynvReqs  :: [req]
                      , ynvCode  :: (Text,YPos)
                      } deriving (Show, Generic)

instance (Binary req, Binary port) => Binary (Nv req port)

instance (FromYAML req, FromYAML port) => FromYAML (Nv req port) where
  parseYAML = withMap "Native component" $ \r -> do
    name  <- r .: "name"
    ports <- r .:? "port"        >>=? parseMap "name" "Port list" .!= emptyMap
    requs <- r .:? "requirement" >>=? parseYAML .!= []
    code  <- r .:? "code" .!= ""
    cpos  <- getTextPos <$> r .:  "code"
    return $ Nv name ports requs (code,cpos)

instance (ToDoc req, ToDoc port) => ToDoc (Nv req port) where
  toDoc _ cp = definitionList
    [ (text "ports:", map portList $ idEntries $ ynvPorts cp)
    , (text "requirements:", [rowTabWith (toDoc "") $ ynvReqs cp]) -- TODO
    , (text "code:", [codeBlock $ fst $ ynvCode cp])
    ]
    where
      portList (n,p)  = simpleTable [] [[plain $ ibold n <> strong ": ",  toDoc "" p]]

-----------------------------------------------------------

data Bind = Query T.Text
          | Param YNode
          deriving (Show, Generic)
instance Binary Bind

getBindName (Query q) = head $ splitOn "." q
getBindName (Param _) = "_internal_"

data Ref = Ref { yrefId     :: Id
               , yrefInline :: Bool
               , yrefBinds  :: Map Bind
               } deriving (Show, Generic)
instance Binary Ref

instance FromYAML Ref where
  parseYAML = withMap "Reference" $ \r -> do
    from <- r .: "component"
    inln <- r .:? "inline" .!= False
    bind <- r .:? "bind" >>=? parseMap "replace" "Binding list" .!= emptyMap
    return $ Ref from inln bind

instance FromYAML Bind where
  parseYAML r = withMap "Binding"
    (\b -> do
        port  <- b .:? "with"
        query <- b .:? "withQuery"
        val   <- b .:? "withValue"
        case (port, query, val) of
          (Just with, Nothing, Nothing) -> return $ Query $ with `append` "name"
          (Nothing, Just with, Nothing) -> return $ Query with
          (Nothing, Nothing, Just with) -> return $ Param with
          _ -> failAtNode r "Binding not valid"
    ) r
-----------------------------------------------------------

parseMap :: FromYAML v => Text -> String -> YNode -> Parser (Map v)     
parseMap name desc = withSeq desc (fmap mkMap . mapM mkPair)
  where mkPair a = (,) <$> getId a <*> parseYAML a
        getId  = withMap ("Node with "++ unpack name ++" attribute") $ \r -> r .: name

formattedBinds :: Map Ref -> Map [(Blocks,Bind)]
formattedBinds = groupAll . map format . foldr flatten [] . idEntries
  where
    flatten (pl, Ref id il bs) flist =
      map (\(rpl,wt) -> (getBindName wt, (wt,id,rpl,il,pl))) (idEntries bs) ++ flist
    format (n,(wt,id,rpl,il,pl)) =
      let formInline i = if i then ibold "!" else text ""
          formBind (Query _) = code
          formBind (Param _) = iangles . text
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
