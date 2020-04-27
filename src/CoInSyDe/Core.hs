{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Core
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains the core types and generic methods to build them using the
-- "CoInSyDe.Frontend" API.
----------------------------------------------------------------------
module CoInSyDe.Core (
  -- * Aliases and convenience types
  Id, Map, MapH, IfMap, InstMap,
  mkMap, ids, entries, (!?!),
  -- * Core Types
  Target(..), Comp(..), If(..), Instance(..),
  -- * Core Type Constructors
  mkNative, mkTemplate, mkPattern,
  ) where

import Control.Monad (when)
import Data.Binary
import Data.Maybe
import Data.Text as T (Text,append,pack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.YAML (encode1)
import GHC.Generics
import Text.Pandoc.Builder hiding (Target)

import CoInSyDe.Internal.Map
import CoInSyDe.Internal.YAML
import CoInSyDe.Internal.Docs

------------- ALIASES -------------

type IfMap l = Map (If l)
type InstMap = Map Instance

------------- CORE TYPES -------------

-- | Class for providing a common API for different target languages, where @l@ is
-- mainly a proxy type.
class ( Show (Port l), ToDoc (Port l), Binary (Port l)
      , Show (Type l), ToDoc (Type l), Binary (Type l)
      , Show (Requ l), ToDoc (Requ l), Binary (Requ l)
      ) => Target l where
  -- | A set of data type definitions, relevant to the target language.
  data Type l :: * 
  -- | A set of interface definitions, relevant to the target language.
  data Port l :: *
  -- | A set of special requirements relevant to the target language.
  data Requ l :: *
  -- | Constructor for data types used in common methods
  mkType  :: MapH (Type l) -> YMap -> YParse (Type l)
  -- | Constructor for ifs used in common methods. 
  mkPort  :: MapH (Type l) -> YMap -> YParse (Port l)
  -- | Constructor for requirement type
  mkRequ  :: YMap -> YParse (Requ l)

-- | Container for functional components or glue operators
data Comp l where
  -- | Template functional. Contains template code managed by CoInSyDe
  TmComp :: Target l =>
    { cpName  :: Id          -- ^ unique component ID
    , cpIfs   :: IfMap l     -- ^ maps template hooks to target-relevant ifs
    , cpReqs  :: [Requ l]    -- ^ special requirements for component
    , cpRefs  :: InstMap     -- ^ maps a (template) function placeholder to
                 -- an existing component, with new bindings
    , cpTpl   :: (YPos,Text) -- ^ template code
    } -> Comp l
  -- | Native functional. Code used \"as-is\", no manipulation done.
  NvComp :: Target l =>
    { cpName  :: Id         -- ^ unique component ID
    , cpIfs   :: IfMap l    -- ^ maps if names to (caller) interfaces
    , cpReqs  :: [Requ l]   -- ^ special requirements for component
    , cpCode  :: Maybe Text -- ^ maybe native code
    } -> Comp l
deriving instance Target l => Show (Comp l)

-- | Interface
data If l where
  Param ::             { ifParam :: YNode } -> If l
  TPort :: Target l => { ifPort :: Port l } -> If l
deriving instance Target l => Show (If l)

-- type Query = Id

-- | Container used for storing a reference to a functional component. 
data Instance =
  Ref { refId     :: Id     -- ^ functional component ID
      , refInline :: Bool   -- ^ True if expanded inline
      , refBinds  :: Map Id -- ^ bindings between parent and component
      } deriving (Show, Generic)
instance Binary Instance

------------- EXIFED DICTIONARY BUILDERS -------------

-- | Builds a component dictionaty and load history from nodes
--
--  > <root>/native[@name=*]CTEXT?
-- 
-- These nodes /might/ contain a @CTEXT@ field with the source code for the native
-- function. If it does not, then a @requirement@ child node pointing to the header
-- where the function is defined is necessary.
mkNative :: Target l
         => MapH (Type l)   -- ^ (fully-loaded) library of types
         -> YMap            -- ^ @\<root\>@ node
         -> YParse (Comp l) -- ^ updated library of components
mkNative typeLib n = do
  name   <- n @! "name"
  requs  <- n |= "requirement" >>= mapM mkRequ 
  ports  <- n |= "port"        >>= mkPortDict typeLib
  params <- n |= "parameter"   >>= mkParamDict
  code   <- n @? "code"
  when (isNothing code && null requs) $
    yamlError n "Native code or requirement missing!"
  return $ NvComp name (ports `union` params) requs code
   
-- | Builds a component dictionaty and load history from nodes
--
--  > <root>/template[@name=*]CTEXT
-- 
-- The @CTEXT@ needs to be written in a template langiage, see 'TTm'.
mkTemplate :: Target l
           => MapH (Type l)   -- ^ (fully-loaded) library of types
           -> YMap            -- ^ @\<root\>@ node
           -> YParse (Comp l) -- ^ updated library of components
mkTemplate typeLib n = do
  name   <- n @! "name"
  requs  <- n |= "requirement" >>= mapM mkRequ 
  ports  <- n |= "port"        >>= mkParamDict
  params <- n |= "parameter"   >>= mkParamDict
  templ  <- n @! "code"
  tmpos  <- n @^ "code"
  return $ TmComp name (ports `union` params) requs emptyMap (tmpos,templ)

-- | Builds a component dictionaty and load history from all nodes
--
--  > <root>/pattern[@name=*,@type=*]
mkPattern :: Target l
          => MapH (Type l)   -- ^ (fully-loaded) library of types
          -> MapH (Comp l)   -- ^ (fully-loaded) library of templates
          -> YMap            -- ^ @\<root\>@ node
          -> YParse (Comp l) -- ^ updated library of components
mkPattern tyLib cpLib n = do
  name      <- n @! "name"
  kind      <- (cpLib !*) <$> (n @! "type") >>=
               maybe (yamlError n "Template type not loaded!") return
  requs     <- n |= "requirement" >>= mapM mkRequ 
  ports     <- n |= "port"        >>= mkPortDict tyLib
  params    <- n |= "parameter"   >>= mkParamDict
  (b,extra) <- n |= "instance"    >>= mkInstDict (ports `union` params)
  return $ TmComp name (ports `union` params `union` extra) requs b (cpTpl kind)
                       
------------- INTERNAL DICTIONARY BUILDERS -------------

mkPortDict :: Target l => MapH (Type l) -> [YMap] -> YParse (IfMap l)
mkPortDict tyLib = fmap mkMap . mapM load
  where load p = do
          name <- p @! "name" 
          port <- mkPort tyLib p
          return (name, TPort port)

mkParamDict :: Target l => [YMap] -> YParse (IfMap l)
mkParamDict = fmap mkMap . mapM load
  where load p = do
          name <- p @! "name"
          val  <- p @! "value"
          return (name, Param val)

mkInstDict :: Target l => IfMap l -> [YMap] -> YParse (InstMap, IfMap l)
mkInstDict parentIfs = fmap (break . unzip) . mapM load 
  where break (a,b) = (mkMap a, mkMap $ concat b)
        load n = do
          to     <- n @! "placeholder"
          from   <- n @! "component"
          inln   <- n @? "inline" @= False
          (b,ex) <- n |= "bind" >>= mkBindDict to parentIfs
          return $ ((to, Ref from inln b), ex)

mkBindDict :: Target l => Text -> IfMap l -> [YMap] -> YParse (Map Id, [(Id, If l)])
mkBindDict place pIfs = fmap (break . unzip) . mapM load . zip [0..]
  where break (a,b) = (mkMap a, concat b)
        load (ix,n) = do
          repl  <- n @! "replace"
          with  <- n @? "with"
          withv <- n @? "withParam"
          case (with ,withv) of
            (Just with, Nothing) -> do
              _ <- maybe (yamlError n $ "Port or parameter does not exist!")
                   return (pIfs !? with)
              return ((repl, with),[])
            (Nothing, Just with) -> do
              let newRef = "__" `append` place `append` pack (show ix)
              return ((repl, newRef), [(newRef, Param with)]) 
            _ -> yamlError n "Binding node malformed!"


------------- OTHER INSTANCES -------------

instance  Target l => Binary (Comp l) where
  put (TmComp n i r f t) = do put (0 :: Word8)
                              put n >> put i >> put r >> put f >> put t
  put (NvComp n i r f)   = do put (1 :: Word8)
                              put n >> put i >> put r >> put f
  get = do tag <- getWord8
           case tag of
             0 -> TmComp <$> get <*> get <*> get <*> get <*> get
             1 -> NvComp <$> get <*> get <*> get <*> get


instance Target l => Binary (If l) where
  put (Param v) = put (0 :: Word8) >> put v
  put (TPort p) = put (1 :: Word8) >>  put p
  get = getWord8 >>= \tag -> case tag of
                               0 -> Param <$> get
                               1 -> TPort <$> get

-- monster code for Pandoc "pretty documentation"

instance Target l => ToDoc (Comp l) where
  toDoc _ cp@TmComp{} = definitionList
    [ (text "interfaces:",    map ifList $ idEntries $ cpIfs cp)
    , (text "requirements:",  map (toDoc "") $ cpReqs cp) 
    , (text "template code:", [codeBlock $ snd $ cpTpl cp]) ]
    where
      ifList (n,p) = rowTab
        [ simpleTable [] [[plain $ ibold n <> strong ": ",  toDoc "" p]]
        , bulletList (nameBRows n) ]
      fbinds      = formattedBinds (cpRefs cp)
      nameBRows n = maybe [] id $ fbinds !? n
  toDoc _ cp@NvComp{} = definitionList
    [ (text "interfaces:",    map ifList $ idEntries $ cpIfs cp)
    , (text "requirements:",  map (toDoc "") $ cpReqs cp) 
    , (text "native code:", [codeBlock $ fromMaybe "" $ cpCode cp]) ]
    where
      ifList (n,p) = simpleTable [] [[plain $ ibold n <> strong ": ",  toDoc "" p]]
        
instance Target l => ToDoc (If l) where
  toDoc _ (Param p) = toDoc "" p
  toDoc _ (TPort p) = toDoc "" p

instance ToDoc YNode where
  toDoc _ = codeBlock . toStrict . decodeUtf8 . encode1    

instance ToDoc l => ToDoc (MapH l) where
  toDoc pref = definitionList . map makedefs . idEntries
    where 
      makedefs (n,(e,i))
        = (ibold n, [codeBlock (comment $ head i)] ++ 
                    [divWith (pref `T.append` n,[pack "def",pref],[]) $ toDoc pref e]
                    ++ [showLoad i]  ++ showOverride i ++ [horizontalRule])
      showLoad i = plain $ (text $ pack "Loaded from: ")
                   <> (code $ pack $ prettyInfo $ head i)
      showOverride i = case tail i of
        [] -> []
        x  -> [simpleTable [] [[ plain $ text $ pack "Overrides:"
                               , catMap (plain . code . pack . prettyInfo) x ]]]

-----------------------------------
    
formattedBinds :: InstMap -> Map [Blocks]
formattedBinds = groupAll . map format . foldr flatten [] . idEntries
  where
    flatten (plh, Ref cpid il binds) flist =
      map (\(rpl,with) -> (plh,cpid,il,rpl,with)) (idEntries binds) ++ flist
    format (plh,cpid,il,rpl,with) =
      let formInline i = if i then ibold "!" else text ""
      in (with, plain $ formInline il <> iparens (code plh) <>  math "\\rightarrow"
                <> ilink "cp" cpid <> text ":" <> iangles (code rpl))
    groupAll xs = mkMapWith (++) [ (k, [v]) | (k, v) <- xs ]
