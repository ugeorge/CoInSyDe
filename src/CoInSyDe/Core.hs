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
  -- * Core Types
  Target(..), Comp(..), If(..), Instance(..), Binding, updateOnBind,
  -- * Core Type Constructors
  mkNative, mkTemplate, mkPattern
  ) where

import Control.Arrow ((***))
import Control.Monad (when)
import Data.Binary
import Data.Either
import Data.HashMap.Strict as M hiding (map,filter,foldr, null)
import Data.Maybe
import Data.Text as T (Text,append,pack,stripEnd)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.YAML (encode1)
import GHC.Generics
import Text.Pandoc.Builder hiding (Target,fromList)

import CoInSyDe.Internal.Docs
import CoInSyDe.Internal.Map
import CoInSyDe.Internal.YAML

------------- ALIASES -------------

type IfMap l = Map (If l)
type InstMap = Map Instance

------------- CORE TYPES -------------

-- | Class for providing a common AST for working with different target languages. @l@
-- is mainly a proxy type, but it defines a type family, as below.
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

-- | Container for functional components. See the <#g:3 type constructor functions> to
-- see how thet are being instantiated.
data Comp l where
  -- | Template functional. Contains template code managed by CoInSyDe
  TmComp :: Target l =>
    { cpName  :: Id          -- ^ unique component ID
    , cpIfs   :: IfMap l     -- ^ maps template hooks to target-relevant ifs
    , cpReqs  :: [Requ l]    -- ^ special requirements for component
    , cpRefs  :: InstMap     -- ^ maps a (template) function placeholder to
                 -- an existing component, with new bindings
    , cpTpl   :: YSrcCode    -- ^ template code
    } -> Comp l
  -- | Native functional. Code used \"as-is\", no manipulation done.
  NvComp :: Target l =>
    { cpName  :: Id         -- ^ unique component ID
    , cpIfs   :: IfMap l    -- ^ maps if names to (caller) interfaces
    , cpReqs  :: [Requ l]   -- ^ special requirements for component
    , cpCode  :: Maybe Text -- ^ maybe native code
    } -> Comp l
deriving instance Target l => Show (Comp l)

-- | Wrapper for interfaces. 
data If l where
  -- | Target-independent parameters (YAML entry).
  Param ::             { ifParam :: YNode } -> If l
  -- | Target-relevant port.
  TPort :: Target l => { ifPort :: Port l } -> If l
deriving instance Target l => Show (If l)

-- | Container used for storing a reference to a functional component. 
data Instance =
  Ref { refId     :: Id        -- ^ functional component ID
      , refInline :: Bool      -- ^ True if expanded inline
      , refBinds  :: [Binding] -- ^ bindings between parent and component
      } deriving (Show, Generic)
instance Binary Instance

-- | Binding between a (callee inerface, caller interface, usage). The third parameter
-- @usage@ can be a Ginger template defining how the port variable is going to be used
-- in the context of the calee.
type Binding = (Id, Id, Maybe Text)

-- | Returns a dictionary of interfaces with eventual custom usage template based on a
-- list of bindings. It returns a safe error message in case any of the binding do not
-- point to existing interfaces.
updateOnBind :: Target l
             => IfMap l   -- ^ caller interface map
             -> [Binding] -- ^ list of bindings
             ->  Either String (Map (If l, Maybe Text))
updateOnBind ifmap bindings =
  let (errs,nIfs) = partitionEithers $ map update bindings
  in case errs of
    [] -> Right $ M.fromList nIfs
    e  -> Left $ unlines e
  where update (repl,with,usage) =
          let renew i = (repl, (i, usage))
          in renew <$> (ifmap !~ with)

------------- EXIFED DICTIONARY BUILDERS -------------

-- | Input parser for native components. __Check source for input syntax.__
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
  return $ NvComp name (ports `union` params) requs (T.stripEnd <$> code)
   

-- | Input parser for template components. __Check source for input syntax.__
mkTemplate :: Target l
           => FilePath
           -> MapH (Type l)   -- ^ (fully-loaded) library of types
           -> YMap            -- ^ @\<root\>@ node
           -> YParse (Comp l) -- ^ updated library of components
mkTemplate path typeLib n = do
  name   <- n @! "name"
  requs  <- n |= "requirement" >>= mapM mkRequ 
  ports  <- n |= "port"        >>= mkParamDict
  params <- n |= "parameter"   >>= mkParamDict
  templ  <- n @! "code"
  tmpos  <- n @^ "code"
  return $ TmComp name (ports `union` params) requs M.empty (YSrc path tmpos "" templ)

-- | Input parser for pattern components. __Check source for input syntax.__
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
mkPortDict tyLib = fmap M.fromList . mapM load
  where load p = do
          name <- p @! "name" 
          port <- mkPort tyLib p
          return (name, TPort port)

mkParamDict :: Target l => [YMap] -> YParse (IfMap l)
mkParamDict = fmap M.fromList . mapM load
  where load p = do
          name <- p @! "name"
          val  <- p @! "value"
          return (name, Param val)

mkInstDict :: Target l => IfMap l -> [YMap] -> YParse (InstMap, IfMap l)
mkInstDict parentIfs = fmap (break . unzip) . mapM load 
  where break (a,b) = (M.fromList a, M.fromList $ concat b)
        load n = do
          to     <- n @! "placeholder"
          from   <- n @! "component"
          inln   <- n @? "inline" @= False
          (b,ex) <- n |= "bind" >>= mkBindDict to parentIfs
          return $ ((to, Ref from inln b), ex)

mkBindDict :: Target l => Text -> IfMap l -> [YMap]
           -> YParse ([Binding], [(Id, If l)])
mkBindDict place pIfs = fmap ((id *** concat) . unzip) . mapM load . zip [0..]
  where load (ix,n) = do
          repl  <- n @! "replace"
          with  <- n @? "with"
          withv <- n @? "withParam"
          usage <- n @? "usage"
          case (with ,withv) of
            (Just with, Nothing) -> do
              _ <- maybe (yamlError n $ "Port or parameter does not exist!")
                   return (pIfs !? with)
              return ((repl, with,usage),[])
            (Nothing, Just with) -> do
              let newRef = "__" `append` place `append` pack (show ix)
              return ((repl, newRef,usage), [(newRef, Param with)])
            _ -> yamlError n "Expected either \"with\" or \"withParam\" in binding!"

  
------------- OTHER INSTANCES -------------

-- | can be dumped to binary file
instance  Target l => Binary (Comp l) where
  put (TmComp n i r f t) = do put (0 :: Word8)
                              put n >> put i >> put r >> put f >> put t
  put (NvComp n i r f)   = do put (1 :: Word8)
                              put n >> put i >> put r >> put f
  get = do tag <- getWord8
           case tag of
             0 -> TmComp <$> get <*> get <*> get <*> get <*> get
             1 -> NvComp <$> get <*> get <*> get <*> get

-- | can be dumped to binary file
instance Target l => Binary (If l) where
  put (Param v) = put (0 :: Word8) >> put v
  put (TPort p) = put (1 :: Word8) >>  put p
  get = getWord8 >>= \tag -> case tag of
                               0 -> Param <$> get
                               1 -> TPort <$> get

-- | can generate documentation
instance Target l => ToDoc (Comp l) where
  toDoc _ cp@TmComp{} = definitionList
    [ (text "interfaces:",    map ifList $ M.toList $ cpIfs cp)
    , (text "requirements:",  map (toDoc "") $ cpReqs cp) 
    , (text "template code:", [codeBlock $ ysrcCode $ cpTpl cp]) ]
    where
      ifList (n,p) = rowTab
        [ simpleTable [] [[plain $ ibold n <> strong ": ",  toDoc "" p]]
        , bulletList (nameBRows n) ]
      fbinds      = formattedBinds (cpRefs cp)
      nameBRows n = maybe [] id $ fbinds !? n
  toDoc _ cp@NvComp{} = definitionList
    [ (text "interfaces:",    map ifList $ M.toList $ cpIfs cp)
    , (text "requirements:",  map (toDoc "") $ cpReqs cp) 
    , (text "native code:", [codeBlock $ fromMaybe "" $ cpCode cp]) ]
    where
      ifList (n,p) = simpleTable [] [[plain $ ibold n <> strong ": ",  toDoc "" p]]
        
-- | can generate documentation
instance Target l => ToDoc (If l) where
  toDoc _ (Param p) = toDoc "" p
  toDoc _ (TPort p) = toDoc "" p

-- | needed to generate documentation
instance ToDoc YNode where
  toDoc _ = codeBlock . toStrict . decodeUtf8 . encode1    

-- | can generate documentation
instance ToDoc l => ToDoc (MapH l) where
  toDoc pref = definitionList . map makedefs . M.toList
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
formattedBinds = groupAll . map format . foldr flatten [] . M.toList
  where
    flatten (plh, Ref cpid il binds) flist = flist ++
      map (\(rpl,with,usage) -> (plh,cpid,il,rpl,with)) binds
    format (plh,cpid,il,rpl,with) =
      let formInline i = if i then ibold "!" else text ""
      in (with, plain (formInline il <> iparens (code plh) <>  math "\\rightarrow"
                <> ilink "cp" cpid <> text ":" <> iangles (code rpl)) ) 
    groupAll xs = M.fromListWith (++) [ (k, [v]) | (k, v) <- xs ]
