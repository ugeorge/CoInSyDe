{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Backend.C.Proj
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines a record of different containers built specifically for a C project, having
-- only the information necessary to simplify code generation and pretty printing.
----------------------------------------------------------------------
module CoInSyDe.Target.C.Builder (
  Proj(..), buildProjs, drawDotGraph,
  resolveIncludes, getFunDecls, getTypeDecls, getGlobVars
  ) where

import Data.Graph as G
import Data.HashMap.Strict as M
import Data.HashSet as S
import Data.Hashable
import Data.List as L
import Data.Maybe as J
import Data.Text as T

import Control.Monad
import Control.Monad.State.Lazy
import Text.Dot
import Text.Pandoc.Builder

import CoInSyDe.Core
import CoInSyDe.Internal.Docs
import CoInSyDe.Internal.Map
import CoInSyDe.Target.C.Core


type ProjBuilder = State Proj

-- | Structure for a C project.
data Proj = Proj
  { greeter   :: Text                  -- ^ greeter message
  , topModule :: Id
  , callStack :: [(Id,Info)]
  , namespace :: HashSet Id
  , funDecls  :: HashSet Id            -- ^ functions that need to be declared
  , includes  :: Map (Vertex,[Vertex]) -- ^ will be dependency graph
  , globVars  :: Map (Port C)          -- ^ set of global variables
  , typeDecls :: Map (Type C)          -- ^ set of types used
  , projComps :: MapH (Comp C)         -- ^ only the components called from top module
  } deriving (Show)

emptyProj :: Id -> Proj
emptyProj top = Proj greet top [] S.empty S.empty emptyMap emptyMap emptyMap emptyMap

greet = pack "// Generated with CoInSyDe : Code Synthesizer for System Design //"

resolveIncludes :: Proj -> [Id]
resolveIncludes proj = L.map ((\(i,_,_) -> i) . getNode) $ G.topSort graph
  where (graph,getNode,_)  = graphFromEdges $ L.map (\(i,(h,d)) -> (i,h,d))
                             $ idEntries $ includes proj

getFunDecls :: Proj -> [Id]
getFunDecls = S.toList . funDecls

getTypeDecls :: Proj -> [Type C]
getTypeDecls = entries . typeDecls

getGlobVars :: Proj -> [Port C]
getGlobVars = entries . globVars

--------------------------------------------------------------------

-- | Builds a list of 'Proj' structure for each top module declared.
buildProjs :: MapH (Comp C) -- ^ the /complete/ component database
           -> [Id]          -- ^ list with top module 'Id's
           -> [Proj]
buildProjs db = L.map (\n -> execState (projBuilder db n) (emptyProj n)) 
        
projBuilder :: MapH (Comp C) -> Id -> ProjBuilder ()
projBuilder db n = case db !? n of
  Just (entry, info:_) -> do
    pushCallStack n info
    updateRequirements $ cpReqs entry
    newPorts <- mapM traversePort $ cpIfs entry
    -- TODO: update greeter with relevant info

    _ <- case entry of
      TmComp{} ->
        forM_ (cpRefs entry) $ \ref ->
          if refInline ref then projBuilder db (refId ref)
          else withFreshNamespace (refId ref) $ projBuilder db (refId ref)
      NvComp{} -> return ()

    let newEntry = entry { cpIfs = newPorts }
    modify $ \s -> s { projComps = dictUpdate Keep n newEntry info $ projComps s }
    popCallStack
    
  Nothing -> throwError $ "Referenced component " ++ show n ++ " does not exist!"
  
-----------------------------------------------

updateRequirements :: [Requ C] -> ProjBuilder ()
updateRequirements rlist = unless (L.null rlist) $ do
  rgraph <- includes <$> get
  let idlist = L.map (\(Include x) -> (x, maybe (hash x) fst (rgraph !? x))) rlist
      depens = L.map ((:[]) . snd) (L.tail idlist) ++ [[]] -- shift-left the hashes
      paired = L.zipWith (\(i,h) d -> (i,h,d)) idlist depens
      rule (i,h,d) = M.insertWith (\(_,nd) (h',md) -> (h',nd++md)) i (h,d)
  modify $ \s -> s { includes = L.foldr (liftMap . rule) rgraph paired }

-- modifies variable name and updates type declarations, namespace, global
-- variable space and requirements
traversePort :: If C -> ProjBuilder (If C)
traversePort (TPort p) = do
  uname <- getUniqueName (pName p)
  let newp = p { pName = uname }
      ty   = pTy p
  when (isGlobal p) $
    modify $ \s -> s { globVars = liftMap (M.insert uname newp) $ globVars s }
  when (isEnum ty || isStruct ty) $
    modify $ \s -> s { typeDecls = liftMap (M.insert (tyId ty) ty) $ typeDecls s }
  when (isForeign ty) $
    updateRequirements $ tyRequ ty
  return $ TPort newp
traversePort p = return p
  
-----------------------------------------------

-- modifies also the function declarations
withFreshNamespace :: Id -> ProjBuilder () -> ProjBuilder ()
withFreshNamespace ref builder = do
  ns <- namespace <$> get
  gl <- globVars <$> get
  modify $ \s -> s { namespace = M.keysSet $ getMap gl }
  builder
  gl <- globVars <$> get
  modify $ \s -> s { namespace = ns `S.union` M.keysSet (getMap gl) }
  modify $ \s -> s { funDecls = S.insert ref $ funDecls s }

getUniqueName :: Id -> ProjBuilder Id
getUniqueName n = do
  st <- get
  let uname = unique (0 :: Int) n (namespace st)
  modify $ \s -> s { namespace = S.insert uname $ namespace s }
  return uname
  where unique ix nm set
          | nm `S.member` set = if (nm `T.append` T.pack (show ix)) `S.member` set
                                then unique (ix+1) nm set
                                else nm `T.append` T.pack (show ix) 
          | otherwise = nm

pushCallStack :: Id -> Info -> ProjBuilder ()
pushCallStack x a = modify $ \s -> s {callStack = (x,a) : callStack s}

popCallStack :: ProjBuilder ()
popCallStack = modify $ \s -> s { callStack = L.tail (callStack s) }
    
throwError :: String -> ProjBuilder a
throwError msg = do
  st <- get
  let (curr:stack) = callStack st
  error $ "Error representing core for component\n+++ " ++ show (fst curr)
    ++ " [" ++ prettyInfo (snd curr) ++ "]"
    ++ L.concatMap (\(c,i) -> "\n  +> called by " ++ show c
                     ++ " [" ++ prettyInfo i ++ "]")
    stack ++ "\n" ++ msg ++ "\n"
  
-----------------------------------------------

instance ToDoc Proj where
  toDoc _ p = 
    header 3 (text "Greeter")
    <> codeBlock (greeter p)
    <> header 3 (text "Include list")
    <> bulletList (L.map codeBlock $ resolveIncludes p)
    <> header 3 (text "Types to declare")
    <> bulletList (L.map (toDoc "") $ getTypeDecls p)
    <> header 3 (text "Functions to declare")
    <> bulletList (L.map (plain . text) $ getFunDecls p)
    <> header 3 (text "Global Variables")
    <> bulletList (L.map (toDoc "") $ getGlobVars p)
    -- <> header 3 (text "Components")
    -- <> toDoc "" (projComps p)

--------------------------------------------------------------------
-- Separate builder for dot project

drawDotGraph :: Proj -> String
drawDotGraph proj = showDot $ dotBuilder True (projComps proj) (topModule proj)


type DotMap = HashMap Id (HashMap Id NodeId)
dotBuilder :: Bool -> MapH (Comp C) -> Id -> Dot (DotMap)
dotBuilder standalone db n = case db !? n of
  Just (cp, _) -> fmap snd . cluster $ do
    attribute ("style","rounded,filled")
    attribute ("label",unpack n)
    if standalone
      then attribute ("fillcolor","lightgrey")
      else attribute ("fillcolor","white")
    
    maps <- case cp of
      NvComp{} -> return M.empty
      TmComp{} -> L.foldr1 M.union <$> forM (entries $ cpRefs cp)
                  (\ref -> dotBuilder (not $ refInline ref) db (refId ref))
      
    ids <- forM (idEntries $ cpIfs cp) $ \(i,p) -> case p of
      TPort (Var n (InArg _) _ _) -> liftM ((,) i)
        $ node [("shape","invtriangle"),("fixedsize","shape"),("label",unpack n)]
      TPort (Var n (OutArg _) _ _) -> liftM ((,) i)
        $ node [("shape","triangle"),("fixedsize","shape"),("label",unpack n)]
      TPort (Var n GlobVar _ _) -> liftM ((,) i)
        $ node [("shape","doubleoctagon"),("label",unpack n)]
      TPort (Var n LocVar _ _) -> liftM ((,) i)
        $ node [("shape","ellipse"),("label",unpack n)]
      TPort (Var n RetArg _ _) -> liftM ((,) i)
        $ node [("shape","triangle"),("fixedsize","shape"),("label",unpack n)]
      Param _ -> liftM ((,) i)
        $ node [("shape","parallelogram"),("label",unpack i)]
    let pmap = M.fromList ids

    case cp of
      NvComp{} -> return ()
      TmComp{} -> forM_ (idEntries $ cpRefs cp) $ \(ph,ref) ->
        forM_ (idEntries $ refBinds ref) $ \(repl,with) ->
        case (M.lookup with pmap, M.lookup repl (maps ! (refId ref))) of
          (Just src, Just dst) -> src .->. dst 
          _ -> return ()
          -- _ -> return $ "[WARNING] connection " ++ show (cpName cp) ++ ":"
          --      ++ show with ++ "->" ++ show (refId ref) ++ ":" ++ show repl
          --      ++ " referenced but does not exist!"        

    return $ M.singleton (cpName cp) pmap
