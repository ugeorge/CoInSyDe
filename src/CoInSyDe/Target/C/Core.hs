{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module CoInSyDe.Target.C.Core where

import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Graph as G
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as Set
import           Data.Hashable
import           Data.List as L
import           Data.Maybe
import qualified Data.Text as T
import           Data.YAML
import           GHC.Generics

import           CoInSyDe.Frontend
import           CoInSyDe.Internal.Map
import           CoInSyDe.Internal.YAML
import           CoInSyDe.Target.C.Frontend
import           CoInSyDe.Target.C.Rules

type GingerExpr = T.Text
type CNode = Node ()

data CRef = Extern { bref :: Id, bbind :: Map CNode }
          | Inline CComp
          deriving (Show)

data CComp = Cp { cpName :: Id
                , cpDict :: CNode      -- ready-built dict to be passed to Ginger
                , cpCode :: (T.Text,YPos)
                , cpBind :: Map CRef 
                } deriving (Show,Generic)

type CoreBuilder = State BuildSt
data BuildSt = St { callStack :: [(Id,Info)]
                  , typeDb    :: MapH CType
                  , compDb    :: MapH (Comp CRequ CPort)
                  , globVars  :: Map CPort
                  , namespace :: Set.HashSet Id
                  , typeDecls :: Map CType
                  , includes  :: Map (Vertex,[Vertex]) -- Will build a dependecy graph
                  } deriving (Show)

-- returns list of ids for uprocessed components
buildCore :: Id -> Map YNode -> CoreBuilder CComp
buildCore n binds = (compDb <$> get) >>= \cpLib -> case cpLib !? n of
  Just (entry, info:_) -> do
    pushCallStack n info
    
    cps <- case entry of
      PtComp cp -> do
        temp <- case cpLib !? yptType cp of
          Just (TmComp tm,_) -> return $ ytmTpl tm
          _ -> throwError $ "Template type " ++ show (yptType cp)
               ++ " does not exist!"
        updateRequirements (yptReqs cp)
        params <- buildParams binds (yptParams cp) 
        ports  <- buildPorts binds (yptPorts cp)
        binds  <- mapM (buildBinds (yptPorts cp)) (yptRefs cp)
        let name = yptName cp
            dict = mapping ["port" .= ports, "param" .= params]
        return $ Cp name dict temp binds

      TmComp cp -> do
        updateRequirements (ytmReqs cp)
        params <- buildParams binds emptyMap
        ports  <- buildPorts binds emptyMap
        let dict = mapping ["port" .= ports, "param" .= params]
            name = ytmName cp
            temp = ytmTpl cp
        return $ Cp name dict temp emptyMap

      NvComp cp -> do
        unless (isEmpty binds) $ throwError "Native component called inline!"
        ports  <- buildPorts emptyMap (ynvPorts cp)
        let dict = mapping ["port" .= ports]
            name = ynvName cp
            code = ynvCode cp
        updateRequirements (ynvReqs cp)
        return $ Cp name dict code emptyMap

    pullCallStack
    return cps
    
  Nothing -> throwError $ "Referenced component " ++ show n ++ " does not exist!"
  
---------------------------------------------------------------

updateRequirements :: [CRequ] -> CoreBuilder ()
updateRequirements rlist = unless (null rlist) $ do
  rmap <- includes <$> get
  let idlist  = map (\(CInclude x) -> (x, maybe (hash x) fst (rmap !? x))) rlist
      paired  = zipWith (\(i,h) d -> (i,h,d)) idlist
                (map ((:[]) . snd) (tail idlist) ++ [[]])
      newrmap = foldr (\(i,h,d) rm -> insertWith
                                      (\(nh,nd) (mh,md) -> (mh,nd++md)) i (h,d) rm)
                rmap paired
  modify $ \s -> s { includes = newrmap }

-- if both binds and params, keep binds
buildParams :: Map YNode -> Map YNode -> CoreBuilder CNode
buildParams binds = return . mapToNode . liftMap2 M.union binds
  where mapToNode = toYAML . map (\(k,v) -> mapping [k .= v]) . idEntries

buildPorts :: Map YNode -> Map CPort -> CoreBuilder CNode
buildPorts binds ports = (mapToNode . liftMap2 M.union binds) <$> mapM portRule ports
  where mapToNode = toYAML . map (\(k,v) -> mapping [k .= v]) . idEntries

buildBinds :: Map CPort -> (Id,Ref) -> CoreBuilder (Id,CRef)
buildBinds = undefined

---------------------------------------------------------------

portRule :: CPort -> CoreBuilder CNode
portRule p@(CPort n kind ty val) = do
  tyLib <- typeDb <$> get
  name  <- getUniqueName n
  when (kind == "state") $ addGlobVar name p
    

---------------------------------------------------------------

makeArgList :: Map CPort -> CoreBuilder (Id, [Id])
makeArgList ports = do
  let oargs = map fst $ filter (\(_,x) -> pKind x == "oarg") $ idEntries ports
      iargs = map fst $ filter (\(_,x) -> pKind x == "iarg") $ idEntries ports
  retarg <- case filter (\(_,x) -> pKind x == "retarg") $ idEntries ports of
              []      -> return ""
              [(x,_)] -> return x
              _       -> throwError "Multiple 'retarg' ports declared!"
  return (retarg, sort $ oargs ++ iargs)

getUniqueName :: Id -> CoreBuilder Id
getUniqueName n = do
  st <- get
  let uname = uniqueName (0 :: Int) n (namespace st)
  modify $ \s -> s { namespace = Set.insert uname $ namespace s }
  return uname
  where
    uniqueName ix name set
      | name `Set.member` set = uniqueName (ix+1)
                                (name `T.append` T.pack (show ix)) set
      | otherwise = name

addGlobVar :: Id -> CPort -> CoreBuilder ()
addGlobVar x p =  modify $ \s -> s {globVars = liftMap (M.insert x p) $ globVars s}

pushCallStack :: Id -> Info -> CoreBuilder ()
pushCallStack x a = modify $ \s -> (s {callStack = (x,a) : callStack s})

pullCallStack :: CoreBuilder ()
pullCallStack = modify $ \s -> s { callStack = tail (callStack s) }

throwError :: String -> CoreBuilder a
throwError msg = do
  st <- get
  let (curr:stack) = callStack st
  error $ "Error representing core for component " ++ show (fst curr)
    ++ "\n+++ defined in " ++ prettyInfo (snd curr)
    ++ concatMap (\(c,i) -> "  +> called by " ++ show c ++ " |" ++ prettyInfo i) stack
    ++ "\n" ++ msg
  
