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
  Proj(..) --, buildProjStructure, getDependencies
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

import CoInSyDe.Core
import CoInSyDe.Internal.Map
import CoInSyDe.Target.C.Core


type ProjBuilder = State Proj

-- | Structure for a C project.
data Proj = Proj
  { greeter   :: Text                  -- ^ greeter message
  , topModule :: Id
  , callStack :: [(Id,Info)]
  , namespace :: HashSet Id
  , funDecls  :: [Id]                  -- ^ functions that need to be declared
  , includes  :: Map (Vertex,[Vertex]) -- ^ will be dependency graph
  , globVars  :: Map (Port C)          -- ^ set of global variables
  , typeDecls :: Map (Type C)          -- ^ set of types used
  , allFuncs  :: MapH (Comp C)         -- ^ only the components called from top module
  } deriving (Show)

emptyProj :: Id -> Proj
emptyProj top = Proj greet top [] S.empty [] emptyMap emptyMap emptyMap emptyMap

greet = pack "// Generated with CoInSyDe : Code Synthesizer for System Design //"

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
    traversePorts $ cpIfs entry
  
    -- only time namespace is "reset" is when referenced non-inlined -> to globspace
    popCallStack
    
  Nothing -> throwError $ "Referenced component " ++ show n ++ " does not exist!"

-- -- | Builds a list of 'Proj' structure for each top module declared.
-- buildProjStructure :: MapH (Comp C) -- ^ the /complete/ component database
--                    -> [Id]   -- ^ list with top module 'Id's
--                    -> [Proj]
-- buildProjStructure db = L.map (\n -> updateProj db (db !* n) (emptyProj n))

-- traverseProj :: MapH (Comp C) -> Comp C -> Proj -> Proj
-- traverseProj db cp@NvComp{} proj = updateProj db cp proj
-- traverseProj db cp@TmComp{} proj =
--   M.foldr (\n -> updateProj db (db !* n)) proj (M.map refId $ refs cp)


-- updateProj :: MapH (Comp C) -> Comp C -> Proj -> Proj
-- updateProj db cp proj =
--   go cp $ proj { welcome  = newWelcome
--                , funDecls = newFunDecls cp
--                , requmnts = newReqmnts
--                , globVars = newGlobVars
--                , allTypes = newTypes
--                , allFuncs = newFuncs
--                }
--   where
--     ---------- SAME FOR ALL COMPONENT TYPES ----------
--     -- no modification to the greeter message
--     newWelcome  = welcome proj
--     -- check requirements of used types and components
--     newReqmnts  = nub $ requmnts proj ++ reqs cp ++
--                   L.concatMap tyRequ (L.filter isForeign currTypes)
--     -- only states are allowed to be declared as global variables
--     newGlobVars = M.union (globVars proj) (M.filter isGlobal $ ifs cp)
--     -- only non-foreign types need to be declared
--     newTypes    = nub $ allTypes proj ++ L.filter canDeclare currTypes
--     -- transfer component as-is
--     newFuncs    = dictTransfer (cpName cp) db (allFuncs proj)
--     ---------- DIFFERENT BETWEEN COMPONENT TYPES ----------
--     -- if it is a component, continue traversing the project
--     go cp@TmComp{} = traverseProj db cp
--     -- if it is a native, stop!
--     go cp@NvComp{} = id
--     -- only the functions which are not called as inline need to be declared
--     newFunDecls cp@TmComp{} = nub $ funDecls proj
--       ++ (L.map refId . M.elems . M.filter (not . inline)) (refs cp)
--     -- in the case of native functions without source code, they are removed from the
--     -- declaration list
--     newFunDecls cp@NvComp{} = maybe (L.delete (cpName cp) (funDecls proj))
--       (const $ funDecls proj) (funCode cp)
--     ---------- AUXILLIARY FUNCTIONS ----------
--     canDeclare x = not $ isForeign x || isPrimitive x || isArray x
--     currTypes    = J.mapMaybe getTypeOf (M.elems $ ifs cp)
--     -- errGlob k    = "Global variable " ++ show k ++ " declared multiple times."

-- getDependencies = L.map (\(Include f) -> unpack f) . requmnts


-----------------------------------------------

updateRequirements :: [Requ C] -> ProjBuilder ()
updateRequirements rlist = unless (L.null rlist) $ do
  rgraph <- includes <$> get
  let idlist = L.map (\(Include x) -> (x, maybe (hash x) fst (rgraph !? x))) rlist
      depens = L.map ((:[]) . snd) (L.tail idlist) ++ [[]] -- shift-left the hashes
      paired = L.zipWith (\(i,h) d -> (i,h,d)) idlist depens
      rule (i,h,d) = M.insertWith (\(_,nd) (h',md) -> (h',nd++md)) i (h,d)
  modify $ \s -> s { includes = L.foldr (liftMap . rule) rgraph paired }

-- updates variable names, namespace and global variable space
traversePorts :: IfMap C -> ProjBuilder (IfMap C)
traversePorts = mapM checkVar
  where
    checkVar (TPort p) = do
      uname <- getUniqueName (pName p)
      let newp = p { pName = uname }
      when (pKind p == GlobVar) $
        modify $ \s -> s { globVars = liftMap (M.insert uname newp) $ globVars s }
      return $ TPort newp
    checkVar p = return p


-----------------------------------------------

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
  error $ "Error representing core for component " ++ show (fst curr)
    ++ "\n+++ defined in " ++ prettyInfo (snd curr)
    ++ L.concatMap (\(c,i) -> "  +> called by " ++ show c ++ " |" ++ prettyInfo i)
    stack ++ "\n" ++ msg
  
