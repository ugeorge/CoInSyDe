module CoInSyDe.Backend.C.Proj (
  Proj(..), buildProjStructure
  ) where

import Data.List     as L
import Data.Map.Lazy as M
import Data.Text     as T

import CoInSyDe.Dictionary
import CoInSyDe.Core
import CoInSyDe.Backend.C.Core

-- TODO: Monad instance; monadic generation during a single traversal.
data Proj = Proj { welcome  :: Text
                 , funDecls :: [Id]
                 , requmnts :: [Requ C]
                 , globVars :: IfMap C
                 , allTypes :: [Type C]
                 , allFuncs :: Dict (Comp C)
                 } deriving (Show)

greet = pack "// Generated with CoInSyDe : Code Synthesizer for System Design //"

emptyProj = Proj greet [] [] M.empty [] emptyDict

buildProjStructure :: Dict (Comp C) -> [Id] -> [Proj]
buildProjStructure db = L.map (\n -> updateProj db (db !* n) emptyProj)

traverseProj :: Dict (Comp C) -> Comp C -> Proj -> Proj
traverseProj db cp@NvComp{} proj = updateProj db cp proj
traverseProj db cp@TmComp{} proj =
  M.foldr (\n -> updateProj db (db !* n)) proj (M.map refId $ refs cp)


updateProj :: Dict (Comp C) -> Comp C -> Proj -> Proj
updateProj db cp@TmComp{} proj =
  traverseProj db cp $ proj { welcome  = newWelcome
                            , funDecls = newFunDecls
                            , requmnts = newReqmnts
                            , globVars = newGlobVars
                            , allTypes = newTypes
                            , allFuncs = newFuncs
                            }
  where
    newWelcome  = welcome proj
    newFunDecls = nub $ funDecls proj ++
                  (L.map refId . M.elems . M.filter (not . inline)) (refs cp)
    newReqmnts  = nub $ requmnts proj ++ reqs cp
    newGlobVars = M.unionWithKey (\k _ _ -> error $ errGlob k)
                  (globVars proj) (M.filter isState $ ifs cp)
    newTypes    = nub $ allTypes proj ++
                  L.concatMap (maybe [] (:[]) . getTypeOf) (M.elems $ ifs cp)
    newFuncs    = dictTransfer (funName cp) db (allFuncs proj)
    ----------------------------
    errGlob k   = "Global variable " ++ show k ++ " declared multiple times."

updateProj db cp@NvComp{} proj =
  proj { welcome  = newWelcome
       , funDecls = newFunDecls
       , requmnts = newReqmnts
       , globVars = newGlobVars
       , allTypes = newTypes
       , allFuncs = newFuncs
       }
  where
    newWelcome  = welcome proj
    newFunDecls = maybe (L.delete (funName cp) (funDecls proj))
                  (const $ funDecls proj) (funCode cp)
    newReqmnts  = nub $ requmnts proj ++ reqs cp
    newGlobVars = M.unionWithKey (\k _ _ -> error $ errGlob k)
                  (globVars proj) (M.filter isState $ ifs cp)
    newTypes    = nub $ allTypes proj ++
                  L.concatMap (maybe [] (:[]) . getTypeOf) (M.elems $ ifs cp)
    newFuncs    = dictTransfer (funName cp) db (allFuncs proj)
    ----------------------------
    errGlob k   = "Global variable " ++ show k ++ " declared multiple times."                  


