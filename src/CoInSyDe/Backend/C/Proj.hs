module CoInSyDe.Backend.C.Proj (
  Proj(..), buildProjStructure
  ) where

import Data.List     as L
import Data.Map.Lazy as M
import Data.Text     as T
import Data.Maybe    as J

import CoInSyDe.Dictionary
import CoInSyDe.Core
import CoInSyDe.Backend.C.Core

-- TODO: Monad instance; monadic generation during a single traversal.
data Proj = Proj { welcome  :: Text
                 , top      :: Comp C
                 , funDecls :: [Id]
                 , requmnts :: [Requ C]
                 , globVars :: IfMap C
                 , allTypes :: [Type C]
                 , allFuncs :: Dict (Comp C)
                 } deriving (Show)

greet = pack "// Generated with CoInSyDe : Code Synthesizer for System Design //"

emptyProj top = Proj greet top [] [] M.empty [] emptyDict

buildProjStructure :: Dict (Comp C) -> [Id] -> [Proj]
buildProjStructure db = L.map (\n -> updateProj db (db !* n) (emptyProj (db !* n)))

traverseProj :: Dict (Comp C) -> Comp C -> Proj -> Proj
traverseProj db cp@NvComp{} proj = updateProj db cp proj
traverseProj db cp@TmComp{} proj =
  M.foldr (\n -> updateProj db (db !* n)) proj (M.map refId $ refs cp)


updateProj :: Dict (Comp C) -> Comp C -> Proj -> Proj
updateProj db cp proj =
  go cp $ proj { welcome  = newWelcome
               , funDecls = newFunDecls cp
               , requmnts = newReqmnts
               , globVars = newGlobVars
               , allTypes = newTypes
               , allFuncs = newFuncs
               }
  where
    ---------- SAME FOR ALL COMPONENT TYPES ----------
    -- no modification to the greeter message
    newWelcome  = welcome proj
    -- check requirements of used types and components
    newReqmnts  = nub $ requmnts proj ++ reqs cp ++
                  L.concatMap tyRequ (L.filter isForeign currTypes)
    -- only states are allowed to be declared as global variables
    newGlobVars = M.unionWithKey (\k _ _ -> error $ errGlob k)
                  (globVars proj) (M.filter isState $ ifs cp)
    -- only non-foreign types need to be declared
    newTypes    = nub $ allTypes proj ++ L.filter canDeclare currTypes
    -- transfer component as-is
    newFuncs    = dictTransfer (funName cp) db (allFuncs proj)
    ---------- DIFFERENT BETWEEN COMPONENT TYPES ----------
    -- if it is a component, continue traversing the project
    go cp@TmComp{} = traverseProj db cp
    -- if it is a native, stop!
    go cp@NvComp{} = id
    -- only the functions which are not called as inline need to be declared
    newFunDecls cp@TmComp{} = nub $ funDecls proj
      ++ (L.map refId . M.elems . M.filter (not . inline)) (refs cp)
    -- in the case of native functions without source code, they are removed from the
    -- declaration list
    newFunDecls cp@NvComp{} = maybe (L.delete (funName cp) (funDecls proj))
      (const $ funDecls proj) (funCode cp)
    ---------- AUXILLIARY FUNCTIONS ----------
    canDeclare x = not $ isForeign x || isPrimitive x
    currTypes    = J.mapMaybe getTypeOf (M.elems $ ifs cp)
    errGlob k    = "Global variable " ++ show k ++ " declared multiple times."
   
