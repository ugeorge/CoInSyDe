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
module CoInSyDe.Backend.C.Proj (
  Proj(..), buildProjStructure, getDependencies
  ) where

import Data.HashMap.Strict as M
import Data.List           as L
import Data.Maybe          as J
import Data.Text           as T

import CoInSyDe.Core
import CoInSyDe.Core.Dict
import CoInSyDe.Backend.C.Core

-- | Structure for a C project.
--
-- TODO: should be part of a Writer-like monad, for more maintainable code generation
-- and error messages!
data Proj = Proj { welcome  :: Text     -- ^ greeter message
                 , mainFun  :: Id       -- ^ top module 'Id'
                 , funDecls :: [Id]     -- ^ functions that need to be declared
                 , requmnts :: [Requ C] -- ^ set of requirements
                 , globVars :: IfMap C  -- ^ set of global variables
                 , allTypes :: [Type C] -- ^ set of types used
                 , allFuncs :: MapH (Comp C)
                 -- ^ dictionary with only the components used in building the
                 -- executable for 'mainFun'.
                 } deriving (Show)

greet = pack "// Generated with CoInSyDe : Code Synthesizer for System Design //"

emptyProj top = Proj greet top [] [] M.empty [] emptyDict

-- | Builds a list of 'Proj' structure for each top module declared.
buildProjStructure :: MapH (Comp C) -- ^ the /complete/ component database
                   -> [Id]   -- ^ list with top module 'Id's
                   -> [Proj]
buildProjStructure db = L.map (\n -> updateProj db (db !* n) (emptyProj n))

traverseProj :: MapH (Comp C) -> Comp C -> Proj -> Proj
traverseProj db cp@NvComp{} proj = updateProj db cp proj
traverseProj db cp@TmComp{} proj =
  M.foldr (\n -> updateProj db (db !* n)) proj (M.map refId $ refs cp)


updateProj :: MapH (Comp C) -> Comp C -> Proj -> Proj
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
    newGlobVars = M.union (globVars proj) (M.filter isState $ ifs cp)
    -- only non-foreign types need to be declared
    newTypes    = nub $ allTypes proj ++ L.filter canDeclare currTypes
    -- transfer component as-is
    newFuncs    = dictTransfer (cpName cp) db (allFuncs proj)
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
    newFunDecls cp@NvComp{} = maybe (L.delete (cpName cp) (funDecls proj))
      (const $ funDecls proj) (funCode cp)
    ---------- AUXILLIARY FUNCTIONS ----------
    canDeclare x = not $ isForeign x || isPrimitive x || isArray x
    currTypes    = J.mapMaybe getTypeOf (M.elems $ ifs cp)
    -- errGlob k    = "Global variable " ++ show k ++ " declared multiple times."

getDependencies = L.map (\(Include f) -> unpack f) . requmnts
