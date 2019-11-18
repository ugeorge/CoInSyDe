----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Backend.C.Chain
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines the synthesis chain towards a generic C target. The reason why this module
-- is separate from "CoInSyDe.Backend.C.Proj" is that different target specializations
-- might have different synthesis chains but might use the same internal project
-- structure.
----------------------------------------------------------------------
module CoInSyDe.Backend.C.Chain where

import CoInSyDe.Core.Dict
import CoInSyDe.Backend.C.Proj
import CoInSyDe.Backend.C.Pretty

-- import Prelude hiding ((<>))
-- import Text.PrettyPrint
import Data.Text.Prettyprint.Doc 

-- | Generates Bare-Metal C code from a built C project spec.
generateCode (Proj welcome top funDecls requmnts globVars allTypes allFuncs) =
  (vsep . punctuate hardline)
  [ pretty welcome
  , pretty "// Included libraries" <> line
    <> vsep (map pInclude requmnts)
  , pretty "// Custom types" <> line
  -- TODO: Include natives here!
    <> vsep (map pTyDecl allTypes)
  , pretty "// State variables" <> line
    <> vsep (map ((<>semi) . pVarDecl) $ entries globVars)
  , pretty "// Function declarations" <> line
    <> vsep (map (pFunDecl . (!*) allFuncs) funDecls)
  , pretty "// Function definitions" <> line
    <> vsep (map (pFunDef allFuncs . (!*) allFuncs) funDecls)
  , pretty "// Main function" <> line
    <> pMainFunc allFuncs globVars (allFuncs !* top)
  ] <> hardline
