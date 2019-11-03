module CoInSyDe.Backend.C.Chain where


import CoInSyDe.Dictionary
import CoInSyDe.Backend.C.Proj
import CoInSyDe.Backend.C.Pretty

import Data.Text.Prettyprint.Doc
import qualified Data.Map.Lazy as M

generateCode (Proj welcome top funDecls requmnts globVars allTypes allFuncs) =
  (vsep . punctuate hardline)
  [ pretty welcome
  , pretty "// Included libraries" <> line
    <> vsep (map pInclude requmnts)
  , pretty "// Custom types" <> line
    <> vsep (map pTyDecl allTypes)
  , pretty "// State variables" <> line
    <> vsep (map ((<>semi) . pVarDecl) $ M.elems globVars)
  , pretty "// Function declarations" <> line
    <> vsep (map (pFunDecl . (!*) allFuncs) funDecls)
  , pretty "// Function definitions" <> line
    <> vsep (map (pFunDef allFuncs . (!*) allFuncs) funDecls)
  , pretty "// Main function" <> line
    <> pMainFunc allFuncs globVars top
  ] <> hardline
