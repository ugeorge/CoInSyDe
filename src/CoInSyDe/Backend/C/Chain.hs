module CoInSyDe.Backend.C.Chain where

import CoInSyDe.Backend.C.Core
import CoInSyDe.Backend.C.Proj
import CoInSyDe.Backend.C.Pretty

import Data.Text.Prettyprint.Doc
import qualified Data.Map.Lazy as M

generateCode proj =
  (vsep . punctuate hardline)
  [ pretty (welcome proj)
  , pretty "// Included libraries" <> line
    <> (vsep $ map pInclude $ requmnts proj)
  , pretty "// Custom types" <> line
    <> (vsep $ map pTyDecl $ allTypes proj)
  , pretty "// State variables" <> line
    <> (vsep $ map pVarDecl $ M.elems $ globVars proj)
  ] <> hardline
