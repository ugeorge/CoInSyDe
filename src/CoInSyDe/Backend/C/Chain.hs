module CoInSyDe.Backend.C.Chain where

import CoInSyDe.Backend.C.Core
import CoInSyDe.Backend.C.Proj
import CoInSyDe.Backend.C.Pretty

import Data.Text.Prettyprint.Doc

generateCode proj =
  vsep $ punctuate hardline
  [ pretty (welcome proj)
  , mkIncludes proj
  ]

mkIncludes proj =
  pretty "// Include headers" <> line <>
  (vsep $ map pInclude $ requmnts proj)
