{-# LANGUAGE OverloadedStrings #-}
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
import CoInSyDe.Backend.Gen
import CoInSyDe.Backend.C.Proj
import CoInSyDe.Backend.C.Pretty

import Data.Text.Prettyprint.Doc 

-- | Generates Bare-Metal C code from a built C project spec.
generateCode opts (Proj welcome top funDecls requmnts globVars allTypes allFuncs) =
  (vsep . punctuate hardline)
  [ pretty welcome
  , "// Included libraries" <> line
    <> vsep (mapGen "requirements" pInclude requmnts)
  , "// Custom types" <> line
    <> vsep (mapGen "type declaratons" pTyDecl allTypes)
  , "// State variables" <> line
    <> vsep (mapGen "global variable decl" (semiM pVDecl) $ entries globVars)
  , "// Function declarations" <> line
    <> vsep (mapGenCp pFunDecl funDecls)
  , "// Function definitions" <> line
    <> vsep (mapGenCp pFunDef funDecls)
  , "// Main function" <> line
    <> genDoc state top (pMainDef globVars $ allFuncs !* top)
  ] <> hardline
  where
    state = GenS { stage = "", cpDb = allFuncs, layout = opts }
    mapGen s f = map (genDoc state s . f)
    mapGenCp f = map (\x -> genDoc state x $ f (cpDb state !* x))
