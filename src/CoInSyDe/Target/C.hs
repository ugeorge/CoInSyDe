{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Target.C
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines the synthesis chain towards a generic C target. Re-exports
-- some main utilities defined in its sub-modules.
----------------------------------------------------------------------
module CoInSyDe.Target.C (
  generateCode, C(..),
  -- * C Project
  Proj(topModule,projComps), buildProjs, drawDotGraph,
  resolveIncludes, getFunDecls, getTypeDecls, getGlobVars
  ) where


import CoInSyDe.Target.C.Core
import CoInSyDe.Target.C.Builder
import CoInSyDe.Target.C.Rules
import CoInSyDe.Target.Gen

import Data.Text.Prettyprint.Doc

-- | Generates C code from a built C project spec.
generateCode opts proj =
  (vsep . punctuate hardline)
  [ pretty (greeter proj)
  , "// Included libraries" <> line
    <> vsep (mapGen "declaring requirements" pInclude $ resolveIncludes proj)
  , "// Custom types" <> line
    <> vsep (mapGen "defining custom types" pTyDecl $ getTypeDecls proj)
  , "// State variables" <> line
    <> vsep (mapGen "declaring global variables" pVarDecl $ getGlobVars proj)
  , "// Function declarations" <> line
    <> vsep (mapGenCp "declaring function" pFunDecl $ getFunDecls proj)
  , "// Function definitions" <> line
    <> vsep (mapGenCp "defining function" pFunDef $ getFunDecls proj)
  , "// Main function" <> line
    <> genCodeCp state "defining main function" (topModule proj)
       (pMainDef $ getGlobVars proj)
  ] <> hardline
  where
    state = initGenState (projComps proj) opts
    mapGen   s f = map (genCode state s . f)
    mapGenCp s f = map (\i -> genCodeCp state s i f)
