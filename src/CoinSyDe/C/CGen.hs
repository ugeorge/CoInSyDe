----------------------------------------------------------------------
-- |
-- Module      :  CoinSyDe.C.CGen
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This file contains the code generators C backend.
----------------------------------------------------------------------
module CoinSyDe.C.CGen where

import CoinSyDe.C.Core

import Data.Map.Lazy as M hiding (map)
import Data.Text.Prettyprint.Doc 

-- generates type declarations.
genTyDecl :: Ty -> Maybe C
genTyDecl (PrimTy _) = Nothing
genTyDecl (EnumTy nm vals) = Just $
  pretty "typedef enum" <+> (cBraces . sepDefLine . map initVar) vals <+> pretty nm <> semi
-- genTyDecl (BoolTy nm true false) =
--   "typedef enum {" <+> nest (true <+> "= 1," <+> false <+> " = 0") <+> "}" <+> nm <+> ";"
-- genTyDecl (Struct nm types) =
--   "struct " <+> nm <+> "{\n"
--   <+> intercalate "\n" (map (\(n,ty)->"  "<+> tyName ty <+> " " <+> n <+> ";") types)
--   <+> "};"
-- genTyDecl t = error $ "Code generation for type " <+> show t <+> " is not supported!"

-- helpers for type declarations
cBraces x = lbrace <> nest 4 (softline' <> x) <> softline' <> rbrace
sepDefLine = sep . punctuate comma
initVar (a, Nothing) = pretty a
initVar (a, Just v)  = pretty (a ++ " = " ++ v)
