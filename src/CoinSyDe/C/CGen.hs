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
import CoinSyDe.C.Template


import qualified Data.Map.Lazy as M
import Data.Map.Lazy hiding (map,filter)
import Data.Text.Prettyprint.Doc 
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.FilePath (takeExtension)

-------------------------------
-- type declarations generator
-------------------------------

-- pretty printer helper
sepDefLine c x = (braces . nest 4 . sep)
               ([softline'] ++ punctuate c x ++ [softline'])

genTyDecl :: Ty -> Maybe C
genTyDecl (PrimTy _) = Nothing

genTyDecl (EnumTy nm vals) = Just $
  pretty "typedef enum"
  <+> (sepDefLine comma . map initVar) vals
  <+> pretty nm <> semi
  where initVar (a, Nothing) = pretty a
        initVar (a, Just v)  = pretty (a ++ " = " ++ v)

genTyDecl (BoolTy nm true false) =
  genTyDecl $ EnumTy nm [(true, Just "1"), (false, Just "0")]

genTyDecl (Struct nm types) = Just $
  pretty "struct"
  <+> pretty nm
  <+> (sepDefLine semi . map initType) (toList types) <> semi
  where initType (n, ty) = pretty n <+> pretty (tyName ty) 

genTyDecl t = error $ "Code generation for type " ++ show t ++ " is not supported!"

-------------------------------
-- variable code generators
-------------------------------

genVarDecl (Var nm _ ty _) =
  pretty (tyName ty) <+> pretty nm 
genVarInit (Var nm _ _ (Just vl)) =
  pretty nm <+> equals <+> pretty vl  
genVarDeclInit (Var nm _ ty (Just vl)) =
  pretty (tyName ty) <+> pretty nm <+> equals <+> pretty vl 
genVarDeclInit (Var nm _ ty Nothing)   =
  pretty (tyName ty) <+> pretty nm 

-------------------------------
-- variable code generators
-------------------------------

-- pretty printer helpers
sepArgLine = align . parens . sep . punctuate comma
cBraces x = vsep $ [nest 4 $ vsep $ lbrace : x, rbrace]

-- port getters from variable lists
getInputs    = filter (isInput . varPort)
getStates    = filter (isState . varPort)
getVariables = filter (isVariable . varPort)
getOutput n  = takeOne . filter (isOutput . varPort)
  where takeOne [a] = a
        takeOne ps  = error $ "Somehow function '" ++ n ++
                      "' has not _exactly_ one output port!\n" ++ show ps

isHeader f = takeExtension f `elem` [".h",".hpp", ".hh"]

-- phoney conversion between CFun and TmFun for procedure reuse
phoneyTmFun (CFun name ports file) = (TmFun name False ports M.empty [])

genHIncl :: Fun -> Maybe C
genHIncl TmFun{} = Nothing
genHIncl (CFun _ _ file)
  | isHeader file = Just $ pretty "#include" <+> dquotes (pretty file) <> semi
  | otherwise     = Nothing

genFunDecl :: Fun -> C
genFunDecl (TmFun name inline ports binds templ) =
  pretty (tyName outTy) <+> pretty name
  <+> sepArgLine (map genVarDeclInit inVar) <> semi
  where
    vars  = map snd $ M.toList ports
    outTy = varTy $ getOutput name vars
    inVar = getInputs vars
genFunDecl f@(CFun _ _ file)
  | isHeader file = emptyDoc
  | otherwise     = genFunDecl (phoneyTmFun f)

genFunDef :: Map FunId Fun -> Fun -> C
genFunDef dict (TmFun name inline ports binds templ) =
  header <+> cBraces (outDecl ++ varDecl ++ body ++ retStr)
  where
    header = (pretty $ tyName $ varTy outVar) <+> pretty name
             <+> sepArgLine (map genVarDecl inVars)
    outDecl= [genVarDecl outVar <> semi]
    varDecl= map ((<>semi) . genVarDeclInit) varVars
    body   = genFunCode dict ports binds templ
    retStr = [pretty "return" <+> pretty (varName outVar) <>semi]
    --------------------------------------------------
    vars   = map snd $ M.toList ports
    outVar = getOutput name vars
    inVars = getInputs vars
    varVars= getVariables vars
genFunDef _ (CFun name ports file)
  | isHeader file = emptyDoc
  | otherwise     = pretty "#include" <+> dquotes (pretty file) <> semi

-- TODO: maybe I can work with docs instead of strings
genFunCode :: Map FunId Fun
           -> Map VarId Var
           -> Map Name (FunId, Map VarId Var)
           -> [TTm]
           -> [C]
genFunCode dict cPorts cBind = map generate
  where
    generate (CodeTTm str) = pretty str
    generate (VarTTm str)  = pretty $ answerQ cPorts str
    generate (FunTTm str)
      | isInline pFun = nest 4 $ fillSep $ genFunCode dict bPorts pBind pTemp
      | otherwise     = genFunCall bPorts pFun
      where (pFunId, bPorts) = cBind !?! str
            pFun   = dict !?! pFunId
            pTemp  = funTempl pFun
            pBind  = replace (bindings pFun) bPorts
            --------------------------------------------------
            replace bMap bPorts =
              let replVar (vName, Var vn _ _ _) = (vName, bPorts !?! vn)
              in M.map (\(f,m)->(f, (M.fromList . map replVar . M.toList) m)) bMap

genFunCall :: Map VarId Var -> Fun -> C
genFunCall bind (TmFun name inline ports binds templ) =
  outBName <+> equals <+> pretty name <+> sepArgLine bArgs <> semi
  where
    vars   = map snd $ M.toList ports
    outVar = getOutput name vars
    inVars = getInputs vars
    outBName = pretty $ varName (bind !?! varName outVar)
    bArgs    = map (\var -> pretty $ varName (bind !?! varName var)) inVars
genFunCall bind f@(CFun _ _ _)
  = genFunCall bind (phoneyTmFun f)


genMainFunc :: Map FunId Fun -> [Var] -> Fun -> C
genMainFunc dict states top@(TmFun name inline ports binds templ) =
  header <+> cBraces (initStates ++ outDecl ++ inDecl
                      ++ varDecl ++ message ++ topModule)
  where
    header     = pretty "int main(int argc, char ** argv)"
    initStates = map ((<>semi) . genVarInit) states
    outDecl    = [genVarDeclInit outVar <> semi]
    inDecl     = map ((<>semi) . genVarDeclInit) inVars
    varDecl    | inline    = map ((<>semi) . genVarDeclInit) varVars
               | otherwise = []
    message    = [pretty "\n/***  Here You Write Your Testbench ***/\n"]
    topModule  | inline    = genFunCode dict ports binds templ
               | otherwise = [genFunCall ports top]
    --------------------------------------------------
    vars   = map snd $ M.toList ports
    outVar = getOutput name vars
    inVars = getInputs vars
    varVars= getVariables vars
genMainFunc _ _ (CFun name _ _)
  = error $ "The native C function '" ++ name ++ "' cannot be a top module!"

-------------------------------
-- Queries in variable TTm 
-------------------------------

answerQ binds [name] = varName $ binds !?! name
answerQ binds [name,"value"] = fromJust $ varVal $ binds !?! name
answerQ binds [name,"type"] = tyName $ varTy $ binds !?! name
answerQ binds [name,"type",spec] = specificQ spec $ varTy $ binds !?! name
  where
    specificQ "true"  (BoolTy _ t _) = t
    specificQ "false" (BoolTy _ _ f) = f
    specificQ q ty = error $ "I don't know what '" ++ q ++ "' means for " ++ show ty
answerQ _ q = "I don't know what the template parameter '" ++ show q ++ "' means!"


-------------------------------
-- Generic Utilities 
-------------------------------

infixl 9 !?!

-- Map lookup operator, but which throws a more useful error for debugging.
(!?!) :: (Ord k, Show k, Show a) => Map k a -> k -> a 
m !?! k = check $ m !? k
  where
    check Nothing = error $ "Key '" ++ show  k ++ "' does not exist in:\n" ++ show m
    check (Just a) = a
