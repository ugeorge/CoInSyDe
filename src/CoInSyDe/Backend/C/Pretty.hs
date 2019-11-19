{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module CoInSyDe.Backend.C.Pretty (
  -- * Main Generator Type
  CGen,
  -- * Requirements
  pInclude,
  -- * Type Declarations
  pTyDecl,
  -- * Variable Declarations and Initializations
  pVarDecl, pVarInit, pVarDeclInit
  ) where

import CoInSyDe.Core
import CoInSyDe.Core.Dict
import CoInSyDe.Backend.Template
import CoInSyDe.Backend.C.Core

import Data.Text (Text,pack, unpack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Aeson as JSON
import           Control.Monad.Writer.Lazy as W

import Data.Vector as V (fromList)
import Data.Maybe
import Text.Read (readMaybe)

type CGen = Gen C (Doc ())

-------------------------------
-- helpers, not exported!
-------------------------------

-- pretty printer helper
sepDef c x = (braces . nest 4 . sep)
                 ([softline'] ++ punctuate c x ++ [softline'])
sepArg = align . parens . sep . punctuate comma
cBraces x = vsep $ [nest 4 $ vsep $ lbrace : x, rbrace]

-------------------------------
-- requirements generator
-------------------------------

pInclude :: Requ C -> CGen
pInclude (Include file) = return $
  "#include" <+> dquotes (pretty file)

-------------------------------
-- type declarations generator
-------------------------------

-- OBS: no PrimTy allowed
pTyDecl :: Type C -> CGen
pTyDecl (NoTy _) = throwCritical "You cannot declare void type!"

pTyDecl (EnumTy nm vals) = return $
  "typedef enum" <+> (sepDef comma . map initVar) vals <+> pretty nm <> semi
  where initVar (a, Nothing) = pretty a
        initVar (a, Just v)  = pretty a <> equals <> pretty v

-- pTyDecl (Array nm base size) = 
--   pretty (tyName base) <+> pretty nm <> brackets (pretty size)

pTyDecl (Struct nm types) = return $
  "struct" <+> pretty nm
  <+> (sepDef semi . map initType) (idEntries types) <> semi
  where initType (n, ty) = pretty n <+> pretty (tyName ty) 

pTyDecl t = throwError $
  "Code generation for type " ++ show t ++ " is not supported!"

-------------------------------------------
-- generators for variables and interfaces
-------------------------------------------
-- internal helpers
pVarT :: If C -> Doc ()
pVarT = pretty . tyName . ifTy

pVarN :: If C -> Doc ()
pVarN = pretty . ifName

pVarV :: If C -> CGen
pVarV = prettify . ifVal
  where
    prettify NoVal    = return emptyDoc
    prettify (Val  v) = return $ space <> equals <+> pretty v 
    prettify (Cons v) = do f <- pVarFunCall v
                           return $ space <> equals <+> f

pVarFunCall :: Name -> CGen
pVarFunCall n = do
  (cp,db) <- getCpAndDb
  let inst   = (refs cp) !?! n
      rCp    = db !* refId inst
      rBinds = bindings inst
  if inline inst
    then nest 4 <$> pFunCode rBinds (refs rCp) (template rCp)
    else pFunCall rBinds rCp

--------------

pVarDecl :: If C -> CGen
pVarDecl Macro{} = throwCritical "Macro should not be declared!"
pVarDecl i = case ifTy i of
  (Array n b s) -> return $ pretty (tyName b)  <+> pVarN i <> brackets (pretty s)
  _             -> return $ pVarT i <+> pVarN i

pVarInit :: If C -> CGen
pVarInit Macro{} = throwCritical "Macro should not be initialized!"
pVarInit i = do v <- pVarV i
                return $ pVarN i <> v

pVarDeclInit :: If C -> CGen
pVarDeclInit Macro{} = throwCritical "Macro should not be initialized!"
pVarDeclInit i = case ifTy i of
  (Array n b s) -> return $ pretty (tyName b) <+> pVarN i <> brackets (pretty s)
  _             -> do v <- pVarV i
                      return $ pVarT i <+> pVarN i <> v

-------------------------------
-- function code generator
-------------------------------

pFunDecl :: Comp C -> CGen
pFunDecl f = do
  sif <- sepIfs $ ifs f
  let retTy = tyName $ ifTy $ ret sif
      name  = cpName f
  varDecls <- mapM pVarDecl $ oarg sif ++ iarg sif
  return $ pretty retTy <+> pretty name <+> sepArg varDecls <> semi

pFunCall :: IfMap C -> Comp C -> CGen
pFunCall binds f = do
  sif    <- sepIfs $ ifs f
  bIArgs <- mapM (\var -> boundI $ binds !?! ifName var) (iarg sif)
  bOArgs <- mapM (\var -> boundO $ binds !?! ifName var) (oarg sif)
  let assignStr = retStr (ret sif) (cpName f)
  return $ assignStr <+> sepArg (bOArgs ++ bIArgs) <> semi
  where
    retStr r n
      | isVoid (ifTy r)  = pretty n
      | otherwise = pretty (ifName $ binds !?! ifName r) <+> pretty n
    boundI parentV
      | isMacro parentV  = return $ pretty $ macroVal parentV
      | isGlobal parentV = return $ "&" <> pretty (ifName parentV)
      | otherwise        = return $ pretty (ifName parentV)
    boundO parentV
      | isMacro parentV = throwErrorH $ "Trying to bind an output arg for "
                          ++ show (cpName f) ++ " with a macro: "
                          ++ show parentV
      | isArray . ifTy $ parentV = return $ pretty (ifName parentV)
      | otherwise       = return $ "&" <> pretty (ifName parentV)


-- pMainFunc :: MapH (Comp C) -> IfMap C -> Comp C -> CDoc
-- pMainFunc db states top | sanity top =
--   header <+> cBraces (initSt ++ varDecl ++ loop[code])
--   where
--     header = pretty "int main(int argc, char ** argv)"
--     initSt = map ((<>semi) . pVarInit db top) (M.elems states)
--     varDecl= map ((<>semi) . pVarDeclInit db top) allVars
--     loop x = [pretty "while (1)" <+> cBraces x]
--     -- callGet= map ((<>semi) . pPortSync db top) getIfs
--     -- callPut= map ((<>semi) . pPortSync db top) putIfs
--     code   = pFunCode db (ifs top) (refs top) (template top)
--     --------------------------------------------------
--     interfs = M.elems $ ifs top
--     getIfs  = filter isGet interfs
--     putIfs  = filter isPut interfs
--     allVars = filter isVar interfs ++ getIfs ++ putIfs
--     --------------------------------------------------
--     sanity t@NvComp{} = error $ "Gen: The native C function "
--                         ++ show (cpName t) ++ " cannot be a top module!"
--     sanity t@TmComp{} =
--       case filter (\x-> isInput x || isOutput x) (M.elems $ ifs t) of
--         [] -> True
--         _ -> error $ "Gen: Function arguments for main functions are not yet"
--              ++ " supported (in top module " ++ show (cpName t) ++ ")"

-- pFunDef :: MapH (Comp C) -> Comp C -> CDoc
-- pFunDef db f = header <+> cBraces body
--   where
--     header = pretty (tyName $ ifTy retArg) <+> pretty (cpName f)
--              <+> sepArg (map pVarDecl inArgs)
--     body   = case f of
--                TmComp{} -> varDecl ++ [code] ++ retStr
--                NvComp{} -> maybe (error critMsg) ((:[]) . pretty) (funCode f)
--     --------------------------------------------------
--     varDecl= map ((<>semi) . pVarDeclInit db f) allVars
--     -- callGet= map ((<>semi) . pPortSync db f) getIfs
--     -- callPut= map ((<>semi) . pPortSync db f) putIfs
--     code   = pFunCode db (ifs f) (refs f) (template f)
--     retStr = map (\a -> pretty "return" <+> pretty (ifName a) <>semi) retArg'
--     --------------------------------------------------
--     interfs = M.elems $ ifs f
--     getIfs  = filter isGet interfs
--     putIfs  = filter isPut interfs
--     retArg  = getOutput (cpName f) interfs
--     retArg' = filter isOutput interfs
--     inArgs  = filter isInput interfs
--     allVars = retArg' ++ filter isVar interfs ++ getIfs ++ putIfs


-- -- | TODO: 'cIfs' merged with 'bindings cRefs'. Is it possible?
pFunCode :: IfMap C
         -> InstMap C
         -> String -- template
         -> CGen
pFunCode = undefined
-- pFunCode db cIfs cRefs = pretty . fst . W.runWriter . generateCode (mkContext genFun varMap)
--   where
--     varMap = M.insert (pack "instance")
--              (JSON.toJSON $ sort $ ids cRefs)  -- sort instance names
--              (M.map JSON.toJSON cIfs) :: M.HashMap Text JSON.Value
--     genFun n
--       | rInline   = renderText $
--                     pFunCode db rBoundIfs rBoundRefs (template rComp)
--       | otherwise = renderText $ pFunCall rBoundIfs rComp
--       where (Ref rId rInline rBoundIfs) = cRefs !?! n
--             rComp      = db !* rId
--             rBoundRefs = replace (refs rComp) rBoundIfs
--             replace rRefs rBIfs =
--               let replaceIf intf = rBIfs !?! ifName intf
--               in M.map (\(Ref n i b)->Ref n i (M.map replaceIf b)) rRefs


-- renderText n = renderStrict (layoutPretty defaultLayoutOptions n)

-- getPHs = filter (\x -> isJust (readMaybe (unpack x) :: Maybe Int))
