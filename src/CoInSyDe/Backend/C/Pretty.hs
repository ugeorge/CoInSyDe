module CoInSyDe.Backend.C.Pretty where

import CoInSyDe.Core
import CoInSyDe.Core.Dict
import CoInSyDe.Backend.Template
import CoInSyDe.Backend.C.Core

import Data.Text (Text,pack, unpack)
import qualified Data.HashMap.Strict as M
-- import Text.PrettyPrint
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Aeson as JSON
import           Control.Monad.Writer.Lazy as W

import Data.Vector as V (fromList)
import Data.Maybe
import Text.Read (readMaybe)
import Data.List (sort)

type CDoc = Doc ()
-------------------------------
-- requirements generator
-------------------------------

pInclude :: Requ C -> CDoc
pInclude (Include file) = pretty "#include" <+> dquotes (pretty file)

-------------------------------
-- type declarations generator
-------------------------------

-- pretty printer helper
sepDefLine c x = (braces . nest 4 . sep)
                 ([softline'] ++ punctuate c x ++ [softline'])

-- OBS: no PrimTy allowed
pTyDecl :: Type C -> CDoc
pTyDecl (NoTy _) = error "Gen: you cannot declare void!"

pTyDecl (EnumTy nm vals) = 
  pretty "typedef enum"
  <+> (sepDefLine comma . map initVar) vals
  <+> pretty nm <> semi
  where initVar (a, Nothing) = pretty a
        initVar (a, Just v)  = pretty a <> equals <> pretty v

-- pTyDecl (Array nm base size) = 
--   pretty (tyName base) <+> pretty nm <> brackets (pretty size)

pTyDecl (Struct nm types) =
  pretty "struct"
  <+> pretty nm
  <+> (sepDefLine semi . map initType) (M.toList types) <> semi
  where initType (n, ty) = pretty n <+> pretty (tyName ty) 

pTyDecl t = error $ "Gen: Code generation for type " ++ show t ++ " is not supported!"

-------------------------------------------
-- generators for variables and interfaces
-------------------------------------------
-- internal helpers
pVarT = pretty . tyName . ifTy
pVarN = pretty . ifName
pVarV db cp = prettify . ifVal
  where
    prettify NoVal    = emptyDoc
    prettify (Val  v) = space <> equals <+> pretty v 
    prettify (Cons v) = space <> equals <+> pVarFunCall db cp v

pVarFunCall :: MapH (Comp C) -> Comp C -> Name -> CDoc
pVarFunCall db cp n =
  let inst    = (refs cp) !?! n
  in  if inline inst
      then let rCp = db !* refId inst
           in  nest 4 $
               pFunCode db (bindings inst) (refs rCp) (template rCp)
      else pFunCall (bindings inst) (db !* refId inst)

--------------

pVarDecl :: If C -> CDoc
pVarDecl Macro{} = error "Gen: Macro should not be declared!"
pVarDecl i = case ifTy i of
  (Array n b s) -> pretty (tyName b) <+> pretty "*" <> pVarN i
  _ -> pVarT i <+> pVarN i

pVarInit :: MapH (Comp C) -> Comp C -> If C -> CDoc
pVarInit _ _ Macro{} = error "Gen: Macro should not be initialized!"
pVarInit db cp i = pVarN i <> pVarV db cp i

pVarDeclInit :: MapH (Comp C) -> Comp C -> If C -> CDoc
pVarDeclInit _ _ Macro{} = error "Gen: Macro should not be initialized!"
pVarDeclInit db cp i = case ifTy i of
  (Array n b s) -> pretty (tyName b) <+> pVarN i <> brackets (pretty s)
  _ -> pVarT i <+> pVarN i <> pVarV db cp i

-- pPortSync :: MapH (Comp C) -> Comp C -> If C -> CDoc
-- pPortSync db cp i@Get{} = pVarFunCall db cp (ifGlue i) 
-- pPortSync db cp i@Put{} = pVarFunCall db cp (ifGlue i)
-- pPortSync _ _ i = error $ "Gen: Interface " ++ show (ifName i)
--                   ++ " is not a get/put port, so it cannot sync!"
  
---------------------
-- interface queries
---------------------

ifQuery db cp cIfs name []        = pretty $ ifName (cIfs !?! name)
ifQuery db cp cIfs name ["type"]  = pretty $ (tyName . ifTy) (cIfs !?! name)
ifQuery db cp cIfs name ["value"] = case cIfs !?! name of
                                      Macro _ val -> pretty val
                                      x -> pVarV db cp x

-------------------------------
-- function code generator
-------------------------------

-- pretty printer helpers
sepArgLine = align . parens . sep . punctuate comma
cBraces x = vsep $ [nest 4 $ vsep $ lbrace : x, rbrace]
critMsg = "Gen [CRITICAL]: The program should not be here!" 

pFunDecl :: Comp C -> CDoc
pFunDecl f =
  pretty (tyName retTy) <+> pretty (cpName f)
  <+> sepArgLine (map pVarDecl inArgs) <> semi
  where
    vars   = M.elems $ ifs f
    retTy  = ifTy $ getOutput (cpName f) vars
    inArgs = filter isInput vars

pFunCall :: IfMap C -> Comp C -> CDoc
pFunCall binds f =
  retStr (pretty (cpName f)) <+> sepArgLine bArgs <> semi
  where
    retStr   | isVoid (ifTy retArg) = (emptyDoc <>)
             | otherwise = (retBName <+> equals <+>)
    vars     = M.elems $ ifs f
    retArg   = getOutput (cpName f) vars
    inArgs   = filter isInput vars
    retBName = pretty $ ifName (binds !?! ifName retArg)
    bArgs    = map (\var -> ifCall (binds !?! ifName var)) inArgs
    --- TODO: test -- I don't want to deal with macros and pointers
    ifCall (Macro _ v) = pretty v
    ifCall (GlobVar n _ _) = pretty "&" <> pretty n
    ifCall i = pretty (ifName i)
    -- ifCall i = case ifTy i of
    --              PtrTy{} -> pretty "&" <> pretty (ifName i)
    --              _       -> pretty (ifName i)


pMainFunc :: MapH (Comp C) -> IfMap C -> Comp C -> CDoc
pMainFunc db states top | sanity top =
  header <+> cBraces (initSt ++ varDecl ++ loop[code])
  where
    header = pretty "int main(int argc, char ** argv)"
    initSt = map ((<>semi) . pVarInit db top) (M.elems states)
    varDecl= map ((<>semi) . pVarDeclInit db top) allVars
    loop x = [pretty "while (1)" <+> cBraces x]
    -- callGet= map ((<>semi) . pPortSync db top) getIfs
    -- callPut= map ((<>semi) . pPortSync db top) putIfs
    code   = pFunCode db (ifs top) (refs top) (template top)
    --------------------------------------------------
    interfs = M.elems $ ifs top
    getIfs  = filter isGet interfs
    putIfs  = filter isPut interfs
    allVars = filter isVar interfs ++ getIfs ++ putIfs
    --------------------------------------------------
    sanity t@NvComp{} = error $ "Gen: The native C function "
                        ++ show (cpName t) ++ " cannot be a top module!"
    sanity t@TmComp{} =
      case filter (\x-> isInput x || isOutput x) (M.elems $ ifs t) of
        [] -> True
        _ -> error $ "Gen: Function arguments for main functions are not yet"
             ++ " supported (in top module " ++ show (cpName t) ++ ")"

pFunDef :: MapH (Comp C) -> Comp C -> CDoc
pFunDef db f = header <+> cBraces body
  where
    header = pretty (tyName $ ifTy retArg) <+> pretty (cpName f)
             <+> sepArgLine (map pVarDecl inArgs)
    body   = case f of
               TmComp{} -> varDecl ++ [code] ++ retStr
               NvComp{} -> maybe (error critMsg) ((:[]) . pretty) (funCode f)
    --------------------------------------------------
    varDecl= map ((<>semi) . pVarDeclInit db f) allVars
    -- callGet= map ((<>semi) . pPortSync db f) getIfs
    -- callPut= map ((<>semi) . pPortSync db f) putIfs
    code   = pFunCode db (ifs f) (refs f) (template f)
    retStr = map (\a -> pretty "return" <+> pretty (ifName a) <>semi) retArg'
    --------------------------------------------------
    interfs = M.elems $ ifs f
    getIfs  = filter isGet interfs
    putIfs  = filter isPut interfs
    retArg  = getOutput (cpName f) interfs
    retArg' = filter isOutput interfs
    inArgs  = filter isInput interfs
    allVars = retArg' ++ filter isVar interfs ++ getIfs ++ putIfs


-- | TODO: 'cIfs' merged with 'bindings cRefs'. Is it possible?
pFunCode :: MapH (Comp C)
         -> IfMap C
         -> InstMap C
         -> String -- template
         -> CDoc
pFunCode db cIfs cRefs = pretty . fst . W.runWriter . generateCode (mkContext genFun varMap)
  where
    varMap = M.insert (pack "instance")
             (JSON.toJSON $ sort $ ids cRefs)  -- sort instance names
             (M.map JSON.toJSON cIfs) :: M.HashMap Text JSON.Value
    genFun n
      | rInline   = renderText $
                    pFunCode db rBoundIfs rBoundRefs (template rComp)
      | otherwise = renderText $ pFunCall rBoundIfs rComp
      where (Ref rId rInline rBoundIfs) = cRefs !?! n
            rComp      = db !* rId
            rBoundRefs = replace (refs rComp) rBoundIfs
            -- rBoundIfs' = M.union rBoundIfs (ifs rComp)  -- raises child's interfaces
            replace rRefs rBIfs =
              let replaceIf intf = rBIfs !?! ifName intf
              in M.map (\(Ref n i b)->Ref n i (M.map replaceIf b)) rRefs


renderText n = renderStrict (layoutPretty defaultLayoutOptions n)

getPHs = filter (\x -> isJust (readMaybe (unpack x) :: Maybe Int))
 
-- pFunCode :: MapH (Comp C)
--          -> Comp C
--          -> IfMap C
--          -> InstMap C
--          -> String -- template
--          -> CDoc
-- pFunCode db cp cIfs cRefs = fillCat . map generate
--   where
--     generate (TCode str) = pretty $ replace (pack "¤") (pack "\n") str
--     generate (TPort n q) = ifQuery db cp cIfs n q
--     generate (TFun  n _) -- TODO: for now query is ignored 
--       | rInline   = nest 4 $
--                     pFunCode db rComp rBoundIfs rBoundRefs (template rComp)
--       | otherwise = pFunCall rBoundIfs rComp
--       where (Ref rId rInline rBoundIfs) = cRefs !?! n
--             rComp      = db !* rId
--             rBoundRefs = replace (refs rComp) rBoundIfs
--             replace rRefs rBIfs =
--               let replaceIf intf = rBIfs !?! ifName intf
--               in M.map (\(Ref n i b)->Ref n i (M.map replaceIf b)) rRefs
