{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings #-}
module CoInSyDe.Backend.C.Pretty (
  -- * Main Generator Type
  CGen, semiM,
  -- * Requirements
  pInclude,
  -- * Type Declarations
  pTyDecl,
  -- * Variable Declarations and Initializations
  pVDecl, pVInit, pVDeclInit,
  -- * Function Declaration and Definition
  pFunDecl, pFunCall, pFunDef, pMainDef
  ) where

import           CoInSyDe.Core
import           CoInSyDe.Core.Dict
import           CoInSyDe.Backend.Gen
import           CoInSyDe.Backend.C.Core

import           Data.Aeson hiding (Array,Value)
import qualified Data.HashMap.Strict as H
import           Data.List (sort,sortOn)
import           Data.Text (Text,pack, unpack,append)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text


type CGen = Gen LayoutOptions C (Doc ())

-------------------------------
-- helpers, not exported!
-------------------------------

-- pretty printer helper
sepDef c x = (braces . nest 4 . sep)
             ([softline'] ++ punctuate c x ++ [softline'])
sepArg     = align . parens . sep . punctuate comma
cBraces  x = vsep $ [nest 4 $ vsep $ lbrace : x, rbrace]
semiM      = ((<$>) (<>semi) .)

-- Interface separator. Does a lot of plumbing when it comes to maniputaling the
-- interfaces.
data SeparatedIfs = Sep {
  iarg  :: [If C], oarg  :: [If C], ret ::  If C,  ret'  :: [If C],
  iport :: [If C], oport :: [If C], var :: [If C], state :: [If C], macro :: [If C]
  } deriving (Show)
categorize is = do
  ret <- getOutput ret'
  return $ Sep iarg oarg ret ret' iport oport var state macro
  where
    (macro,vars) = (filter isMacro $ entries is, filter (not . isMacro) $ entries is)
    iarg  = sortOn ifName $ filter ((==InArg) . ifKind) vars
    oarg  = sortOn ifName $ filter ((==OutArg) . ifKind) vars
    iport = filter ((==Get) . ifKind) vars
    oport = filter ((==Put) . ifKind) vars
    var   = filter ((==LocVar) . ifKind) vars
    state = filter ((==GlobVar) . ifKind) vars
    ret'  = filter ((==RetArg) . ifKind) vars
    --------------------------------------------
    getOutput []  = return $ Variable "__OUT_" RetArg (NoTy "void") NoVal
    getOutput [a] = return a
    getOutput xs  = throwError $ "C cannot return more than one argument:\n"
                          ++ show xs

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
pTyDecl (NoTy _) =  error "You cannot declare void type!"

pTyDecl (EnumTy nm vals) = return $
  "typedef enum" <+> (sepDef comma . map initVar) vals <+> pretty nm <> semi
  where initVar (a, Nothing) = pretty a
        initVar (a, Just v)  = pretty a <> equals <> pretty v

-- pTyDecl (Arraym base size) = 
--   pretty (tyName base) <+> pretty nm <> brackets (pretty size)

pTyDecl (Struct nm types) = return $
  "struct" <+> pretty nm
  <+> (sepDef semi . map initType) (idEntries types) <> semi
  where initType (n, ty) = pretty n <+> pretty (tyName ty) 

pTyDecl t = throwError $
  "Code generation for type " ++ show t ++ " is not supported!"

-------------------------------------------
-- generators for variables and interfaces
---------------------------------------------

--         | Decl      | Init          | IBind | OBind | IFDef     | OFDef
-------------------------------------------------------------------------------
-- prim    | int a;    | a = 1;        | f(a   | f(&a  | f(int a   | f(int* a
-- enum    | e_t b;    | b = true;     |  ,b   |  ,&b  |  ,e_t b   |  ,e_t* b
-- struct  | s_t c;    | c.x=1; c.y=2; |  ,c   |  ,&c  |  ,s_t c   |  ,s_t* c
-- array   | int d[3]; | d = {1,2,3};  |  ,d   |  , d  |  ,int* d  |  ,int* d
-- foreign | fifo* e;  | e = mkFifo(3);|  ,e); |  , e);|  ,fifo* e)|  ,fifo* e);
--
-- OBS:- struct init happens in a template/code, as an instance, similar to foreign
--     - ocalls change the scoped name of the variable
--     - foreigns specify their bind/call prefix

-- internal helpers
pVDecl :: If C -> CGen
pVDecl Macro{} = error "Macro should not be declared!"
pVDecl i = case ifTy i of
  (Array b s) -> return $ pretty (tyName b) <+>
                 pretty (ifName i) <> brackets (pretty s)
  _           -> return $ pretty (tyName $ ifTy i) <+> pretty (ifName i)

pVInit :: If C -> CGen
pVInit i = case ifVal i of
  NoVal    -> return emptyDoc
  (Val  v) -> return $ pretty (ifName i) <+> equals <+> pretty v 
  (Cons v) -> pVFun v >>= \f -> return (pretty (ifName i) <+> equals <+> f)

pVDeclInit :: If C -> CGen
pVDeclInit i = case ifVal i of
  NoVal    -> pVDecl i
  (Val  v) -> pVDecl i >>= \x -> return (x <+> equals <+> pretty v) 
  (Cons v) -> pVDecl i >>= \x -> pVFun v >>= \f -> return (x <+> equals <+> f)

pVIBind :: If C -> CGen
pVIBind i
  | isMacro i  = return $ pretty $ macroVal i
  | isGlobal i = return $ "&" <> pretty (ifName i)
  | otherwise  = return $ pretty (ifName i)

pVOBind :: If C -> CGen
pVOBind i@Macro{} = throwError $ "Cannot bind output arg with macro\n " ++ show i
pVOBind i = case ifTy i of
  (Array b s)       -> return $ pretty (ifName i)
  (Foreign n c b _) -> return $ pretty b <> pretty (ifName i)
  _                 -> return $ "&" <> pretty (ifName i)

pVIFDef :: If C -> CGen
pVIFDef i@Macro{} = throwError $ "Macro in type signature!\n " ++ show i
pVIFDef i = case ifTy i of
  (Array b s)       -> return $ pretty (tyName b) <+> "*" <> pretty (ifName i)
  (Foreign n c b _) -> return $ pretty n <+> pretty c <> pretty (ifName i)
  _                 -> return $ pretty (tyName $ ifTy i) <+> pretty (ifName i)

pVOFDef :: If C -> CGen
pVOFDef i@Macro{} = throwError $ "Macro in type signature!\n " ++ show i
pVOFDef i = case ifTy i of
  (Foreign n c b _) -> return $ pretty n <+> pretty c <> pretty (ifName i)
  _                 -> return $ pretty (tyName $ ifTy i) <+> "*" <> pretty (ifName i)

pVFun :: Id -> CGen
pVFun n = do
  db <- getDb
  cp <- getCp
  let inst   = refs cp !?! n
      rCp    = db !* refId inst
      rBinds = bindings inst
  if inline inst
    then nest 4 <$> pFunCode rBinds (refs rCp) (template rCp)
    else pFunCall rBinds rCp

-------------------------------
-- function code generator
-------------------------------

pFunDecl :: Comp C -> CGen
pFunDecl f = do
  sif <- categorize $ ifs f
  let retTy = tyName $ ifTy $ ret sif
  dIArgs <- mapM pVIFDef (iarg sif)
  dOArgs <- mapM pVOFDef (oarg sif)
  return $ pretty retTy <+> pretty (cpName f) <+> sepArg (dOArgs ++ dIArgs) <> semi

pFunCall :: IfMap C -> Comp C -> CGen
pFunCall binds f = do
  sif    <- categorize $ ifs f
  bIArgs <- mapM (\v -> pVIBind $ binds !?! ifName v) (iarg sif)
  bOArgs <- mapM (\v -> pVOBind $ binds !?! ifName v) (oarg sif)
  let r = ret sif
      assignStr 
        | isVoid (ifTy r) = pretty (cpName f)
        | otherwise = pretty (ifName $ binds !?! ifName r)
                      <+> equals <+> pretty (cpName f)
  return $ assignStr <+> sepArg (bOArgs ++ bIArgs) <> semi

pMainDef :: IfMap C -> Comp C -> CGen
pMainDef states top = do
  sif    <- categorize $ ifs top
  sanity sif top
  initSt <- mapM (semiM pVInit) (entries states)
  inVars <- mapM (semiM pVDeclInit) (var sif ++ iport sif ++ oport sif)
  code   <- pFunCode (ifs top) (refs top) (template top)
  return $ header <+> cBraces (initSt ++ inVars ++ loop [code])
  where
    header = "int main(int argc, char ** argv)"
    loop x = ["while (1)" <+> cBraces x]
    sanity _ NvComp{} = throwError "A native C function cannot be top module!"
    sanity sif _ = case iarg sif ++ oarg sif of
      [] -> return ()
      _  -> throwError "Main function arguments are not yet supported"

pFunDef :: Comp C -> CGen
pFunDef f = do 
  sif    <- categorize $ ifs f
  iArgs  <- mapM pVIFDef (iarg sif)
  oArgs  <- mapM pVOFDef (oarg sif)
  inVars <- mapM (semiM pVDeclInit) (var sif ++ iport sif ++ oport sif)
  let retStr = map (\a -> "return" <+> pretty (ifName a) <>semi) (ret' sif)
      retTy  = tyName $ ifTy $ ret sif
      header = pretty retTy <+> pretty (cpName f) <+> sepArg (oArgs ++ iArgs)
  body <- case f of
            TmComp{} -> do
              code <- pFunCode (ifs f) (refs f) (template f)
              return $ inVars ++ [code] ++ retStr
            NvComp{} -> maybe
                        (throwError "Cannot expand native code which was not given!")
                        (return . (:[]) . pretty) (funCode f)
  return $ header <+> cBraces body

-- | TODO: 'cIfs' merged with 'bindings cRefs'. Is it possible?
pFunCode :: IfMap C -> InstMap C -> String -> CGen
pFunCode cIfs cRefs tpl = do
  db     <- getDb
  layout <- getLayoutOpts
  s      <- getState
  let varMap  = makeJsonMap (ids cRefs) cIfs
      genText = renderStrict . removeTrailingWhitespace . layoutPretty layout . genFun
      genFun n
        | rInline   = genDocComp' s (cpName rComp) $
                      pFunCode rBdIfs rBdRefs (template rComp)
        | otherwise = genDoc' s $ pFunCall rBdIfs rComp
        where (Ref rId rInline rBdIfs) = cRefs !?! n
              rComp   = db !* rId
              rBdRefs = replace (refs rComp) rBdIfs
              replace rRefs rBIfs =
                let replaceIf intf = rBIfs !?! ifName intf
                in  fmap (\(Ref n i b) -> Ref n i (fmap replaceIf b)) rRefs
  code <- fromTemplate (mkContext genText varMap) tpl
  return $ pretty code

----------------------------------------------------------------------

makeJsonMap refs cIfs = H.insert "instance" (toJSON $ sort refs) (H.map toJSON cIfs) 

-- HACK!!!

pVOUse Macro{} = error "should not be used as such"
pVOUse i = case ifTy i of
  (Array b s)       -> ifName i
  (Foreign n c b _) -> c   `append` ifName i
  _                 -> "*" `append` ifName i

instance ToJSON (If C)  where
  toJSON (Macro name val) = object [name .= val]
  toJSON i@(Variable _ OutArg t v) =
    object ["name" .= pVOUse i, "type" .= toJSON t, "value" .= toJSON v]
  toJSON (Variable n _ t v) =
    object ["name" .= n, "type" .= toJSON t, "value" .= toJSON v]
