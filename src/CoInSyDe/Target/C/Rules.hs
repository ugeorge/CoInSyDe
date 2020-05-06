{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}
module CoInSyDe.Target.C.Rules  where

import           Control.Arrow ((&&&),second)
import           Control.Monad ((>=>))
import           Control.Monad.State.Lazy (get)
import           Data.ByteString.Lazy as S (fromStrict)
import           Data.Either
import qualified Data.HashMap.Strict as M
import           Data.List (sortOn)
import           Data.Maybe
import           Data.Text as T hiding (map,foldr1,filter,concat)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Data.YAML hiding (Doc)

import           CoInSyDe.Core
import           CoInSyDe.Internal.Ginger
import           CoInSyDe.Internal.Map
import           CoInSyDe.Internal.YAML
import           CoInSyDe.Target.C.Core
import           CoInSyDe.Target.Gen

type CGen = CodeGen LayoutOptions C

-------------------------------
-- helpers, not exported!
-------------------------------

-- pretty printer helper
sepDef c x = (braces . nest 2 . sep)
             ([softline'] ++ punctuate c x ++ [softline'])
sepArg     = align . parens . sep . punctuate comma
cBraces  x = vsep [nest 2 $ vsep $ lbrace : x, rbrace]

nullId = "_oOo_" :: Id
voidVar = Var nullId RetArg (NoTy "void") Nothing

-- Interface separator. Does a lot of plumbing so that the user doesn't
data IfSeparator = Sep {
  iarg  :: [(Id, Port C)], oarg  :: [(Id, Port C)], args ::[(Id, Port C)],
  ret   :: Maybe (Id, Port C),   var :: [(Id, Port C)],
  state :: [(Id, Port C)], param :: [(Id, YNode)], ports :: [(Id, Port C)]
  } deriving (Show)
categorize ifmap = case ret' of
  []    -> return $ Sep iarg oarg args Nothing var state param ports
  [ret] -> return $ Sep iarg oarg args (Just ret) var state param ports
  xs    -> genError $ "C cannot return more than one argument: " ++ show xs
  where
    ifs   = M.toList ifmap
    param = [ (i,x) | (i, Param x) <- ifs ]
    iarg  = [ (i,x) | (i, TPort x) <- ifs, isInArg (pKind x) ]
    oarg  = [ (i,x) | (i, TPort x) <- ifs, isOutArg (pKind x) ]
    args  = sortOn (getPos . pKind . snd) $ iarg ++ oarg
    var   = [ (i,x) | (i, TPort x) <- ifs, pKind x == LocVar ]
    state = [ (i,x) | (i, TPort x) <- ifs, pKind x == GlobVar ]
    ret'  = [ (i,x) | (i, TPort x) <- ifs, pKind x == RetArg ]
    ports = args ++ var ++ state ++ ret'
    --------------------------------------------
    getPos (InArg x)  = x
    getPos (OutArg x) = x
    isInArg  (InArg _)  = True
    isInArg  _          = False
    isOutArg (OutArg _) = True
    isOutArg _          = False

-------------------------------
-- requirements generator
-------------------------------

pInclude :: Requ C -> CGen (Doc ())
pInclude (Include file) = return $ "#include" <+> dquotes (pretty file)

pInclude' :: Text -> CGen (Doc ())
pInclude' file = return $ "#include" <+> dquotes (pretty file)


-------------------------------
-- type declarations generator
-------------------------------

-- OBS: no PrimTy allowed
pTyDecl :: Type C -> CGen (Doc ())
pTyDecl (NoTy _) =  error "You cannot declare void type!"

pTyDecl (EnumTy _ nm vals) = return $
  "typedef enum" <+> (sepDef comma . map initVar) vals <+> pretty nm <> semi
  where initVar (a, Nothing) = pretty a
        initVar (a, Just v)  = pretty a <> equals <> pretty v

pTyDecl (StrucTy _ nm types) = return $
  "struct" <+> pretty nm
  <+> sepDef semi (map initType types) <> semi 
  where initType (n, ty) = pretty n <+> pretty (tyName ty) 
-- todo: make constructor

pTyDecl t = genError $
  "Code generation for type " ++ show t ++ " is not supported!"

-------------------------------------------
-- generators for variables and interfaces
---------------------------------------------
varError what name kind ty = genError $
  "Cannot " ++ what ++ " " ++ show kind ++ " " ++ show name ++ " of type " ++ show ty
-- | Generates code for (global space) declaration of variables. Implements the
-- following LUT:
--
-- +---------+-----------+------------+---------+--------+-----------+
-- |         | InArg     | OutArg     | RetArg  | LocVar | GlobVar   |
-- +=========+===========+============+=========+========+===========+
-- | void    | X         | X          | void    | X      | X         |
-- +---------+-----------+------------+---------+--------+-----------+
-- | prim    | int a     | int* a     | int     | X      | int a     |
-- +---------+-----------+------------+---------+--------+-----------+
-- | enum    | enum_t a  | enum_t* a  | enum_t  | X      | enum_t a  |
-- +---------+-----------+------------+---------+--------+-----------+
-- | struct  | struc_t a | struc_t* a | struc_t | X      | struc_t a |
-- +---------+-----------+------------+---------+--------+-----------+
-- | array   | int* a    | int* a     | int*    | X      | int[N] a  |
-- +---------+-----------+------------+---------+--------+-----------+
-- | foreign | foreign a | foreign a  | foreign | X      | foreign a |
-- +---------+-----------+------------+---------+--------+-----------+
pVarDecl :: Port C -> CGen (Doc ())
pVarDecl (Var n LocVar t v) =  varError "globally declare" n LocVar (tyId t)  
pVarDecl (Var n k (NoTy tyn) v) = case k of                 -- ¤¤¤¤¤ VOID ¤¤¤¤¤¤
  RetArg -> return $ pretty tyn                             -- void
  _      -> varError "globally declare" n k "void"          -- !!!
pVarDecl (Var n k (PrimTy tyId tyn) v) = case k of          -- ¤¤¤ PRIMITIVE ¤¤¤
  InArg _  -> return $ pretty tyn <+> pretty n              -- int a  
  OutArg _ -> return $ pretty tyn <> "*" <+> pretty n       -- int* a 
  RetArg   -> return $ pretty tyn                           -- int
  GlobVar  -> return $ pretty tyn <+> pretty n              -- int a
pVarDecl (Var n k (EnumTy tyId tyn _) v) = case k of        -- ¤¤ ENUMERATION ¤¤¤
  InArg _  -> return $ pretty tyn <+> pretty n              -- enum_t a  
  OutArg _ -> return $ pretty tyn <> "*" <+> pretty n       -- enum_t* a 
  RetArg   -> return $ pretty tyn                           -- enum_t
  GlobVar  -> return $ pretty tyn <+> pretty n              -- enum_t a
pVarDecl (Var n k (StrucTy tyId tyn _) v) = case k of       -- ¤¤¤ STRUCTURE ¤¤¤¤
  InArg _  -> return $ pretty tyn <+> pretty n              -- struct_t a  
  OutArg _ -> return $ pretty tyn <> "*" <+> pretty n       -- struct_t* a 
  RetArg   -> return $ pretty tyn                           -- struct_t
  GlobVar  -> return $ pretty tyn <+> pretty n              -- struct_t a
pVarDecl (Var n k (ArrTy tyId tyn _ s) v) = case k of       -- ¤¤¤¤¤ ARRAY ¤¤¤¤¤¤
  InArg _  -> return $ pretty tyn <> "*" <+> pretty n       -- int* a  
  OutArg _ -> return $ pretty tyn <> "*" <+> pretty n       -- int* a 
  RetArg   -> return $ pretty tyn <> "*"                    -- int*
  GlobVar  -> either                                        -- int a[N]
    (\_ -> genError  $ "Global array with non-fixed size " ++ show n)
    (return . (\a -> pretty tyn <+> pretty n <> brackets a) . pretty . show) s
pVarDecl (Var n k (Foreign tyId tyn cp bp _) v) = case k of -- ¤¤¤¤ FOREIGN ¤¤¤¤¤
  InArg _  -> return $ pretty tyn <+> pretty n              -- foreign a 
  OutArg _ -> return $ pretty tyn <+> pretty n              -- foreign a 
  RetArg   -> return $ pretty tyn                           -- foreign
  GlobVar  -> return $ pretty tyn <+> pretty n              -- foreign a
              
-- | Generates code for (local) definitions of variables. Implements the following
-- LUT:
--
-- +---------+-------+--------+------------------+--------------------+-------------+
-- |         | InArg | OutArg | RetArg           | LocVar             | GlobVar     |
-- +=========+=======+========+==================+====================+=============+
-- | void    | X     | X      | X                | X                  | X           |
-- +---------+-------+--------+------------------+--------------------+-------------+
-- | prim    | X     | X      | int a = 1        | int a = 1          | a = 1       |
-- +---------+-------+--------+------------------+--------------------+-------------+
-- | enum    | X     | X      | enum_t a = green | enum_t a = green   | a = green   |
-- +---------+-------+--------+------------------+--------------------+-------------+
-- | struct  | X     | X      | struc_t a = _mk. | struc_t a = _mk    | a = _mk...  |
-- +---------+-------+--------+------------------+--------------------+-------------+
-- | array   | X     | X      | X                | int[N] a = {1,2,3} | a = {1,2,3} |
-- +---------+-------+--------+------------------+--------------------+-------------+
-- | foreign | X     | X      | foreign a = ...  | foreign a = ...    | a = ...     |
-- +---------+-------+--------+------------------+--------------------+-------------+
pVarInit :: Port C -> CGen (Doc ())
pVarInit (Var n k@InArg{} t v)  = varError "locally initialize" n k (tyId t)  
pVarInit (Var n k@OutArg{} t v) = varError "locally initialize" n k (tyId t)  
pVarInit (Var n k (NoTy tyn) v) = varError "locally initialize" n k "void" 
pVarInit (Var n LocVar (ArrTy tyId tyn _ s) v) = -- int[3] a = {1,2,3}
  return $ pretty tyn <+> pretty n <> brackets (either pretty (pretty . show) s) <>
  maybe emptyDoc ((<+>) (space <> equals) . pretty) v
pVarInit (Var n GlobVar ty v)   =                -- a = value
  return $ pretty n <>  maybe emptyDoc ((<+>) (space <> equals) . pretty) v
pVarInit (Var n k ty v)         =                -- type a = value
  return $ pretty (tyName ty) <+> pretty n <>
  maybe emptyDoc ((<+>) (space <> equals) . pretty) v

-- | Generates code for passing arguments based on bindings.
--   Implements the following LUT:
--
-- +--------+-----------+------------+-----------+-----------+-----------+-----------+
-- |        | InArg     | OutArg     | RetArg    | LocVar    | GlobVar   | Param     |
-- +--------+-----------+------------+-----------+-----------+-----------+-----------+
-- | void   | X         | X          | X         | X         | X         | X         |
-- +--------+-----------+------------+-----------+-----------+-----------+-----------+
-- | prim   | {{_name}} | &{{_name}} | {{_name}} | {{_name}} | {{_name}} | {{value}} |
-- +--------+-----------+------------+-----------+-----------+-----------+-----------+
-- | enum   | {{_name}} | &{{_name}} | {{_name}} | {{_name}} | {{_name}} | {{value}} |
-- +--------+-----------+------------+-----------+-----------+-----------+-----------+
-- | struct | {{_name}} | &{{_name}} | {{_name}} | {{_name}} | {{_name}} | {{value}} |
-- +--------+-----------+------------+-----------+-----------+-----------+-----------+
-- | array  | {{_name}} | {{_name}}  | X         | {{_name}} | {{_name}} | {{value}} |
-- +--------+-----------+------------+-----------+-----------+-----------+-----------+
-- | forgn  | {{_name}} | o{{_name}} | {{_name}} | {{_name}} | {{_name}} | {{value}} |
-- +--------+-----------+------------+-----------+-----------+-----------+-----------+
--
-- Nore: @?@ denotes that usage will be replaced with a Ginger template which will
-- generate the actual code. This template is passed through a binding, or, in the
-- case of foreign types, in the definition.
pVarBind :: If C -> CGen Text
pVarBind Param{} = return "{{value}}"
pVarBind (TPort Var{..}) = case (pKind, pTy) of
  (_, NoTy{})                   -> varError "bind from argument" pName pKind "void"
  (RetArg{}, ArrTy tyId _ _ _)  -> varError "bind from argument" pName pKind tyId
  (OutArg{}, PrimTy{})          -> return "&{{_name}}" 
  (OutArg{}, EnumTy{})          -> return "&{{_name}}"
  (OutArg{}, StrucTy{})         -> return "&{{_name}}"
  (OutArg{}, Foreign _ _ i o _) -> return $ o <> "{{_name}}"
  (_,        Foreign _ _ i o _) -> return $ i <> "{{_name}}"
  (_, _)                        -> return  "{{_name}}"

-- | Generates base names for code template usage. Implements LUT:
-- 
-- +--------+-------+--------+--------+--------+---------+
-- |        | InArg | OutArg | RetArg | LocVar | GlobVar |
-- +--------+-------+--------+--------+--------+---------+
-- | void   | X     | X      | X      | X      | X       |
-- +--------+-------+--------+--------+--------+---------+
-- | prim   | a     | *a     | a      | a      | a       |
-- +--------+-------+--------+--------+--------+---------+
-- | enum   | a     | *a     | a      | a      | a       |
-- +--------+-------+--------+--------+--------+---------+
-- | struct | a     | *a     | a      | a      | a       |
-- +--------+-------+--------+--------+--------+---------+
-- | array  | a     | a      | X      | a      | a       |
-- +--------+-------+--------+--------+--------+---------+
-- | forgn  | a     | a      | a      | a      | a       |
-- +--------+-------+--------+--------+--------+---------+
--
-- Nore: @?@ denotes that usage will be replaced with a Ginger template which will
-- generate the actual code. This template is passed through a binding, or, in the
-- case of foreign types, in the definition.
pVarUse :: Port C -> CGen Text
-- pVarUse Param{} = return "{{value}}"
pVarUse Var{..} = case (pKind, pTy) of
  (_, NoTy{})                   -> varError "bind from argument" pName pKind "void"
  (RetArg{}, ArrTy tyId _ _ _)  -> varError "bind from argument" pName pKind tyId
  (OutArg{}, PrimTy{})          -> return $ "*" <> pName 
  (OutArg{}, EnumTy{})          -> return $ "*" <> pName
  (OutArg{}, StrucTy{})         -> return $ "*" <> pName
  (_,        Foreign _ _ i o _) -> return $ i <> pName
  (_, _)                        -> return pName

-------------------------------
-- function code generator
-------------------------------

pFunDecl :: CGen (Doc ())
pFunDecl = do
  cp    <- getCp
  sif   <- categorize (cpIfs cp)
  dRArg <- pVarDecl $ fromMaybe voidVar (snd <$> ret sif)
  dArgs <- mapM (pVarDecl . snd) (args sif)
  return $ dRArg <+> pretty (cpName cp) <+> sepArg dArgs <> semi
                       
pMainDef :: [Port C] -> CGen (Doc ())
pMainDef globSts = do
  cp   <- getCp
  sif  <- categorize (cpIfs cp)
  sanity sif cp
  vars <- mapM (fmap (<>semi) . pVarInit) globSts
  code <- pFunCode M.empty (cpIfs cp)
  return $ header <+> cBraces (vars ++ code)
  where
    header = "int main(int argc, char ** argv)"
    sanity _ NvComp{} = genError "A native C function cannot be top module!"
    sanity sif _ = case iarg sif ++ oarg sif ++ maybeToList (ret sif) of
      [] -> return ()
      _  -> genError "Main function arguments are not yet supported"

pFunDef :: CGen (Doc ())
pFunDef = do
  cp   <- getCp
  --- header ---
  sif  <- categorize (cpIfs cp)
  args <- mapM (pVarDecl . snd) (args sif)
  retA <- pVarDecl $ maybe voidVar snd (ret sif)
  let header = retA <+> pretty (cpName cp) <+> sepArg args
  ---- body ----
  body <- case cp of
            TmComp{} -> pFunCode M.empty (cpIfs cp)
            NvComp{} -> maybe
                        (genError "Cannot expand native code which was not given!")
                        (return . (:[]) . pretty) (cpCode cp)
  return $ header <+> cBraces body

pFunCode :: IfMap C -> CGen [Doc ()]
pFunCode parentMeta boundIfs = do
  let msg = "... during interface rebinding in function code expantion\n"
  cp     <- getCp
  layout <- layoutOpts <$> get
  st     <- get
  ------------------------
  sif  <- categorize (cpIfs cp)
  vars <- mapM (fmap (<>semi) . pVarInit . snd) (maybeToList (ret sif) ++ var sif)
  retS <- return  $ maybe emptyDoc
          (\a -> "return" <+> pretty (pName a) <> semi) (snd <$> ret sif)
  ------------------------
  let preamble = "{% macro use(a) %}{{ _use[a] }}{% endmacro %}\n"
  usemeta <- toYAML <$> mapM pVarUse (M.fromList $ ports sif)
  ------------------------
  let scopedIfs  = boundIfs `M.union` cpIfs cp     
      dictionary = M.unionWith combineNode parentMeta   -- >
                   $ M.insert "_use" usemeta            -- >
                   $ M.mapWithKey ifToYAML scopedIfs    --
      template   = (cpTpl cp) { yPreamble = preamble }
      genFun :: Id -> Map Text -> Either String [Doc ()]
      genFun n tplArgs
        | isNothing inst = Left $ "Did not find placeholder " ++ show n ++ "!"
        | rInline        = generator pFunCode <$> tplMeta <*> newBinds
        | otherwise      = generator pFunCall <$> tplMeta <*> newBinds
        where inst = cpRefs cp !? n
              (Ref rId rInline rBinds) = fromJust inst
              nodesMeta = map (\(i,v) -> (,) i <$> parseYText v) $ M.toList tplArgs
              tplMeta   = case partitionEithers nodesMeta of
                ([],n) -> Right $ M.fromList n
                (e,_)  -> Left $ concat e
              newBinds = updateOnBind scopedIfs rBinds
              generator f a b = spawnGen st rId (f a b)
              
  let phFun n args = renderStrict . removeTrailingWhitespace . -- TODO: fix indent
                     layoutPretty layout . indent 2 . vsep <$> genFun n args
      context = mkGingerContext template dictionary phFun
   
  code <- fromTemplate context template
  return $ vars ++ map pretty (T.lines $ T.strip code) ++ [retS]

pFunCall :: Map (Node ()) -> IfMap C -> CGen [Doc ()]
pFunCall parentMeta boundIfs = do
  cp     <- getCp
  layout <- layoutOpts <$> get
  ----------------------------
  (retId,argsIds) <- ((fmap fst . ret) &&& (map fst . args)) <$> categorize (cpIfs cp)
                     :: CGen (Maybe Id, [Id])
  let retUse = maybe "" (\i -> "{{ use(\"" <> i <> "\") }} = " ) retId 
      argUse = map (\i -> "{{ use(\"" <> i <> "\") }}" ) argsIds
  ----------------------------
  bRet    <- mapM getBoundIf $ maybeToList retId 
  bArgs   <- mapM getBoundIf argsIds
  usemeta <- (toYAML . M.fromList) <$> mapM applyPVarBind (bRet ++ bArgs)
  let preamble = "{% macro use(a) %}{{ eval(_use[a], port(a)) }}{% endmacro %}\n"
  ----------------------------
  let template   = ysrcFromText preamble
                   $ retUse <> cpName cp <> "(" <> intercalate "," argUse <> ");"
      dictionary = M.unionWith combineNode parentMeta   -- >
                   $ M.insert "_use" usemeta            -- >
                   $ M.mapWithKey ifToYAML boundIfs     --
      context    = mkGingerContext template dictionary (\_ _ -> Right "")
  text   <- fromTemplate context template :: CGen Text
  return [pretty text]
  where
    applyPVarBind (i,v) = (,) i <$> pVarBind v
    getBoundIf i = either
                   (genError . (++) "... during instanciation of function call\n")
                   (return . (,) i) $ boundIfs !~ i

----------------------------------------------------------------------

