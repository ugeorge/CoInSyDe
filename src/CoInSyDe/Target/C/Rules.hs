{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Target.C.Rules
-- Copyright   :  (c) George Ungureanu, 2020
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines the rules for code expansion from different AST elements in a
-- 'CoInSyDe.Target.C.Builder.Proj' to actual C code.
----------------------------------------------------------------------
module CoInSyDe.Target.C.Rules(
  -- * Requirements
  pInclude,
  -- * Types
  pTyDecl,
  -- * Variables
  pVarDecl, pVarInit, pVarBind, pVarUse,
  -- * Functions
  pFunDecl, pMainDef, pFunDef, pFunCode, pFunCall
  )  where

import           Control.Arrow ((&&&),second)
import           Control.Monad ((>=>),forM)
import           Control.Monad.State.Lazy (get)
import           Data.ByteString.Lazy as S (fromStrict)
import           Data.Default
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

-- | Generates code for include file.
pInclude :: Text -> CGen (Doc ())
pInclude file = return $ "#include" <+> dquotes (pretty file)

-------------------------------
-- type declarations generator
-------------------------------

-- | Generates code for type declaration.
pTyDecl :: Type C -> CGen (Doc ())
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

-- | Generates code for (global) declaration of variables. Implements the
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
              
-- | Generates code for scoped definitions of variables. Implements the following
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
pVarInit Var {..} = case (pKind,pTy) of
  (InArg{},_)  -> varError "locally initialize" pName pKind (tyId pTy)
  (OutArg{},_) -> varError "locally initialize" pName pKind (tyId pTy)
  (_,NoTy{})   -> varError "locally initialize" pName pKind "void"
  (LocVar,ArrTy i n _ s) -> return $      -- int[3] a = {1,2,3}
    pretty n <+> pretty pName <> brackets (either pretty (pretty . show) s) <>
    maybe emptyDoc ((<+>) (space <> equals) . pretty) pVal
  (GlobVar,_)  -> return $                -- a = value
    pretty pName <>  maybe emptyDoc ((<+>) (space <> equals) . pretty) pVal
  (_,_)        -> return $                -- type a = value
    pretty (tyName pTy) <+> pretty pName <>
    maybe emptyDoc ((<+>) (space <> equals) . pretty) pVal
  
-- | Adds specific prefixes for passing to argument functions.
--   Implements the following LUT:
-- 
-- +--------+-------+--------+--------+--------+---------+
-- |        | InArg | OutArg | RetArg | LocVar | GlobVar |
-- +--------+-------+--------+--------+--------+---------+
-- | void   | X     | X      | X      | X      | X       |
-- +--------+-------+--------+--------+--------+---------+
-- | prim   | a     | &a     | a      | a      | a       |
-- +--------+-------+--------+--------+--------+---------+
-- | enum   | a     | &a     | a      | a      | a       |
-- +--------+-------+--------+--------+--------+---------+
-- | struct | a     | &a     | a      | a      | a       |
-- +--------+-------+--------+--------+--------+---------+
-- | array  | a     | a      | X      | a      | a       |
-- +--------+-------+--------+--------+--------+---------+
-- | forgn  | <i>a  | <o>a   | <i>a   | <i>a   | <i>a    |
-- +--------+-------+--------+--------+--------+---------+
--
-- Nore: for foreign type prefixes check the definition of 'Type C'.
pVarBind :: Port C -> CGen Text
pVarBind Var{..} = case (pKind, pTy) of
  (_, NoTy{})                   -> varError "bind from argument" pName pKind "void"
  (RetArg{}, ArrTy tyId _ _ _)  -> varError "bind from argument" pName pKind tyId
  (OutArg{}, PrimTy{})          -> return $ "&" <> pName 
  (OutArg{}, EnumTy{})          -> return $ "&" <> pName
  (OutArg{}, StrucTy{})         -> return $ "&" <> pName
  (OutArg{}, Foreign _ _ i o _) -> return $ o <> pName
  (_,        Foreign _ _ i o _) -> return $ i <> pName
  (_, _)                        -> return pName

-- | Adds specific prefixes for in-code variable usage. Implements LUT:
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
-- | forgn  | <i>a  | <i>a   | <i>a   | <i>a   | <i>a    |
-- +--------+-------+--------+--------+--------+---------+
--
-- Nore: for foreign type prefixes check the definition of 'Type C'.
pVarUse :: Port C -> CGen Text
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

-- | Generates code for function declaration (type signature).
pFunDecl :: CGen (Doc ())
pFunDecl = do
  cp    <- getCp
  sif   <- categorize (cpIfs cp)
  dRArg <- pVarDecl $ fromMaybe voidVar (snd <$> ret sif)
  dArgs <- mapM (pVarDecl . snd) (args sif)
  return $ dRArg <+> pretty (cpName cp) <+> sepArg dArgs <> semi
                       

-- | Generates code for __main__ function definition. Initializes all global
-- variables. Calls 'pFunCode'.
pMainDef :: [Port C] -> CGen (Doc ())
pMainDef globSts = do
  cp   <- getCp
  sif  <- categorize (cpIfs cp)
  sanity sif cp
  vars <- mapM (fmap (<>semi) . pVarInit) globSts
  code <- pFunCode (cpIfs cp)
  return $ header <+> cBraces (vars ++ code)
  where
    header = "int main(int argc, char ** argv)"
    sanity _ NvComp{} = genError "A native C function cannot be top module!"
    sanity sif _ = case iarg sif ++ oarg sif ++ maybeToList (ret sif) of
      [] -> return ()
      _  -> genError "Main function arguments are not yet supported"

-- | Generates code for normal function definitions.. Calls 'pFunCode'.
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
    TmComp{} -> pFunCode (cpIfs cp)
    NvComp{} -> maybe (genError "Cannot expand native code which was not given!")
                (return . (:[]) . pretty) (cpCode cp)
  return $ header <+> cBraces body

-- | Most important worker in code generation. Generates function code with all the
-- gritty details involved. Main steps are (see source for details):
--
-- * defines and initializes local variables
-- * generates dictionaries of bound variables and renames them based on the custom
--   bind usage. This means that if a custom usage template is found, it expands it
--   (calls Ginger) in the context of the current component.
-- * builds a Ginger context where: 1) the context variables are the parent variables
--   merged with the component variables; 2) the @placeholder@ function is either
--   `pFunCode` if referenced inline or `pFunCall` otherwise, in called with the
--   previously-bound variables.
-- * expands the component template using this context. Transforms it into code.
pFunCode :: IfMap C -> CGen [Doc ()]
pFunCode boundIfs = do
  let msg = "... during interface rebinding in function code expantion\n"
  cp     <- getCp
  layout <- layoutOpts <$> get
  st     <- get
  -- internal variable initializations ----------------------
  sif  <- categorize (cpIfs cp)
  vars <- mapM (fmap (<>semi) . pVarInit . snd) (maybeToList (ret sif) ++ var sif)
  retS <- return  $ maybe emptyDoc
          (\a -> "return" <+> pretty (pName a) <> semi) (snd <$> ret sif)
  -- Compiling new instance names ----------------------
  let scopedIfs = boundIfs `M.union` cpIfs cp
  reboundIfs <- forM (cpRefs cp) $ \ref -> do
    boundWithUsage <- either genError return $ updateOnBind scopedIfs (refBinds ref)
    updatedNames <- mapM (\(i,(b,u)) -> maybe (return (i,b))
                           (bindRename scopedIfs i b) u) $ M.toList boundWithUsage
    return $ M.fromList updatedNames
  -- building template context ----------------------
  useScopedIfs <- mapM applyPVarUse scopedIfs
  let template   = (cpTpl cp) -- { yPreamble = "" }
      dictionary = M.map toYAML useScopedIfs
      genFun :: Id -> Map Text -> Either String [Doc ()]
      genFun n _
        | isNothing inst = Left $ "Did not find placeholder " ++ show n ++ "!"
        | rInline        = Right $ spawnGen st rId (pFunCode newBinds)
        | otherwise      = Right $ spawnGen st rId (pFunCall newBinds)
        where inst = cpRefs cp !? n
              (Ref rId rInline rBinds) = fromJust inst
              newBinds = reboundIfs M.! n
  -- expanding template ----------------------              
  let phFun n args = renderStrict . removeTrailingWhitespace . -- TODO: fix indent
                     layoutPretty layout . indent 2 . vsep <$> genFun n args
      context = mkGingerContext template dictionary phFun
   
  code <- fromTemplate context template
  return $ vars ++ map pretty (T.lines $ T.strip code) ++ [retS]
  where  applyPVarUse (TPort a) = (\n -> TPort (a { pName = n})) <$> pVarUse a
         applyPVarUse param     = return param

-- | Generates code for function call, binding variables to arguments using syntax
-- from `pVarBind`. Called by `pFunCode`
pFunCall :: IfMap C -> CGen [Doc ()]
pFunCall boundIfs = do
  cp     <- getCp
  -- geting IDs of arguments --------------------------
  (retId,argsIds) <- ((fmap fst . ret) &&& (map fst . args)) <$> categorize (cpIfs cp)
  ----------------------------
  bRet  <- mapM (getBoundIf >=> pVarUse) $ maybeToList retId 
  bArgs <- mapM (getBoundIf >=> pVarBind) argsIds
  let funCall = cat (map (\o -> pretty o <+> equals <> space) bRet)
                <> pretty (cpName cp) <> sepArg (map pretty bArgs) <> semi
  return [funCall]
  where getBoundIf i =
          either (genError . (++) "... during instanciation of function call\n")
          (return . ifPort) $ boundIfs !~ i

----------------------------------------------------------------------

bindRename :: IfMap C -> Id -> If C -> Text -> CGen (Id, If C)
bindRename scopedIfs oId oIf usage = do
  let template   = ysrcFromText "" $
                   "{% set namebuild = " <> usage <> " %}\n" <>
                   "{{ namebuild(" <> extractName oId oIf <> ") }}"
      dictionary = M.map toYAML scopedIfs
      context    = mkGingerContext template dictionary (\_ _ -> Right "")
  newName <- fromTemplate context template
  return $ updateName oId oIf newName
  where extractName _ (TPort a) = "\"" <> pName a <> "\"" 
        extractName i (Param _) = i
        updateName  i (TPort a) name = (i, TPort (a { pName = name }))
        updateName  i (Param _) name = (i, TPort (def { pName = name })) 
