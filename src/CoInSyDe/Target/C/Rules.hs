{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module CoInSyDe.Target.C.Rules  where

import           Control.Arrow ((&&&))
import           Control.Monad.State.Lazy (get)
import           Data.ByteString.Lazy as S (fromStrict)
import qualified Data.HashMap.Strict as M
import           Data.List (sortOn)
import           Data.Maybe
import           Data.Text as T (Text,append,null,intercalate,pack)
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
sepDef c x = (braces . nest 4 . sep)
             ([softline'] ++ punctuate c x ++ [softline'])
sepArg     = align . parens . sep . punctuate comma
cBraces  x = vsep [nest 4 $ vsep $ lbrace : x, rbrace]
-- semiM      = fmap (<>semi)

nullId = "_oOo_" :: Id
voidVar = Var nullId RetArg (NoTy "void") Nothing

-- Interface separator. Does a lot of plumbing so that the user doesn't
data IfSeparator = Sep {
  iarg  :: [(Id, Port C)], oarg  :: [(Id, Port C)], args ::[(Id, Port C)],
  ret   :: Maybe (Id, Port C),   var :: [(Id, Port C)],
  state :: [(Id, Port C)], param :: [(Id, YNode)]
  } deriving (Show)
categorize ifmap = case ret' of
  []    -> return $ Sep iarg oarg args Nothing var state param
  [ret] -> return $ Sep iarg oarg args (Just ret) var state param
  xs    -> genError $ "C cannot return more than one argument: " ++ show xs
  where
    ifs   = M.toList ifmap
    param = [ (i,x) | (i, Param x _) <- ifs ]
    iarg  = [ (i,x) | (i, TPort x _) <- ifs, isInArg (pKind x) ]
    oarg  = [ (i,x) | (i, TPort x _) <- ifs, isOutArg (pKind x) ]
    args  = sortOn (getPos . pKind . snd) $ iarg ++ oarg
    var   = [ (i,x) | (i, TPort x _) <- ifs, pKind x == LocVar ]
    state = [ (i,x) | (i, TPort x _) <- ifs, pKind x == GlobVar ]
    ret'  = [ (i,x) | (i, TPort x _) <- ifs, pKind x == RetArg ]
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
-- +---------+-------+--------+--------+--------+---------+
-- |         | InArg | OutArg | RetArg | LocVar | GlobVar |
-- +=========+=======+========+========+========+=========+
-- | void    | X     | X      | ""     | X      | X       |
-- +---------+-------+--------+--------+--------+---------+
-- | prim    | a     | &a     | a      | X      | X       |
-- +---------+-------+--------+--------+--------+---------+
-- | enum    | a     | &a     | a      | X      | X       |
-- +---------+-------+--------+--------+--------+---------+
-- | struct  | a?    | &a?    | a?     | X      | X       |
-- +---------+-------+--------+--------+--------+---------+
-- | array   | a?    | a?     | X      | X      | X       |
-- +---------+-------+--------+--------+--------+---------+
-- | foreign | a?    | a?     | a?     | X      | X       |
-- +---------+-------+--------+--------+--------+---------+
--
-- Nore: @?@ denotes that usage will be replaced with a Ginger template which will
-- generate the actual code. This template is passed through a binding, or, in the
-- case of foreign types, in the definition.
pVarBind :: Port C -> CGen (Doc ())
pVarBind (Var n LocVar ty v)    = varError "bind from argument" n LocVar (tyId ty) 
pVarBind (Var n GlobVar ty v)   = varError "bind from argument" n GlobVar (tyId ty) 
pVarBind (Var n k (NoTy tyn) v) = case k of
  RetArg   -> return emptyDoc                               -- ""
  _        -> varError "bind from argument" n k "void"      -- !!! 
pVarBind (Var n k (PrimTy tyId tyn) v) = case k of          -- ¤¤¤ PRIMITIVE ¤¤¤
  InArg _  -> return $ pretty n                             -- "a" 
  OutArg _ -> return $ "&" <> pretty n                      -- "&a" 
  RetArg   -> return $ pretty n <> space                    -- "a "
pVarBind (Var n k (EnumTy tyId tyn _) v) = case k of        -- ¤¤ ENUMERATION ¤¤¤
  InArg _  -> return $ pretty n                             -- "a"  
  OutArg _ -> return $ "&" <> pretty n                      -- "&a" 
  RetArg   -> return $ pretty n <> space                    -- "a "
pVarBind (Var n k (StrucTy tyId tyn _) v) = case k of       -- ¤¤¤ STRUCTURE ¤¤¤¤
  InArg _  -> return $ pretty n                             -- "a"  
  OutArg _ -> return $ "&" <> pretty n                      -- "&a" 
  RetArg   -> return $ pretty n <> space                    -- "a "
pVarBind (Var n k (ArrTy tyId tyn _ s) v) = case k of       -- ¤¤¤¤¤ ARRAY ¤¤¤¤¤¤
  InArg _  -> return $ pretty n                             -- a  
  OutArg _ -> return $ pretty n                             -- a 
  RetArg   -> varError "bind from argument" n k tyId        -- !!!
pVarBind (Var n k (Foreign tyId tyn iu ou _) v) = case k of -- ¤¤¤¤ FOREIGN ¤¤¤¤¤
  OutArg _ -> return $ pretty $ if T.null ou then n else ou -- "a?"
  RetArg   -> return $ pretty (if T.null ou then n else ou) <> space
  _        -> return $ pretty $ if T.null iu then n else iu -- a? 

-- | Generates base names for code template usage. Implements LUT:
-- 
-- +---------+-------+--------+--------+--------+---------+
-- |         | InArg | OutArg | RetArg | LocVar | GlobVar |
-- +=========+=======+========+========+========+=========+
-- | void    | X     | X      | X      | X      | X       |
-- +---------+-------+--------+--------+--------+---------+
-- | prim    | a     | *a     | a      | a      | a       |
-- +---------+-------+--------+--------+--------+---------+
-- | enum    | a     | *a     | a      | a      | a       |
-- +---------+-------+--------+--------+--------+---------+
-- | struct  | a?    | *a?    | a?     | a?     | a?      |
-- +---------+-------+--------+--------+--------+---------+
-- | array   | a?    | a?     | X      | a?     | a?      |
-- +---------+-------+--------+--------+--------+---------+
-- | foreign | a?    | a?     | a?     | a?     | a?      |
-- +---------+-------+--------+--------+--------+---------+
--
-- Nore: @?@ denotes that usage will be replaced with a Ginger template which will
-- generate the actual code. This template is passed through a binding, or, in the
-- case of foreign types, in the definition.
pVarUse :: Port C -> CGen T.Text
pVarUse (Var n k (NoTy tyn) v) = varError "bind from argument" n k "void" 
pVarUse (Var n k (PrimTy tyId tyn) v) = case k of          -- ¤¤¤ PRIMITIVE ¤¤¤
  OutArg _ -> return $ "*" `append` n                      --  *a 
  _        -> return n                                     -- a
pVarUse (Var n k (EnumTy tyId tyn _) v) = case k of        -- ¤¤ ENUMERATION ¤¤¤
  OutArg _ -> return $ "*" `append` n                      --  *a 
  _        -> return n                                     -- a
pVarUse (Var n k (StrucTy tyId tyn _) v) = case k of       -- ¤¤¤ STRUCTURE ¤¤¤¤
  OutArg _ -> return $ "*" `append` n                      --  *a 
  _        -> return n                                     -- a
pVarUse (Var n k (ArrTy tyId tyn _ s) v) = case k of       -- ¤¤¤¤¤ ARRAY ¤¤¤¤¤¤
  RetArg   -> varError "bind from argument" n k tyId       -- !!!
  _        -> return n                                     -- a  
pVarUse (Var n k (Foreign tyId tyn iu ou _) v) = case k of -- ¤¤¤¤ FOREIGN ¤¤¤¤¤
  OutArg _ -> return $ if T.null ou then n else ou         -- a?
  _        -> return $ if T.null iu then n else iu         -- a? 

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
  let varPorts = globSts ++ map snd (var sif ++ maybeToList (ret sif))
  vars <- mapM pVarInit varPorts
  retS <- maybe (return emptyDoc)
          (fmap (\a -> "return" <+> pretty a <> semi) . pVarUse) (snd <$> ret sif)
  code <- pFunCode (cpIfs cp)
  return $ header <+> cBraces (vars ++ [code] ++ [retS])
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
  vars <- mapM (pVarDecl . snd) (maybeToList (ret sif) ++ var sif)
  retS <- maybe (return emptyDoc)
          (fmap (\a -> "return" <+> pretty a <> semi) . pVarUse) (snd <$> ret sif)
  body <- case cp of
            TmComp{} -> do
              code <- pFunCode (cpIfs cp)
              return $ vars ++ [code] ++ [retS]
            NvComp{} -> maybe
                        (genError "Cannot expand native code which was not given!")
                        (return . (:[]) . pretty) (cpCode cp)
  return $ header <+> cBraces body

pFunCode :: IfMap C -> CGen (Doc ())
pFunCode boundIfs = do
  cp     <- getCp
  info   <- getInfo
  layout <- layoutOpts <$> get
  st     <- get
  let scopedIfs  = boundIfs `M.union` cpIfs cp
      dictionary = M.mapWithKey ifToYAML scopedIfs
      genFun n tplArgs = if rInline
        then generator pFunCode <$> updatedBinds
        else generator (pFunCall inst) <$> updatedBinds
        where inst@(Ref rId rInline rBinds) = (cpRefs cp) M.! n
              updatedBinds  = mergeArgBinds tplArgs rBinds
              generator f b = spawnGen st rId (f $ updateOnBind scopedIfs b)
              
  let context = mkGingerContext (head info) dictionary $
        \n a -> (renderStrict . removeTrailingWhitespace . layoutPretty layout)
                <$> genFun n a
   
  code <- fromTemplate context (snd $ cpTpl cp)
  return $ pretty code

-- for bindings I am building a template macro first
pFunCall :: Instance -> IfMap C -> CGen (Doc ())
pFunCall parentInst parentIfs = do
  cp    <- getCp
  info  <- getInfo
  (retId,argsIds) <- ((fmap fst . ret) &&& (map fst . args)) <$> categorize (cpIfs cp)
  let bIfs  = updateOnBind parentIfs (refBinds parentInst)
  bArgs <- mapM (\i -> usage i $ bIfs M.! i) argsIds
  bRet  <- maybe (usage nullId $ TPort voidVar Nothing)
                (\i -> usage i $ bIfs M.! i) retId
  let template   = bRet <> pretty (cpName cp) <> sepArg bArgs
      dictionary = M.mapWithKey ifToYAML parentIfs
      context    = mkGingerContext (head info) dictionary (\_ _ -> Right "")
  text <- fromTemplate context (renderStrict $ layoutCompact template) :: CGen Text
  return $ pretty text
  where
    usage _ (TPort x u) = maybe (pVarBind x) (return . pretty) u 
    usage i (Param _ u) = return $ maybe (braces $ braces $ pretty i) pretty u 

----------------------------------------------------------------------

mergeArgBinds :: [Text] -> [Binding] -> Either Text [Binding]
mergeArgBinds targ binds =
  let (errs, nBind) = ( concatMap (either (:[]) (const [])) $ decodeBind targ
                      , concatMap (either (const []) (:[])) $ decodeBind targ )
  in case errs of
    [] -> Right $ toBind $ toMap nBind `M.union` toMap binds
    e  -> Left $ T.pack $ unlines e
  where
    toMap     = M.fromList . map (\(r,w,u) -> (r,(w,u)))
    toBind    = map (\(r,(w,u)) -> (r,w,u)) . M.toList
    decodeBind :: [Text] -> [Either String Binding]
    decodeBind = map ((\s-> either (\(pos,e) -> Left $ prettyPosWithSource pos s e)
                            Right (decode1 s)) . S.fromStrict . encodeUtf8) 
      

-- makeJsonMap refs cIfs = -- H.insert "instance" (toJSON $ sort refs)
--   (H.map toJSON cIfs) 

-- instance ToJSON (If C)  where
--   toJSON (Macro _ val) = val
--   toJSON i@(Variable _ OutArg t v) =
--     object ["name" .= pVOUse i, "type" .= toJSON t, "value" .= toJSON v]
--   toJSON (Variable n _ t v) =
--     object ["name" .= n, "type" .= toJSON t, "value" .= toJSON v]