module CoInSyDe.Backend.C.Pretty where

import CoInSyDe.Core
import CoInSyDe.Backend.C.Core
import CoInSyDe.Backend.C.Proj


import Data.Text (pack)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy hiding (map,filter)
import Data.Text.Prettyprint.Doc 
import Data.List (intercalate)

type CDoc = Doc ()

-------------------------------
-- requirements generator
-------------------------------

pInclude (Include file) = pretty "#include" <+> dquotes (pretty file) <> semi

-------------------------------
-- type declarations generator
-------------------------------

-- pretty printer helper
sepDefLine c x = (braces . nest 4 . sep)
                 ([softline'] ++ punctuate c x ++ [softline'])

-- OBS: no PrimTy allowed
pTyDecl :: Type C -> CDoc

pTyDecl (EnumTy nm vals) = 
  pretty "typedef enum"
  <+> (sepDefLine comma . map initVar) vals
  <+> pretty nm <> semi
  where initVar (a, Nothing) = pretty a
        initVar (a, Just v)  = pretty a <> equals <> pretty v

pTyDecl (BoolTy nm true false) =
  pTyDecl $ EnumTy nm [(true, Just (pack "1")), (false, Just (pack "0"))]

pTyDecl (Struct nm types) =
  pretty "struct"
  <+> pretty nm
  <+> (sepDefLine semi . map initType) (M.toList types) <> semi
  where initType (n, (ty, _)) = pretty n <+> pretty (tyName ty) 

pTyDecl t = error $ "Code generation for type " ++ show t ++ " is not supported!"

-------------------------------
-- variable code generators
-------------------------------

-- genVarDecl (Var nm _ ty _) =
--   pretty (tyName ty) <+> pretty nm 
-- genVarInit (Var nm _ _ (Just vl)) =
--   pretty nm <+> equals <+> pretty vl  
-- genVarDeclInit (Var nm _ ty (Just vl)) =
--   pretty (tyName ty) <+> pretty nm <+> equals <+> pretty vl 
-- genVarDeclInit (Var nm _ ty Nothing)   =
--   pretty (tyName ty) <+> pretty nm 
