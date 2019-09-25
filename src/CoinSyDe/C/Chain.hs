module CoinSyDe.C.Chain where

import CoinSyDe.C.Core
import CoinSyDe.C.CGen
import CoinSyDe.C.XML

import Data.Map (toList, (!))
import Data.Maybe
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderLazy)

generateCode xml =
  let types  = buildTypes xml
      states = buildStateVars types xml
      funcs  = buildFuncs types xml
  in vsep $ punctuate hardline
     [ makeGreeting
     , makeTypedefs types
     , makeStateDecls states
     , makeIncludeHs funcs
     , makeFunDecls funcs
     , makeFunDefs funcs
     , makeMainFun xml states funcs
     , emptyDoc
     ]

debugFuncs xml = funcs
  where
    types  = buildTypes xml
    funcs  = buildFuncs types xml


makeGreeting =
  let str = "// Generated with CoinSyDe : Code Synthesizer for System Design //\n\n"
            ++ "/*** Your Includes Go Here ***/"
  in pretty str

makeTypedefs types =
  pretty "// Custom Types" <> line <>
  (vsep $ mapFilter fromJust isJust $ map genTyDecl $ fromDict types)

makeIncludeHs funcs =
  pretty "// Native C function header files" <> line <>
  (vsep $ remDups [] $ mapFilter fromJust isJust $ map genHIncl $ fromDict funcs)
 
makeStateDecls states =
  pretty "// State variables" <> line <>
  (vsep $ map ((<>semi) . genVarDecl) $ fromDict states)
  
makeFunDecls funcs =
  pretty "// Function declarations" <> line <>
  (vsep $ mapFilter genFunDecl (not . isInline) $ fromDict funcs)
  
makeFunDefs funcs =
  pretty "// Function definitions" <> line <>
  (vsep $ mapFilter (genFunDef funcs) (not . isInline) $ fromDict funcs)

makeMainFun xml states funcs =
  pretty "// Main Function" <> line <>
  genMainFunc funcs (fromDict states) (funcs ! topName)
  where topName = getName $ filter (hasValue "type" "top") $ children "composite" xml
        getName [n] = getAttr "name" n
        genName _   = error $ "The input XML does not have _exactly_ one top module."



-------------------------------

fromDict = map snd . toList

mapFilter f g = map f . filter g

remDups :: (Eq a) => [a] -> [a] -> [a]
remDups _ [] = []
remDups list2 (x:xs)
    | (x `elem` list2) = remDups xs list2
    | otherwise = x : remDups xs (x:list2)


instance Eq (Doc a) where
  x == y = renderLazy (layoutCompact x) == renderLazy (layoutCompact y)
