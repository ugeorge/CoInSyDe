module CoinSyDe.C.XML where

import CoinSyDe.C.Core
import CoinSyDe.C.CLib
import CoinSyDe.C.Template

import Data.Maybe
import Data.List
import Text.XML.Light
import Data.Map.Lazy as M hiding (map,filter,take)



buildTypes :: Element -> Map TypeId Ty
buildTypes root = M.fromList $ standard ++ custom
  where
    tNodes   = children "type" root
    standard = map mkStdEntry $ filter ("class" `hasValue` "standard") tNodes
    custom   = []
    -----------------------------------
    mkStdEntry node = (getAttr "name" node, tyLib ! (getAttr "alias" node))

buildStateVars :: Map TypeId Ty -> Element -> Map VarId Var
buildStateVars types root = M.fromList states
  where
    pNodes = let build n = map ((,) (getAttr "name" n)) (children "port" n)
             in concatMap build (children "pattern" root)
                ++ concatMap build (children "composite" root)
    states = map mkVars $ filter (hasValue "dir" "state" . snd) pNodes
    -----------------------------------
    mkVars (pName, n) = let vName = pName ++ "_" ++ getAttr "name" n
                            vType = types ! getAttr "type" n
                            vVal  = findAttr (qn "value") n
                            vDir  = mkPortTy $ getAttr "dir" n
                        in (vName, Var vName vDir vType vVal)
                           
buildFuncs :: Map TypeId Ty -> Element -> Map FunId Fun
buildFuncs types root = M.fromList $ patterns ++ composites ++ native ++ templates
  where
    patterns   = map (mkPatFunc libTempl) $ children "pattern" root
    composites = map (mkPatFunc stdTempl) $ children "composite" root
    templates  = map (mkPatFunc txtTempl) $ children "template" root
    native     = map mkNativeCode $ children "code" root
    -----------------------------------
    mkNativeCode :: Element -> (FunId, Fun)
    mkNativeCode node = (name, CFun name ports file)
      where
        name   = getAttr "name" node
        ports  = M.fromList $ map (mkVars name) $ children "port" node
        file   = getAttr "call" node
    -----------------------------------
    libTempl name node = patLib ! getAttr "type" node
    txtTempl name node = textToTemplate name (strContent node)
    stdTempl name node = map FunTTm lBinds
      where lBinds = map show $ take (length $ children "binding" node) [1..]
    -----------------------------------
    mkPatFunc :: (FunId -> Element -> [TTm]) -> Element -> (FunId, Fun)          
    mkPatFunc templF node = (name, TmFun name inline ports binds templ)
      where
        name   = getAttr "name" node
        inline = getAttr "call" node == "inline"
        templ  = templF name node
        ports  = M.fromList $ map (mkVars name) $ children "port" node
        binds  = M.fromList $ map (mkBinds ports) $ children "binding" node
    ----------------------------
    mkVars p n = let dir   = mkPortTy $ getAttr "dir" n
                     name  = getAttr "name" n
                     vName | isState dir = p ++ "_" ++ name
                           | otherwise   = name
                     vType = types ! getAttr "type" n
                     vVal  = findAttr (qn "value") n
                 in (name, Var vName dir vType vVal)
    mkBinds p n = let to   = getAttr "to" n
                      what = getAttr "what" n
                      pNodes = children "parameter" n
                  in (to, (what, M.fromList $ map (mkPar p) pNodes))
    mkPar p n = let pName = getAttr "name" n
                    pVal  = getAttr "value" n
                in (pName, p ! pVal)


-----------------
-- Helpers
-----------------
qn id = blank_name {qName=id}

hasValue attr val node
  | found == (Just val) = True
  | otherwise           = False
  where found = findAttr (qn attr) node
  
getAttr attr node = fromJust $ findAttr (qn attr) node

children str = findChildren (qn str)
