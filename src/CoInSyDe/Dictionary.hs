----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Dictionary
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains methods for building common dictionaries for
-- all target backends.
----------------------------------------------------------------------
module CoInSyDe.Dictionary where

import Data.Map as M hiding (map,fold)
import Data.Text (unpack)

import CoInSyDe.Core
import CoInSyDe.Frontend
import CoInSyDe.TTmParse

-- | Builds a dictionary of functionals from a frontend root node.
mkTmDict :: (FNode f, Ty t)
         => (Id -> f -> [TTm]) -- ^ Function for creating composite templates,
                               --   relevant to the target language.
         -> Dict t             -- ^ Existing dictionary of types
         -> Dict Fun           -- ^ Existing (library) dictionary of templates
         -> f                  -- ^ Root node.
         -> Dict Fun           -- ^ Dictionary of templates created from this root.
mkTmDict mkComposite typeLib patternLib root =
  M.fromList $ patterns ++ composites ++ natives ++ templates
  where
    patterns   = []--map (mkTmFun libTempl)    $ root |= "pattern"
    composites = []--map (mkTmFun mkComposite) $ root |= "composite"
    templates  = []--map (mkTmFun txtTempl)    $ root |= "template"
    natives    = map mkNvFun               $ root |= "native"
    -----------------------------------
    mkNvFun node = (name, NvFun name ports code)
      where
        name  = node @!= "name" 
        ports = mkPortDict name typeLib $ node |= "port"
        code  = getCode $ node @= "fromFile"
        getCode Nothing  = Right $ txtContent node
        getCode (Just a) = Left $ unpack a
    -----------------------------------
--     libTempl name node = patLib ! getAttr "type" node
--     txtTempl name node = textToTm name (strContent node)
--     -----------------------------------       
--     mkPatFunc templF node = (name, TmFun name inline ports binds templ)
--       where
--         name   = getAttr "name" node
--         inline = getAttr "call" node == "inline"
--         templ  = templF name node
--         ports  = M.fromList $ map (mkVars name) $ children "port" node
--         binds  = M.fromList $ map (mkBinds ports) $ children "binding" node
--     ----------------------------
--     mkVars p n = let dir   = mkPortTy $ getAttr "dir" n
--                      name  = getAttr "name" n
--                      vName | isState dir = p ++ "_" ++ name
--                            | otherwise   = name
--                      vType = types ! getAttr "type" n
--                      vVal  = findAttr (qn "value") n
--                  in (name, Var vName dir vType vVal)
--     mkBinds p n = let to   = getAttr "to" n
--                       what = getAttr "what" n
--                       pNodes = children "parameter" n
--                   in (to, (what, M.fromList $ map (mkPar p) pNodes))
--     mkPar p n = let pName = getAttr "name" n
--                     pVal  = getAttr "value" n
--                 in (pName, p ! pVal)

-- | Makes a dictionary of ports from a list of port nodes.
mkPortDict :: (FNode f, Ty t, Glue p, Show p)
           => Id       -- ^ parent Id
           -> Dict t   -- ^ type dictionary
           -> [f]      -- ^ list of port nodes
           -> PortMap p
mkPortDict parentId typeLib = M.fromList . map mkPort
 where
   mkPort n = let name = n @!= "name"
              in (name, Port name (mkGlue parentId typeLib n))
