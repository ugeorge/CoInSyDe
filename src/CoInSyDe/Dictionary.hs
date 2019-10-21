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

-- | Builds a type dictionaty from all @type@ child nodes of an input, using the
-- 'Target' API maker function.
mkTypeDict :: (Target l, FNode f)
           => f
           -> Dict (Type l)
mkTypeDict =  M.fromList . map mkType1 . children "type"
  where 
    mkType1 n = let name = n @!= "name"
                in  (name, mkType name n) 

-- | Builds a dictionary of functionals from a frontend root node.
mkFunDict :: (Target l, FNode f)
          => (Id -> f -> [TTm]) -- ^ Function for creating composite templates,
                                --   relevant to the target language.
          -> Dict (Type l)      -- ^ Existing dictionary of types
          -> Dict (Fun l)       -- ^ Existing (library) dictionary of templates
          -> f                  -- ^ Root node.
          -> Dict (Fun l)       -- ^ Dictionary of templates created from this root.
mkFunDict mkComposite typeLib patternLib root =
  M.fromList $ patterns ++ composites ++ natives ++ templates
  where
    patterns   = map (mkTmFun mkLibTempl)  $ root |= "pattern"
    composites = map (mkTmFun mkComposite) $ root |= "composite"
    templates  = map (mkTmFun mkTextTempl) $ root |= "template"
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
    mkTmFun mkTempl node = (name, TmFun name inline ports binds templ)
      where
        name   = node @!= "name"
        inline = ("call" `hasValue` "inline") node
        templ  = mkTempl name node
        ports  = mkPortDict name typeLib (node |= "port")
        binds  = mkBindings ports (node |= "instance")
    -----------------------------------       
    mkLibTempl  name node = funTempl $ patternLib ! (node @!= "type")
    mkTextTempl name node = textToTm (unpack name) (txtContent node)
    -----------------------------------       

-- | Makes a dictionary of ports from a list of port nodes.
mkPortDict :: (Target l, FNode f)
           => Id             -- ^ parent Id
           -> Dict (Type l)  -- ^ type dictionary
           -> [f]            -- ^ list of @port@ nodes
           -> PortMap l
mkPortDict parentId typeLib = M.fromList . map mkPort
  where
    mkPort n = let name = n @!= "name"
               in (name, Port name (mkGlue parentId typeLib n))

-- | Makes a bindings container. See definition of 'TmFun'.
mkBindings :: (Target l, FNode f)
           => PortMap l      -- ^ parent port map
           -> [f]            -- ^ list of @instance@ nodes
           -> Map Name (Id, PortMap l)
mkBindings parentPorts = M.fromList . map mkInst
  where
    mkInst n = let to    = n @!= "replace"
                   from  = n @!= "with"
                   binds = n |= "bind"
               in (to, (from, M.fromList $ map mkBind binds))
    mkBind n = let from = n @!= "port"
                   to   = n @!= "place"
                in (to, parentPorts ! from)
    
