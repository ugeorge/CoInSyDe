{-# LANGUAGE CPP #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoinSyDe.Libs
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains the main library management functions.
--
-- The anatomy of a library file is shown below. All fields are mandatory.
--
-- @path/to/library/<name>.<target>.<what>.xml@
--
-- * @<name>@ is a user-defined name.
-- 
-- * @<target>@ is a chain of dot-separated keywords narrowing down the scope of the
--   synthesizer from language family to a specific kind of implementation
--   e.g. @C.ucosii.mbox_comm@.
--
-- * @<what>@ is a tool-convenient way to tell CoInSyDe what the library file
--   holds. For now the only allowed identifiers are @type@ and @template@.
----------------------------------------------------------------------
module CoInSyDe.Libs where

#ifdef mingw32_HOST_OS
import System.FilePath.Windows
#else
import System.FilePath.Posix
#endif
import System.Directory
import Data.List

import CoInSyDe.Core
import CoInSyDe.Frontend

-- -- | First check if a file does not have multiple nodes with the same name identifier
-- noNameDuplicates what path root
--   | null dup  = root
--   | otherwise = error $ "XML parse error: File '"
--                 ++ path "' contains name duplicates " ++ show dup
--   where names   = allAttrOf what "name" root
--         (_,dup) = foldr scanDup ([],[]) names
--         scanDup n (ns,ds) = if n `elem` ns then (ns,n:ds) else (n:ns,ds) 

-- | Sanity checks a library, using the following rule: no two library files sharing
-- the same @<target>@ should have duplicate name entries.
noNameDuplicates path = do
  files <- listDirectory path
  let targets = nub $ map getTarget paths
      grouped = map (\t -> filter ((==t) . getTarget) files)
  where
    getTarget = snd . splitExtensions . dropExtension . takeBaseName
