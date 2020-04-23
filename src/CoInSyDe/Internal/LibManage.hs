{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoinSyDe.LibManage
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
-- @path\/to\/library/coinlib-\<what\>.\<target\>.\<ext\>@
--
-- * @\<target\>@ is a chain of dot-separated keywords narrowing down the scope of the
--   synthesizer from language family to a specific kind of implementation
--   e.g. @C.ucosii.mbox_comm@.
--
-- * @\<what\>@ is a tool-convenient, yet mandatory way to tell CoInSyDe what the
--   library file holds. Allowed keywords are @template@, @type@, @native@, @pattern@
--
-- * @\<ext\>@ is an aritrary extension and will not be checked
--
-- CoInSyDe libraries for each project in a workspace are pointed out by their
-- configuration files. Check "CoInSyDe.Interna.Config" for more details.
--
-- * the libraries are listed as @stdlib:usrlib1:usrlib2:...@, ranked by a
-- /earlier is older/ (e.g. stdlib) and /later is newer/ (e.g. usrlib2) policy.
--
-- * implementation templates from a generic or /older/ library are overwritten by
--   templates having the same \"name\" identifier from a /newer/ library.
--
-- * no name duplicates are allowed for the same target within the same library.
--
-- * implementation templates for a more generic target (e.g. @.C@) are overwritten by
--   templates having the same \"name\" identifier for a more specialized target
--   (e.g. @.C.ucosii@).
--
-- Libraries are loaded based on their hierarchy on the first run, for each target,
-- and then are dumped to a @objdump@ file. Each subsequent run will load the
-- pre-built libraries from that @objdump@, unless forced to rebuild libraries. Due to
-- load-time inter-dependencies (or the lack of them), types and components are loaded
-- in different orders:
--
-- * types are loaded from the most generic to the most specialized targets,
--   over-writing old entries with the same ID.
--
-- * components are loaded from the most specialized to the most generic targets. Old
--   entries with the same ID are being kept new ones are ignored.
------------------------------------------------------------------------
module CoInSyDe.Internal.LibManage (
  buildLoadLists, shouldReloadLib,
  loadLibs, loadObj, dumpObj, dbgPrettyLib
  )
where

#ifdef mingw32_HOST_OS
import           System.FilePath.Windows
#else
import           System.FilePath.Posix
#endif
import           System.Directory
import           System.Exit

import           Control.Exception
import           Control.Monad (foldM,liftM)
import           Data.Binary as Bin
import qualified Data.ByteString.Lazy as BL
import           Data.Function (on)
import           Data.List
import           Data.Text as T (Text,pack,splitOn)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Time.Clock
import           Data.YAML
import           Text.Pretty.Simple

import           CoInSyDe.Frontend
import           CoInSyDe.Internal.Config
import           CoInSyDe.Internal.Map
import           CoInSyDe.Internal.YAML

---------------------------------------------------------------------

-- | Builds the path lists with all the library files used in loading the types,
-- respectively component databases, according to the CoInSyDe library convention.
buildLoadLists :: SuiteConfig  -- ^ Workspace configuration
               -> ProjConfig   -- ^ Project configuration
               -> IO [(String, [FilePath])]
               -- ^ load lists for @[types,natives,templates,patterns]@
buildLoadLists suite proj = withCurrentDirectory (workspaceRoot suite) $ do
  let target   = projTarget proj
  tyPaths <- saneCheck target "type"     <$> mapM listDirectory (typePaths suite)
  nvPaths <- saneCheck target "native"   <$> mapM listDirectory (nativePaths suite)
  tpPaths <- saneCheck target "template" <$> mapM listDirectory (templatePaths suite)
  return [ ("type",     reverse $ tyPaths ++ projTypes proj)
         , ("native",   reverse $ nvPaths ++ projNative proj)
         , ("template", reverse tpPaths)
         , ("pattern",  reverse $ projPatts proj)]

saneCheck :: TargetId -> String -> [[FilePath]] -> [FilePath]
saneCheck target kind paths | all (checkDup []) sorted = concat sorted
  where
    filtered = map (filter (\p -> baseNameMatch p && targetMatch p)) paths
    sorted   = map (sortBy (compare `on` length . extractTarget)) filtered
    ------------------------------------------------------
    baseNameMatch p = extractBaseName p == ("coinlib-" ++ kind)
    targetMatch   p = extractTarget p `isPrefixOf` target

checkDup :: [TargetId] -> [FilePath] -> Bool
checkDup _ [] = True
checkDup ts (p:ps)
  | extractTarget p `elem` ts = error $ "Duplicate target " ++ p
  | otherwise                 = checkDup (extractTarget p : ts) ps

-- path/to/coinlib-type.c.ucosii.sem.yaml
-- + baseName: coinlib-type
-- + target  : ["c","ucosii","sem"]
extractBaseName = takeBaseName . dropExtensions
extractTarget   = init . stringToTargetid . tail . takeExtensions

-----------------------------------------------------------------------------

shouldReloadLib :: ProjConfig          -- ^ project options
                -> String              -- ^ what to load
                -> [FilePath]          -- ^ list of files to load
                -> IO Bool
shouldReloadLib proj what srcs = do
  let objFile = projObjPath what proj
  doesFileExist objFile >>= \isObjFile ->
    if not isObjFile
      then return True
      else getModificationTime objFile >>=
           \objTime -> any (> objTime) <$> mapM getModificationTime srcs

-- | Reads the content of the given files into a database of target-relevant types.
--
-- According to the library conventions, types are loaded from least to most specific
-- library files, the last one being the project file itself. New entries override old
-- ones with the same ID.
loadLibs :: FromYAML l
         => String
         -> [FilePath]  -- ^ ordered list of load paths. See 'buildLoadLists'
         -> IO (MapH l)
loadLibs what = foldM loadLib emptyMap
  where
    loadLib lib path = readYDoc path >>= \doc -> parseYDoc doc $ makeLib lib doc
    makeLib lib doc  = foldM load lib =<< getChildren (pack what) (yamlRoot doc)
      where
        load lib node = do
          info  <- mkInfo doc node
          name  <- withMap ("Input document for '" ++ what ++ "'") (.: "name") node
          entry <- parseYAML node
          return $ dictUpdate Keep name entry info lib 

-- | Gets parser information about a node
mkInfo :: YDoc -> YNode -> Parser Info
mkInfo doc node = do
  pos <- getPos node
  return $ Info (yamlPath doc) (posLine pos) (posColumn pos)

loadObj :: Binary l => ProjConfig -> String -> IO l
loadObj conf what = liftM Bin.decode $ BL.readFile (projObjPath what conf) 

dumpObj :: Binary l => ProjConfig -> String -> l -> IO ()
dumpObj conf what = BL.writeFile (projObjPath what conf) . Bin.encode

dbgPrettyLib :: Show a => ProjConfig -> String -> a -> IO () 
dbgPrettyLib conf what
  = BL.writeFile (projObjPath what conf ++ ".dbg") . encodeUtf8
    . pShowOpt defaultOutputOptionsNoColor { outputOptionsIndentAmount = 2 }
