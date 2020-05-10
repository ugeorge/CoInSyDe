{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoinSyDe.Internal.LibManage
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
-- configuration files. Check "CoInSyDe.Internal.Config" for more details.
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
-- load-time inter-dependencies, types and components are loaded in different orders:
--
-- * types are loaded from the most generic to the most specialized targets,
--   over-writing old entries with the same ID.
--
-- * components are loaded from the most specialized to the most generic targets. Old
--   entries with the same ID are being kept new ones are ignored.
------------------------------------------------------------------------
module CoInSyDe.Internal.LibManage (
  LibLoadList(..), buildLoadLists, shouldReloadLib,
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
import           Control.Monad (foldM,liftM,forM,when)
import           Data.Binary as Bin
import qualified Data.ByteString.Lazy as BL
import           Data.Function (on)
import qualified Data.HashMap.Strict as M
import           Data.List hiding (union)
import           Data.Text as T (Text,pack,unpack,splitOn)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Time.Clock
import           Data.Version
import           Text.Pretty.Simple

import           CoInSyDe.Core
import           CoInSyDe.Internal.Config
import           CoInSyDe.Internal.Map
import           CoInSyDe.Internal.YAML

data LibLoadList = LL { llWhat :: String, llPaths :: [FilePath] }
                 deriving (Show)

---------------------------------------------------------------------

-- | Builds the path lists with all the library files used in loading the types,
-- respectively component databases, according to the CoInSyDe library convention.
buildLoadLists :: SuiteConfig  -- ^ Workspace configuration
               -> ProjConfig   -- ^ Project configuration
               -> IO [LibLoadList]
               -- ^ load lists for @[types,natives,templates,patterns]@
buildLoadLists suite proj = withCurrentDirectory (workspaceRoot suite) $ do
  let target    = projTarget proj
      listDir d = listDirectory d >>= mapM (canonicalizePath . (d </>))
  tyPaths <- saneCheck target "type"     <$> mapM walkDir (typePaths suite)
  nvPaths <- saneCheck target "native"   <$> mapM walkDir (nativePaths suite)
  tpPaths <- saneCheck target "template" <$> mapM walkDir (templatePaths suite)
  return [ LL "type"     (tyPaths ++ projTypes proj)
         , LL "native"   (reverse $ nvPaths ++ projNative proj)
         , LL "template" (reverse tpPaths)
         , LL "pattern"  (projPatts proj)]
    where walkDir p = traverseDir p $ \filepath ->
            let fn = takeFileName filepath
            in any ($fn) [ isPrefixOf ".", isPrefixOf "#", isSuffixOf "#"
                         , isSuffixOf "~"]
                                            
saneCheck :: TargetId -> String -> [[FilePath]] -> [FilePath]
saneCheck target kind paths --  | all (checkDup []) sorted
  = concat sorted
  where
    filtered = map (filter (\p -> baseNameMatch p && targetMatch p)) paths
    sorted   = map (sortBy (compare `on` length . extractTarget)) filtered
    ------------------------------------------------------
    baseNameMatch p = extractBaseName p == ("coinlib-" ++ kind)
    targetMatch   p = extractTarget p `isPrefixOf` target

-- checkDup :: [TargetId] -> [FilePath] -> Bool
-- checkDup _ [] = True
-- checkDup ts (p:ps)
--   | extractTarget p `elem` ts = error $ "Duplicate target " ++ p
--   | otherwise                 = checkDup (extractTarget p : ts) ps

-- path/to/coinlib-type.c.ucosii.sem.yaml
-- + baseName: coinlib-type
-- + target  : ["c","ucosii","sem"]
extractBaseName = takeBaseName . dropExtensions
extractTarget   = init . stringToTargetid . tail . takeExtensions

-----------------------------------------------------------------------------

shouldReloadLib :: ProjConfig  -- ^ project options
                -> LibLoadList -- ^ what to load and list of files to load
                -> IO Bool
shouldReloadLib proj (LL what srcs) = do
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
loadLibs :: Target l
         => l                      -- ^ proxy
         -> SuiteConfig            -- ^ Suite configuration
         -> ProjConfig             -- ^ Project configuration
         -> [Bool]                 -- ^ see 'shouldReloadLib'
         -> [LibLoadList]          -- ^ See 'buildLoadLists'
         -> IO (MapH (Type l), MapH (Comp l))
loadLibs _ gconf conf [lty,lnv,ltm,lpt] [tys,nvs,tms,pts] = do
  tyLib <- if lty
           then parseYDocs makeTyLibs M.empty tys
           else loadObj conf (llWhat tys)
  nvLib <- if lty || lnv
           then parseYDocs (makeCpLibs tyLib) M.empty nvs
           else loadObj conf (llWhat nvs)
  tmLib <- if lty || ltm
           then parseYDocs (makeCpLibs tyLib) M.empty tms
           else loadObj conf (llWhat tms)
  ptLib <- if lty || ltm || lpt
           then parseYDocs (makeCpLibs tyLib) tmLib pts
           else loadObj conf (llWhat pts)
  return (tyLib, nvLib `M.union` ptLib)
  where
    load parser what l p = readYDoc p >>= \doc -> case yamlMeta doc of
      Nothing -> parseYDoc doc $ parser what l doc
      Just m  -> do docVer <- fromMeta (queryNode["coinsyde-version"]) m
                    maybe (return ()) (compareVersions p) docVer
                    parseYDoc doc $ parser what l doc
    parseYDocs parser lib (LL what paths) = do
      nlib <- foldM (load parser what) lib paths
      dumpObj conf what nlib
      return nlib
    compareVersions p t =
      let docMajor  = take 2 $ versionBranch $ read (T.unpack t)
          coinMajor = take 2 $ versionBranch $ coinsydeVersion gconf
      in do when (docMajor < coinMajor) $ error $
              "Version mismatch in file " ++ show p ++ ". Please update "
              ++ "the file to match the API of the current CoInSyDe version"
            when (docMajor > coinMajor) $ error $
              "Version mismatch in file " ++ show p ++ ". Consider updating "
              ++ "your tool to match the specification of the YAML input."

makeTyLibs :: Target l => String -> MapH (Type l) -> YDoc -> YParse (MapH (Type l))
makeTyLibs what lib doc  = (yamlRoot doc |= pack what) >>= foldM load lib
  where load lib node = do
          name  <- node @! "name"
          entry <- mkType lib node
          info  <- mkInfo doc node
          return $ dbUpdate Replace name entry info lib 

makeCpLibs :: Target l => MapH (Type l) -> String -> MapH (Comp l) -> YDoc
           -> YParse (MapH (Comp l))
makeCpLibs tyLib what lib doc  = (yamlRoot doc |= pack what) >>= foldM load lib
  where load lib node = do
          name <- node @! "name"
          info <- mkInfo doc node
          (policy,entry) <- case what of
            "native"   -> (,) Keep    <$> mkNative tyLib node
            "template" -> (,) Keep    <$> mkTemplate (yamlPath doc) tyLib node
            "pattern"  -> (,) Replace <$> mkPattern tyLib lib node
          return $ dbUpdate policy name entry info lib 

-- | Gets parser information about a node
mkInfo :: YDoc -> YMap -> YParse Info
mkInfo doc node =  do
  let (line,column) = getLineAndColumn node
  comment <- node @? "comment" @= ""
  return $ Info (yamlPath doc) line column comment

-- | Loads a binary object specified by the @what@ string.
loadObj :: Binary l => ProjConfig -> String -> IO l
loadObj conf what = liftM Bin.decode $ BL.readFile (projObjPath what conf) 

-- | Dumps a binary object specified by the @what@ string.
dumpObj :: Binary l => ProjConfig -> String -> l -> IO ()
dumpObj conf what = BL.writeFile (projObjPath what conf) . Bin.encode

-- | Dumps a pretty-printed library in a text file for debugging purposes.
dbgPrettyLib :: Show a => ProjConfig -> String -> a -> IO () 
dbgPrettyLib conf what
  = BL.writeFile (projObjPath what conf ++ ".dbg") . encodeUtf8
    . pShowOpt defaultOutputOptionsNoColor { outputOptionsIndentAmount = 2 }

-------------------------------------------------------

-- | Traverse from 'top' directory and return all the files by
-- filtering out the 'exclude' predicate.
traverseDir :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
traverseDir top exclude = do
  ds <- listDirectory top
  paths <- forM (filter (not . exclude) ds) $ \d -> do
    let path = top </> d
    isSymbolic  <- pathIsSymbolicLink path
    isDirectory <- doesDirectoryExist path
    if not isSymbolic && isDirectory
      then traverseDir path exclude
      else return [path]
  return (concat paths)
