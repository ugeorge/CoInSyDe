{-# LANGUAGE CPP, FlexibleContexts #-}
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
-- @path\/to\/library/\<name\>.\<target\>.\<what\>.\<ext\>@
--
-- * @\<name\>@ is a user-defined name.
-- 
-- * @\<target\>@ is a chain of dot-separated keywords narrowing down the scope of the
--   synthesizer from language family to a specific kind of implementation
--   e.g. @C.ucosii.mbox_comm@.
--
-- * @\<what\>@ is a tool-convenient way to tell CoInSyDe what the library file
--   holds. For now the only allowed identifiers are @type@ and @template@.
--
-- * @\<ext\>@ is the frontend file extension, supported by CoInSyDe. Check
-- * "CoInSyDe.Frontend" to see the supported file types.
--
-- CoInSyDe libraries are all pointed from the @$COINSYDE_PATH@ system
-- variable. Libraries are loaded using the following convention:
--
-- * the @$COINSYDE_PATH@ has a Posix-like format, i.e. libraries are listed as
--   @stdlib:usrlib1:usrlib2:...@, ranked by a /earlier is older/ (e.g. stdlib) and
--   /later is newer/ (e.g. usrlib2) policy.
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
module CoInSyDe.LibManage (
  buildLoadLists,
  loadTypeLibs,loadCompLibs,loadProject,
  loadLibObj, dumpLibObj,dumpPrettyLibObj
  )
where

#ifdef mingw32_HOST_OS
import System.FilePath.Windows
#else
import System.FilePath.Posix
#endif
import System.Directory
import System.Exit

import Control.Monad (foldM,liftM)
import Control.Exception
import Control.DeepSeq
import Data.List
import Data.Function (on)
import Data.Text as T (Text,pack,splitOn,isPrefixOf)
import qualified Data.Text.Lazy as TL (pack,unpack)
import Data.Text.Lazy.Encoding (encodeUtf8,decodeUtf8)
import Text.Pretty.Simple
import qualified Data.ByteString.Lazy as B

import CoInSyDe.Core
import CoInSyDe.Core.Dict
import CoInSyDe.Frontend
import CoInSyDe.Frontend.XML (XML)
import CoInSyDe.Frontend.JSON (JSON)
import CoInSyDe.Frontend.YAML (YAML)

-- | Returns a path-wrapped frontend root node (e.g. XML root element). Should not be
-- used alone, but within a @case@ block to avoid ambiguous instances.
readLibDoc :: FNode f => FilePath -> IO f
readLibDoc path = B.readFile path >>= return . readDoc

-- | Builds the path lists with all the library files used in loading the types,
-- respectively component databases, according to the CoInSyDe library convention.
buildLoadLists :: String  -- ^ target string
               -> String  -- ^ system variable containing the @PATH@s
               -> IO ([FilePath],[FilePath])
               -- ^ load lists for @(types,components)@
buildLoadLists target ldLibraryPath = do
  let libs = splitSearchPath ldLibraryPath
  allPaths <- mapM (liftM filterHidden . listDirAbs) libs
      -- select only the paths compatible to the target
  let trgLibs  = (map . filter) (isOf target . getTrg) allPaths
      -- separate into two groups by what they contain: types and components
      typeLibs = (map . filter) ((==".type") . getType) trgLibs
      compLibs = (map . filter) ((`elem`[".template",".native"]) . getType) trgLibs
      -- sort the two groups based on their respective loading rules
      ordTyLib = (map . sortOn) (length . getTrg) typeLibs
      ordCpLib = (reverse . map (reverse . sortOn (length . getTrg))) compLibs
      -- further group based on "same target" for sanity checks. Flatten outer
      -- (by-library) grouping
      grpTyLib = concatMap (groupBy ((==) `on` getTrg)) ordTyLib
      grpCpLib = concatMap (groupBy ((==) `on` getTrg)) ordCpLib
  --  extract all IDs from the libraries
  tyNames <- (mapM . mapM) (pathToNameList ["type"]) grpTyLib
  cpNames <- (mapM . mapM) (pathToNameList ["template","native"]) grpCpLib
  -- do sanity checks over all extracted names (throws error). Keep only
  -- paths. Flatten the path lists.
  let tyFinal = concatMap noNameDuplicates $ zip grpTyLib tyNames
      cpFinal = concatMap noNameDuplicates $ zip grpCpLib cpNames
  return (tyFinal,cpFinal)
  where
    -- TODO: better way to filter out hidden files
    filterHidden paths = [x | x <- paths, '~' `notElem` x, '#' `notElem` x]
    listDirAbs dir = listDirectory dir >>= mapM (canonicalizePath . (dir </>))
    getTrg  = fst . trgAndType
    getType = snd . trgAndType

-- "path/to/name.C.ucosii.type.xml" -> (".C.ucosii",".type")
trgAndType = splitExtension . snd . splitExtensions . takeBaseName

isOf target = all (`elem` mkTOrd target) . mkTOrd
  where mkTOrd = splitOn (pack ".") . pack

-- | Checks that there are no name duplicates from the extracted \"names\" fields from
-- the same library having the same target.
noNameDuplicates :: ([FilePath],[[Text]]) -> [FilePath]
noNameDuplicates (paths,names)
  | null dup  = paths
  | otherwise = error $ "Duplicate names "++ show dup ++ " in file(s) " ++ show paths
  where (_,dup) = foldr scanDup ([],[]) (concat names)
        scanDup n (ns,ds) = if n `elem` ns then (ns,n:ds) else (n:ns,ds) 

pathToNameList what path = 
  case takeExtension path of
    ".xml" -> do
      xml <- readLibDoc path :: IO XML
      return $ map (@!"name") $ childrenOf what xml
    ".json" -> do
      json <- readLibDoc path :: IO JSON
      return $ map (@!"name") $ childrenOf what json
    ".yaml" -> do
      yaml <- readLibDoc path :: IO YAML
      return $ map (@!"name") $ childrenOf what yaml
    _ -> return []

---------------------------------------------------------------------

forceM :: (Monad m, NFData a) => m a -> m a
forceM m = m >>= (return $!) . force

catchL f l p = Control.Exception.catch (forceM $ f l p) handler
  where
    handler :: SomeException -> IO a
    handler e = die $ "Exception when loading file " ++ show p ++ ":\n" ++ show e
    
-- | Reads the content of the given files into a database of target-relevant types.
--
-- According to the library conventions, types are loaded from least to most specific
-- library files, the last one being the project file itself. New entries override old
-- ones with the same ID.
loadTypeLibs :: Target l
             => FilePath    -- ^ main project file
             -> [FilePath]  -- ^ ordered list of load paths. See 'buildLoadLists'
             -> IO (Dict (Type l))
loadTypeLibs projF paths = foldM (catchL load) emptyDict paths >>=
                           \lib -> catchL load lib projF
  where
    load lib path = case takeExtension path of
      ".xml" -> do
        xml <- readLibDoc path :: IO XML
        return $ mkTypeLib lib path xml
      ".json" -> do
        json <- readLibDoc path :: IO JSON
        return $ mkTypeLib lib path json
      ".yaml" -> do
        yaml <- readLibDoc path :: IO YAML
        return $ mkTypeLib lib path yaml
      _ -> putStrLn ("INFO: ignoring file " ++ show path) >> return lib


-- | Reads the content of the given files into a database of target-relevant
-- components.
--
-- According to the library conventions, components are loaded from most to least
-- specific library files. \"New\" entries of existing components are completely
-- ignored (lazily).
loadCompLibs :: Target l
             => l             -- ^ proxy type to determine target language
             -> Dict (Type l) -- ^ fully-loaded type database
             -> [FilePath]    -- ^ ordered list of load paths. See 'buildLoadLists'
             -> IO (Dict (Comp l))
loadCompLibs lang typeLib = loadCompDb' lang Keep typeLib emptyDict

-- | Loads the final project components from the main project file, after the
-- databases have been succesfully built.
loadProject :: Target l
            => l             -- ^ proxy type to determine target language
            -> Dict (Type l) -- ^ fully-built type database
            -> Dict (Comp l) -- ^ fully-built component database
            -> FilePath      -- ^ path to main project file
            -> IO ([Id], Dict (Comp l))
            -- ^ list with top module IDs along with the new component database
loadProject lang tyLib cpLib path = do
  comps <- loadCompDb' lang Replace tyLib cpLib [path]
  let topModules = filter (T.isPrefixOf (pack "top_")) $ ids comps
  return (topModules, comps)

-- internal common implementation for loading components
loadCompDb' :: Target l
            => l             -- ^ proxy type to determine target language
            -> Policy        -- ^ update policy in case of name clashes
            -> Dict (Type l) -- ^ fully-loaded type database
            -> Dict (Comp l) -- ^ partially-loaded component database
            -> [FilePath]    -- ^ ordered list of load paths. See 'buildLoadLists'
            -> IO (Dict (Comp l))
loadCompDb' lang policy typeLib = foldM (catchL load)
  where 
    mkLib path = case snd (trgAndType path) of
        ".template" -> mkTemplateLib policy path 
        ".native"   -> mkNativeLib policy path typeLib
        ".composite"-> mkCompositeLib lang policy path typeLib
        _  -> \ l x -> flip (mkTemplateLib policy path) x
                       $ flip (mkNativeLib policy path typeLib) x
                       $ flip (mkPatternLib policy path typeLib) x
                       $ mkCompositeLib lang policy path typeLib l x
              
    load lib path = case takeExtension path of
      ".xml" -> do
        xml <- readLibDoc path :: IO XML
        return $ mkLib path lib xml
      ".json" -> do
        json <- readLibDoc path :: IO JSON
        return $ mkLib path lib json
      ".yaml" -> do
        yaml <- readLibDoc path :: IO YAML
        return $ mkLib path lib yaml 
      _ -> putStrLn ("INFO: ignoring file " ++ show path) >> return lib
    
---------------------------------------------------------------------

-- | Dumps the content of a built dictionary into an @objdump@ file. TODO: dump to
-- binary.
dumpLibObj :: Show a
           => FilePath  -- ^ dump directory
           -> a -> IO () 
dumpLibObj path = B.writeFile path . encodeUtf8 . TL.pack . show

-- | A prettier, human readable version of 'dumpLibObj'. The generated file __cannot
-- be loaded__. Adds a @.dbg@ suffix, not to be confused with the normal @objdump@
-- file.
dumpPrettyLibObj :: Show a
                 => FilePath  -- ^ dump directory
                 -> a -> IO () 
dumpPrettyLibObj path
  = B.writeFile (path ++ ".dbg") . encodeUtf8 . pShowOpt
    defaultOutputOptionsNoColor { outputOptionsIndentAmount = 2 }

-- | Loads the content of a dictionary from an @objdump@ file. TODO: load from binary.
loadLibObj :: Read b
           => FilePath  -- ^ load directory
           -> IO b
loadLibObj path = liftM (read . TL.unpack . decodeUtf8) (B.readFile path)  

