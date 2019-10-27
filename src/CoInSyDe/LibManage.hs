{-# LANGUAGE CPP #-}
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
  loadLibObj, dumpLibObj
  )
where

#ifdef mingw32_HOST_OS
import System.FilePath.Windows
#else
import System.FilePath.Posix
#endif
import System.Directory
import Control.Monad (foldM)
import Data.List
import Data.Text (Text,pack,unpack,splitOn)
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import qualified Data.ByteString as B

import CoInSyDe.Core
import CoInSyDe.Frontend
import CoInSyDe.Frontend.XML ()
import Text.XML.Light as XML (Element)

-- | Returns a path-wrapped frontend root node (e.g. XML root element). Should not be
-- used alone, but within a @case@ block to avoid ambiguous instances.
readLibDoc :: FNode f => FilePath -> IO f
readLibDoc path = B.readFile path >>= return . readDoc path

buildLoadLists :: String
               -> String
               -> IO ([FilePath],[FilePath])
buildLoadLists target ldLibraryPath = do
  let libs = splitSearchPath ldLibraryPath
  allPaths <- mapM listDirectory libs
  let -- select only the paths compatible to the target
      trgLibs  = (map . filter) (isOf target . getTrg) allPaths
      -- separate into two groups by what they contain: types and components
      typeLibs = (map . filter) ((==".type") . getType) trgLibs
      compLibs = (map . filter) ((`elem`[".template",".native"]) . getType) trgLibs
      -- sort the two groups based on their respective loading rules
      ordTyLib = (map . sortOn) (length . getTrg) typeLibs
      ordCpLib = reverse $ map (reverse . sortOn (length . getTrg)) typeLibs
      -- further group based on "same target" for sanity checks. Flatten outer
      -- (by-library) grouping
      allTrgs  = nub $ (concatMap . concatMap) getTrg trgLibs
      grpTyLib = concatMap ((map . map) (\t -> filter (==t) allTrgs)) ordTyLib
      grpCpLib = concatMap ((map . map) (\t -> filter (==t) allTrgs)) ordCpLib
  --  extract all IDs from the libraries
  tyNames <- (mapM . mapM) (pathToNameList ["type"]) grpTyLib
  cpNames <- (mapM . mapM) (pathToNameList ["template","native"]) grpCpLib
  -- do sanity checks over all extracted names (throws error). Keep only
  -- paths. Flatten the path lists.
  let tyFinal = concatMap noNameDuplicates $ zip grpTyLib tyNames
      cpFinal = concatMap noNameDuplicates $ zip grpCpLib cpNames
  return (tyFinal,cpFinal)
  where
    getTrg  = fst . trgAndType
    getType = snd . trgAndType

-- "path/to/name.C.ucosii.type.xml" -> (".C.ucosii",".type")
trgAndType = splitExtensions . snd . splitExtensions . takeBaseName

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

-- TODO: update when new frontends added
pathToNameList what path = 
  case takeExtension path of
    ".xml" -> do
      xml <- readLibDoc path :: IO XML.Element
      return $ map (@!"name") $ childrenOf what xml
    _ -> return []

---------------------------------------------------------------------

loadTypeLibs :: Target l
             => [FilePath]
             -> IO (LdHistory (Type l))
loadTypeLibs = foldM loadLib emptyHistory
  where
    loadLib lib path  =
      case takeExtension path of
        ".xml" -> do
          xml <- readLibDoc path :: IO XML.Element
          return $ mkTypeLib lib path xml
        _ -> putStrLn ("INFO: ignoring file " ++ show path) >> return lib

loadCompLibs :: Target l
             => Dict (Type l)
             -> [FilePath]
             -> IO (LdHistory (Comp l))
loadCompLibs typeLib = foldM loadLib emptyHistory
  where
    mkLib path = case snd (trgAndType path) of
      ".template" -> mkTemplateLib path
      ".native"   -> mkNativeLib path typeLib
      x -> error $ "Fatal error: trying to load library of type " ++ show x
    loadLib lib path =
      case takeExtension path of
        ".xml" -> do
          xml <- readLibDoc path :: IO XML.Element
          return $ mkLib path lib xml
        _ -> putStrLn ("INFO: ignoring file " ++ show path) >> return lib

loadProject :: Target l
            => l
            -> Dict (Type l)
            -> LdHistory (Comp l)
            -> FilePath
            -> IO (LdHistory (Comp l))
loadProject lang typeLib compLibH path = 
  case takeExtension path of
    ".xml" -> do
      xml <- readLibDoc path :: IO XML.Element
      let plibH = mkPatternLib path typeLib compLibH xml
          clibH = mkCompositeLib lang path typeLib plibH xml
      return clibH
    _ -> error $ "Cannot load project file " ++ show path

---------------------------------------------------------------------


-- | Dumps the content of a built dictionary into an @objdump@ file. TODO: dump to binary.
dumpLibObj :: Show a
           => String    -- ^ project name/filename
           -> String    -- ^ what is being dumped
           -> FilePath  -- ^ dump directory
           -> a -> IO () 
dumpLibObj target what path lib = do
  let filepath = path </> target <.> what <.> "objdump"  
  B.writeFile filepath $ encodeUtf8 $ pack $ show lib

-- | Loads the content of a dictionary from an @objdump@ file. TODO: load from binary.
loadLibObj :: Read b
           => String    -- ^ project name/filename
           -> String    -- ^ what is being loaded
           -> FilePath  -- ^ load directory
           -> IO b
loadLibObj target what path = do
  let filepath = path </> target <.> what <.> "objdump"
  lib <- B.readFile filepath
  return $ read $ unpack $ decodeUtf8 lib  

