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
  PathAnd
  -- ** Constructors/Destructors
  , wrapPath, unwrapPath, getPath, dropPath
  -- ** Path methods
  , getLibs, groupByTarget 
  -- ** Sanity Checkers
  , uniqueNamesLib
  -- ** Loaders/Dumpers
  , readLibDoc, loadLibObj, dumpLibObj
  ) where

#ifdef mingw32_HOST_OS
import System.FilePath.Windows
#else
import System.FilePath.Posix
#endif
import System.Directory
import Data.List
import Data.Text (Text,pack,unpack)
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import qualified Data.ByteString as B
import CoInSyDe.Frontend

-- | Convenience wapper for associating any container with its origin file path.
newtype PathAnd a = PathAnd (FilePath,a) deriving (Show)

instance Functor PathAnd where
  fmap f (PathAnd (p,a)) = PathAnd (p, f a)

wrapPath p c = PathAnd (p,c)
unwrapPath (PathAnd (p,c)) = (p,c)
getPath    (PathAnd (p,_)) = p
dropPath   (PathAnd (_,c)) = c

-- | Pair-wise concatenates a list of path-wrappers into one wrapper. The resulting
-- path will have a posix-like format, i.e. @path1:path2:path3@.
catPA :: [PathAnd [a]] -> PathAnd [a]
catPA pcs = let (ps, cs) = unzip $ map unwrapPath pcs
            in PathAnd (intercalate ":" ps, concat cs)

-- | Checks that all files in a certain library do not contain identifier duplicates
-- for the same target.
uniqueNamesLib :: FNode f
               => String      -- ^ @\<what\>@ kind of library files are tested
               -> [PathAnd f] -- ^ all listed files found at a library path
               -> ()
uniqueNamesLib what = uniqueNames . catPA . (map . fmap) allNames
  where allNames = map (@!"name") . children what

-- | Checks that there are no name duplicates from the extracted \"names\" fields from
-- the same library having the same target.
uniqueNames :: PathAnd [Text] -> ()
uniqueNames (PathAnd (path,names))
  | null dup  = ()
  | otherwise = error $ "Duplicate names "++ show dup ++ " in file(s) " ++ path
  where (_,dup) = foldr scanDup ([],[]) names
        scanDup n (ns,ds) = if n `elem` ns then (ns,n:ds) else (n:ns,ds) 

-- | Groups all files belonging to a library in sub-lists based on their kind and
-- target.
groupByTarget :: String       -- ^ @\<what\>@ kind of library files are being grouped
              -> [FilePath]   -- ^ all files listed for a library path
              -> [[FilePath]]
groupByTarget what paths =  map (\t -> filter (isOf t) paths) targets
  where
    targets = nub $ map (fst . getTarget) paths
    -- "path/to/name.C.ucosii.type.xml" -> (".C.ucosii",".type")
    getTarget = splitExtensions . snd . splitExtensions . takeBaseName
    isOf trg = (==) (trg , '.' : what) . getTarget

-- | Returns a grouped list with all files from all libraries pointed by
-- @$COINSYDE_PATH@.
getLibs :: String -> IO [[FilePath]]
getLibs ldLibraryPath = do
  let libs = splitSearchPath ldLibraryPath
  mapM listDirectory libs

---------------------------------------------------------------------

-- | Returns a path-wrapped frontend root node (e.g. XML root element). 
readLibDoc :: FNode f => FilePath -> IO (PathAnd f)
readLibDoc path = B.readFile path >>= return . wrapPath path . readDoc path

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

