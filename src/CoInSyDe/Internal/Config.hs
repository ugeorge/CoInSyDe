{-# LANGUAGE CPP, OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoinSyDe.Internal.Config
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Methods for reading and setting up global, workspace and project
-- tool configurations. Most of them relate to setting up paths where
-- different files are found.
------------------------------------------------------------------------

module CoInSyDe.Internal.Config (
  TargetId,
  stringToTargetid, targetidToString,
  -- * Configurations
  SuiteConfig(..), ProjConfig(..),  
  projObjPath, loadConfig,
  -- * Utilities
  coinsydeDir, searchProjRootDir, makeDefaultConfig, makeWorkspace
  ) where

#ifdef mingw32_HOST_OS
import System.FilePath.Windows
#else
import System.FilePath.Posix
#endif
import System.Directory
import System.Exit

import Paths_coinsyde (version,getDataFileName)
import Text.Pandoc.Definition (pandocTypesVersion)

import Control.Monad (when,unless)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text, splitOn, unpack, pack, intercalate)
import Data.Version (Version)
import Data.YAML
import Data.YAML.Event (untagged)

import CoInSyDe.Internal.YAML

type TargetId = [Text]

stringToTargetid :: String -> TargetId
stringToTargetid = splitOn "." . pack

targetidToString :: TargetId -> String
targetidToString = unpack . intercalate "."

-- | Configuration for a project workspace
data SuiteConfig
  = SuiteConfig { suiteName       :: String     -- ^ name of workspace
                , coinsydeVersion :: Version    -- ^ CoInSyDe version
                , pandocVersion   :: Version    -- ^ CoInSyDe version
                , workspaceRoot   :: FilePath   -- ^ workspace root path
                , templatePaths   :: [FilePath] -- ^ paths to template definitions
                , typePaths       :: [FilePath] -- ^ paths to type definitions
                , nativePaths     :: [FilePath] -- ^ paths to native component defs
                , srcsRoot        :: FilePath   -- ^ unique root for all projects
                , targetLibPath   :: FilePath   -- ^ unique root for target libs
                , objDumpPath     :: FilePath   -- ^ where object files will be dumped
                , docsPath        :: FilePath   -- ^ generated documentation path
                , codePath        :: FilePath   -- ^ synthesized code path
                } deriving (Show)

defaultConfig =
  mapping [ "objDumpPath"   .= ("obj" :: Text)
          , "docsPath"      .= ("docs" :: Text)
          , "codePath"      .= ("gen" :: Text)
          , "templatePaths" .= (["templates"] :: [Text])
          , "typePaths"     .= (["types"] :: [Text])
          ]
makeDefaultDIrectories = do
  root <- coinsydeDir
  createDirectoryIfMissing True root
  mapM_ (createDirectoryIfMissing True . (</>) root) ["templates","types"]

loadSuiteConfig :: FilePath -> YDoc -> YDoc -> IO SuiteConfig
loadSuiteConfig root glob curr = mergeConf <$> appRoot (getConf glob) <*> getConf curr
  where
    getConf doc = either (die . prettyErr doc) return
                  $ parseEither (confParser $ snd $ yamlRoot doc)
    confParser = withMap "parent node" $ \ obj -> do
      name  <-      unpack <$> obj .:? "name" .!= ""
      dump  <-      unpack <$> obj .:? "objDumpPath" .!= ""
      docs  <-      unpack <$> obj .:? "docsPath" .!= ""
      code  <-      unpack <$> obj .:? "codePath" .!= ""
      proj  <-      unpack <$> obj .:? "projRoot" .!= "."
      trgl  <-      unpack <$> obj .:? "targetLibPath" .!= "."
      temp  <- fmap unpack <$> obj .:? "templatePaths" .!= []
      typ   <- fmap unpack <$> obj .:? "typePaths" .!= []
      natv  <- fmap unpack <$> obj .:? "nativePaths" .!= []
      return $ SuiteConfig { suiteName = name
                           , coinsydeVersion = version
                           , pandocVersion = pandocTypesVersion
                           , workspaceRoot = root
                           , templatePaths = temp
                           , typePaths = typ
                           , nativePaths = natv
                           , srcsRoot = proj
                           , targetLibPath = trgl
                           , objDumpPath = dump
                           , docsPath = docs
                           , codePath = code }
    mergeConf glob curr =
      curr { templatePaths = ovExpand (templatePaths glob) (templatePaths curr)
           , typePaths     = ovExpand (typePaths glob) (typePaths curr)
           , nativePaths   = ovExpand (nativePaths glob) (nativePaths curr)
           , targetLibPath = override (targetLibPath glob) (targetLibPath curr)
           , objDumpPath   = override (objDumpPath glob) (objDumpPath curr)
           , docsPath      = override (docsPath glob) (docsPath curr)
           , codePath      = override (codePath glob) (codePath curr)
           }
    appRoot conf = updateConf <$> coinsydeDir <*> conf
      where updateConf root conf
              = conf { templatePaths = map ((</>) root) (templatePaths conf)
                     , typePaths     = map ((</>) root) (typePaths conf)
                     , nativePaths   = map ((</>) root) (nativePaths conf)
                     }

override  a "" = a
override  _ b  = b
ovExpand a [] = a
ovExpand a b
  | "$GLOBAL" `elem` b = let (xs,ys) = break (=="$GLOBAL") b
                         in  xs ++ a ++ tail ys
  | otherwise = b

-- | Configuration for one project
data ProjConfig
  = ProjConfig { projName   :: String     -- ^ name of project
               , projTarget :: [Text]     -- ^ target platform (hierarchy split)
               , projPath   :: FilePath   -- ^ path for project sources
               , projTypes  :: [FilePath] -- ^ type sources
               , projPatts  :: [FilePath] -- ^ pattern component sources
               , projNative :: [FilePath] -- ^ native component sources
               , projTops   :: [Text]     -- ^ Top modules
               , projDocs   :: FilePath   -- ^ overrides location of docs dump
               , projCode   :: FilePath   -- ^ Path where code will be dumped
               , projObj    :: FilePath   -- ^ Path for objdump files
               } deriving (Show)
        
loadProjConfigs :: SuiteConfig -> YDoc -> IO [ProjConfig]      
loadProjConfigs suite doc = either (die . prettyErr doc) return
                            $ parseEither (projParser $ snd $ yamlRoot doc)
  where
    projParser = withMap "root" $
                 \r -> withSeq "projects" (mapM parseProj) =<< r .: "projects"
    parseProj (Mapping _ _ obj) = do
      name     <-               unpack <$> obj .: "name"
      target   <-          splitOn "." <$> obj .: "target"
      topMod   <-                          obj .: "top-modules"
      docs     <-          fmap unpack <$> obj .:? "docs-path"
      code     <-          fmap unpack <$> obj .:? "code-path"
      allMaybe <- (fmap . fmap) unpack <$> obj .:? "all-defs"
      typMaybe <- (fmap . fmap) unpack <$> obj .:? "type-defs"
      patMaybe <- (fmap . fmap) unpack <$> obj .:? "pattern-defs"
      natMaybe <- (fmap . fmap) unpack <$> obj .:? "native-defs"
      let projRoot = srcsRoot suite </> name
      return $ ProjConfig
        { projName   = name
        , projTarget = target
        , projPath   = srcsRoot suite </> name
        , projTypes  = map ((</>) projRoot) $ fromMaybe (fromJust allMaybe) typMaybe
        , projPatts  = map ((</>) projRoot) $ fromMaybe (fromJust allMaybe) patMaybe
        , projNative = map ((</>) projRoot) $ fromMaybe (fromJust allMaybe) natMaybe
        , projTops   = topMod
        , projDocs   = fromMaybe (docsPath suite) docs </> name
        , projCode   = fromMaybe (codePath suite) code </> name
        , projObj    = objDumpPath suite
        } 
    parseProj _ = error "Configuration file ill-formed!"

-- | Helper to build object dump paths
projObjPath :: String -> ProjConfig -> FilePath
projObjPath what conf
  = (projObj conf) </> "lib-" ++ what <.> (projName conf) <.> ".objdump"

-----------------------------------------------------------------

-- | loads the configuration of the current workspace. Checks also the
-- global configuration and fills in the missing entries.
loadConfig :: FilePath -> IO (Maybe (SuiteConfig, [ProjConfig]))
loadConfig root = do
  globConfPath   <- (</> "conf.yaml") <$> coinsydeDir
  globConfExists <- doesFileExist $ globConfPath
  let currConfPath = root </> "coinsyde.yaml" 
  if (not globConfExists) then return Nothing
    else do
    globConf  <- readYDoc globConfPath
    currConf  <- readYDoc currConfPath
    suiteConf <- loadSuiteConfig root globConf currConf
    projConfs <- loadProjConfigs suiteConf currConf
    return $ Just (suiteConf, projConfs)

-- | User data directory. Global configuration is found here.
coinsydeDir :: IO FilePath
coinsydeDir = getAppUserDataDirectory "coinsyde"

-- | Returns the parent directory where a @coinsyde.yaml@ file is found.
searchProjRootDir :: FilePath -> IO (Maybe FilePath)
searchProjRootDir path = do
  currpath <- makeAbsolute path
  let pathIsDrive = isDrive currpath
      confFile    = currpath </> "coinsyde.yaml"
  confExists <- doesFileExist $ confFile
  if pathIsDrive
    then return Nothing
    else if not confExists
         then searchProjRootDir (takeDirectory currpath)
         else return (Just currpath)

-----------------------------------------------------------------

-- | Makes a default global configuration if none is already there. 
makeDefaultConfig :: IO ()
makeDefaultConfig = do
  globConfPath   <- (</> "conf.yaml") <$> coinsydeDir
  globConfExists <- doesFileExist $ globConfPath
  if globConfExists
    then putStrLn $ "A global configuration already exists.\n"
         ++"If you are sure you want to reset it, remove the file '"
         ++ globConfPath ++ "' and rerun this command."
    else do makeDefaultDIrectories
            writeYAML globConfPath defaultConfig

-- | Makes a workspace folder structure in the current directory with
-- a default configuration template.
makeWorkspace :: IO ()
makeWorkspace = do
  globConfPath   <- (</> "conf.yaml") <$> coinsydeDir
  globConfExists <- doesFileExist $ globConfPath
  unless globConfExists $ putStrLn "[WARNING] Did not find a global configuration! Consider creatinng one using the command-line tools (see the help menu)."
  createDirectoryIfMissing False "usrlib"
  createDirectoryIfMissing False "libs"
  createDirectoryIfMissing False "proj"
  defaultconf <- getDataFileName $ "resource" </> "default.yaml"
  copyFile defaultconf "coinsyde.yaml"
