{-# LANGUAGE CPP, OverloadedStrings #-}

module CoInSyDe.Internal.Config (
  SuiteConfig, ProjConfig,
  loadConfig, makeDefaultConfig
  ) where

#ifdef mingw32_HOST_OS
import System.FilePath.Windows
#else
import System.FilePath.Posix
#endif
import System.Directory
import System.Exit

import Paths_coinsyde (version)

import Control.Monad (when)
import Data.Map.Strict (Map, fromList)
import Data.Text (Text, splitOn, unpack)
import Data.Vector (toList)
import Data.Version (Version)
import Data.YAML
import Data.YAML.Event (untagged)

import CoInSyDe.Internal.YAML

-- | Configuration for a project workspace
data SuiteConfig
  = SuiteConfig { suiteName       :: String     -- ^ name of workspace
                , coinsydeVersion :: Version    -- ^ CoInSyDe version
                , templatePaths   :: [FilePath] -- ^ paths to template definitions
                , typePaths       :: [FilePath] -- ^ paths to type definitions
                , nativePaths     :: [FilePath] -- ^ paths to native components
                , objDumpPath     :: FilePath   -- ^ where object files will be dumped
                , docsPath        :: FilePath   -- ^ generated documentation path
                , codePath        :: FilePath   -- ^ synthesized code path
                } deriving (Show)

defaultConfig =
  mapping [ "objDumpPath"   .= ("obj" :: Text)
          , "docsPath"      .= ("docs" :: Text)
          , "codePath"      .= ("gen" :: Text)
          , "templatePaths" .= (["templats"] :: [Text])
          , "typePaths"     .= (["types"] :: [Text])
          ]

loadSuiteConfig :: YamlDoc -> YamlDoc -> IO SuiteConfig
loadSuiteConfig glob curr = mergeConf <$> appRoot (getConf glob) <*> getConf curr
  where
    getConf doc = either (die . prettyErr doc) return
                  $ parseEither (confParser $ yamlRoot doc)
    confParser = withMap "parent node" $ \ obj -> do
      name  <-      unpack <$> obj .:? "name" .!= ""
      dump  <-      unpack <$> obj .:? "objDumpPath" .!= ""
      docs  <-      unpack <$> obj .:? "docsPath" .!= ""
      code  <-      unpack <$> obj .:? "codePath" .!= ""
      temp  <- fmap unpack <$> obj .:? "templatePaths" .!= []
      typ   <- fmap unpack <$> obj .:? "typePaths" .!= []
      natv  <- fmap unpack <$> obj .:? "nativePaths" .!= []
      return $ SuiteConfig name version temp typ natv dump docs code
    mergeConf glob curr =
      SuiteConfig { suiteName     = suiteName curr
                  , coinsydeVersion = version
                  , templatePaths = ovExpand (templatePaths glob) (templatePaths curr)
                  , typePaths     = ovExpand (typePaths glob) (typePaths curr)
                  , nativePaths   = ovExpand (nativePaths glob) (nativePaths curr)
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
               , projSrcs   :: [FilePath] -- ^ source file (main)
               , projTop    :: Text       -- ^ Top module
               , projDocs   :: FilePath   -- ^ overrides location of docs dump
               , projCode   :: FilePath   -- ^ Path where code will be dumped
               , projLibs   :: [FilePath] -- ^ Paths where target library files are
               } deriving (Show)
        
loadProjConfigs :: SuiteConfig -> YamlDoc -> IO [ProjConfig]      
loadProjConfigs suite doc = either (die . prettyErr doc) return
                            $ parseEither (projParser $ yamlRoot doc)
  where
    projParser = withMap "root" $
                 \r -> withSeq "projects" (mapM parseProj) =<< r .: "projects"
    parseProj (Mapping _ _ obj) = do
      name   <-               unpack  <$> obj .: "name"
      target <-          splitOn "."  <$> obj .: "target"
      source <-          fmap unpack  <$> obj .: "src-files"
      topMod <-                           obj .: "top-module"
      docs   <- ((</> name) . unpack <$$> obj .:? "docs-path") .!= docsPath suite
      code   <- ((</> name) . unpack <$$> obj .:? "code-path") .!= codePath suite
      libs   <-           map unpack  <$> obj .:? "lib-paths" .!= []
      return $ ProjConfig name target source topMod docs code libs
    parseProj _ = error "Configuration file ill-formed!"

-----------------------------------------------------------------

infixr 2 <$$>
(<$$>) = fmap . fmap

-----------------------------------------------------------------

-- | loads the configuration of the current workspace. Checks also the
-- global configuration and fills in the missing entries.
loadConfig :: FilePath -> IO (SuiteConfig, [ProjConfig])
loadConfig path = do
  globConfPath <- (</> "conf.yaml") <$> coinsydeDir
  currConfPath <- (</> "coinsyde.yaml") <$> searchProjRootDir path
  globConfExists <- doesFileExist $ globConfPath
  when (not globConfExists) $
    do putStrLn "No global configuration found!\nPlease run 'coinsyde --init-setup'" 
       exitFailure
  globConf  <- readYAML globConfPath
  currConf  <- readYAML currConfPath
  suiteConf <- loadSuiteConfig globConf currConf
  projConfs <- loadProjConfigs suiteConf currConf
  return (suiteConf, projConfs)

coinsydeDir :: IO FilePath
coinsydeDir = getAppUserDataDirectory "coinsyde"

searchProjRootDir :: FilePath -> IO FilePath
searchProjRootDir path = do
  currpath <- makeAbsolute path
  let pathIsDrive = isDrive currpath
      confFile    = currpath </> "coinsyde.yaml"
  confExists <- doesFileExist $ confFile
  if pathIsDrive
    then do putStrLn "No configuration file found! Aborting."
            exitFailure
    else if not confExists
         then searchProjRootDir (takeDirectory currpath)
         else return currpath

-----------------------------------------------------------------

-- | Makes a default global configuration
makeDefaultConfig :: IO ()
makeDefaultConfig = do
  globConfPath   <- (</> "conf.yaml") <$> coinsydeDir
  globConfExists <- doesFileExist $ globConfPath
  when globConfExists $
    do putStrLn $ "A global configuration already exists.\n"
         ++"If you are sure you want to reset it, remove the file '"
         ++ globConfPath ++ "' and rerun this command."
       exitSuccess
  createDirectoryIfMissing True =<< coinsydeDir
  writeYAML globConfPath defaultConfig
