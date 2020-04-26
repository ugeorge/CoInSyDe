{-# LANGUAGE CPP, OverloadedStrings #-}
module CLI where

import System.Console.GetOpt
import System.Environment
import System.Exit
#ifdef mingw32_HOST_OS
import System.FilePath.Windows
#else
import System.FilePath.Posix
#endif
import System.Directory
import System.Process
import System.IO
import Data.Time.Clock (getCurrentTime)


import Data.YAML (FromYAML)
import Data.Binary (Binary)
import Data.Proxy (Proxy)


-- import Control.Exception
import Control.Monad
import Data.Maybe
import Text.Pretty.Simple
-- import Data.List
-- import Data.Text (unpack)
-- import Data.Text.Prettyprint.Doc
-- import Data.Text.Prettyprint.Doc.Render.Text

import CoInSyDe.Target.C.Core (C(..))
import CoInSyDe.Internal.Config
import CoInSyDe.Internal.Docs
import CoInSyDe.Internal.LibManage
import CoInSyDe.Internal.Map
import CoInSyDe.Internal.YAML (YNode)

data CLIConfig = CLIConfig { force   :: Bool
                           , docs    :: Maybe String
                           , docopts :: [String]
                           } deriving (Show)

main = do
  -- load options and configurations
  (debug,cliconf,projs) <- getArgs >>= parse
  when debug $ print (cliconf,projs)
  mroot <- getCurrentDirectory >>= searchProjRootDir
  root  <- maybe (die $ "Not inside a workspace!\nRerun command with '--help' "
                   ++"to see how initialize a workspace") return mroot
  mconf <- loadConfig root
  (gconf,pconfs) <- maybe (die $ "No global configuration found!\n"
                           ++"Rerun command with '--global-init' to create one")
                    return mconf
  when debug $ pPrint gconf
  when debug $ pPrint pconfs

  setCurrentDirectory root
  
  -- select the user-demand projects
  let projConfigs = if null projs then pconfs
                    else filter (\p -> projName p `elem` projs) pconfs
  when debug $ print $ map projName projConfigs

  -- for each project proceed with the same algorithm
  forM_ projConfigs $ \conf -> do
    when debug $ pPrint $ "++++++++++ PROJECT " ++ projName conf ++ " ++++++++++"

    -- build the load lists
    libLists <- buildLoadLists gconf conf
    reload   <- mapM (fmap (|| force cliconf) . shouldReloadLib conf) libLists
    when debug $ pPrint $ zip reload libLists
    
    case head (projTarget conf) of
      "c" -> do
        (tyLib,cpLib) <- loadLibs C conf reload libLists
        when debug $ dbgPrettyLib conf "type" tyLib
        when debug $ dbgPrettyLib conf "pattern" cpLib

        unless (isNothing $ docs cliconf) $ dumpLibraryDoc cliconf gconf conf
          [ ("Types",      toDoc "ty" tyLib)
          , ("Components", toDoc "cp" cpLib) ]

        putStrLn "hallo"

      other -> putStrLn $ "[WARNING] CoInSyDe does not support target family "
               ++ show other ++ ". Ignoring project " ++ show (projName conf) ++ "!"


-- dumpLibraryDoc :: CliConfig -> SuiteConfig -> ProjConfig -> Maybe String
--                -> [(String, Blocks)] -> IO ()
dumpLibraryDoc cliconf gconf conf content = do
  createDirectoryIfMissing True (projDocs conf)
  time <- getCurrentTime
  let format   = fromJust $ docs cliconf
      otherOpt = docopts cliconf
      jsonFile = projDocs conf </> "Libs" <.> "json"
      docFile  = projDocs conf </> "Libs" <.> format
      pandoc   = makeDoc "Loaded Libraries" (projName conf)
                 (targetidToText $ projTarget conf) (show time)
                 (coinsydeVersion gconf) (workspaceRoot gconf) content
  writePandocJson jsonFile pandoc
  _ <- createProcess (proc "pandoc" $
                       otherOpt ++ ["--toc", "--from=json", "--to=" ++ format,
                                    "--output=" ++ docFile] ++ [jsonFile])
  return ()
       
  
--   -- check if libraries already loaded
--   let tyObjPathath = objPath cmd </> name cmd <.> target cmd <.> "type" <.> "objdump"
--       cpObjPathath = objPath cmd </> name cmd <.> target cmd <.> "component" <.> "objdump"
--   isTyObj <- doesFileExist tyObjPathath
--   isCpObj <- doesFileExist cpObjPathath

--   -- load types and components libraries only if forced/needed
--   (tyLib,cpLib) <- case isTyObj && isCpObj && not (force cmd) of
--     -- if already existing, load previously-built objdumps
--     True  -> do
--       tyObj <- loadLibObj tyObjPathath :: IO (MapH (Type C))
--       cpObj <- loadLibObj cpObjPathath :: IO (MapH (Comp C))
--       return (tyObj,cpObj)
--     -- if not, build libraries and put them in objdumps
--     False -> do
--       (tyLdList,cpLdList) <- buildLoadLists (target cmd) (libs cmd) 

--       -- double check the load paths are correct
--       printDebug cmd $ "$COINSYDE_PATH = " ++  show (libs cmd)
--       printDebug cmd $ "** Loading type libs: " ++  show tyLdList
--       printDebug cmd $ "** Loading component libs: " ++  show cpLdList

--       tyObj <- loadTypeLibs (infile cmd) tyLdList
--       cpObj <- loadCompLibs tyObj cpLdList

--       -- dump the built databases. If debug, then pretty-dump
--       createDirectoryIfMissing True (objPath cmd)
--       (if isDebug cmd then dumpPrettyLibObj else dumpLibObj) tyObjPathath tyObj
--       (if isDebug cmd then dumpPrettyLibObj else dumpLibObj) cpObjPathath cpObj
        
--       -- return for further use
--       return (tyObj,cpObj)
      
--   -- finally with all types and template libraries, load the C project
--   (topIds,projDb) <- loadProject tyLib cpLib (infile cmd)
--   printDebug cmd $ "** Found top modules: " ++  show topIds

--   let projs   = buildProjStructure projDb topIds
--       dbgPath1 = objPath cmd </> name cmd <.> target cmd <.> "allcomp" <.> "objdump"
--       dbgPath2 = objPath cmd </> name cmd <.> target cmd <.> "project" <.> "objdump"
--   when (isDebug cmd) $ dumpPrettyLibObj dbgPath1 projDb
--   when (isDebug cmd) $ dumpPrettyLibObj dbgPath2 projs

--   zipWithM_ (dumpCode cmd) topIds projs
  
--   return ()
      
-- isDebug x = case debug x of
--   Left False -> False
--   _ -> True

-- initDebug x = when (isDebug x) $ init
--   where
--     init = case debug x of
--       Left False -> error "Impossible!"
--       Left True  -> return ()
--       Right o    -> writeFile o ""

-- printDebug x = when (isDebug x) . debugOut
--   where
--     debugOut = case debug x of
--       Left False -> error "Impossible!"
--       Left True  -> putStrLn
--       Right o    -> appendFile o . (++"\n")

-- dumpCode x top proj = case outPath x of
--   Nothing -> putDoc $ generateCode (layout x) proj
--   Just d  -> do
--     -- create dump path
--     let dumpPath = d </> name x
--     createDirectoryIfMissing True dumpPath
--     -- try to copy existing declared dependencies
--     forM_ (getDependencies proj) $ \f -> do
--       let depPath = inPath x </> normalise f
--           trgPath = dumpPath </> depPath
--       isDepFile <- doesFileExist depPath
--       when isDepFile $ do
--         createDirectoryIfMissing True (takeDirectory trgPath)
--         copyFile depPath trgPath
--     -- now generate and dump the code
--     withFile (dumpPath </> (drop 4 $ unpack top) <.> "c") WriteMode $
--              \h -> renderIO h $ layoutPretty (layout x) $ generateCode (layout x) proj

data Flag
  = Debug
  | Help
  | Init
  | New
  | Force
  | Docs String
  | DocOpt String
  deriving (Eq,Ord,Show)

flags =
  [ -- control flags
    Option ['d'] ["debug"] (NoArg Debug)
    "Logs debug info and dumps debug object files"
  , Option ['h'] ["help"]   (NoArg Help)
    "Print this help message"
  , Option [] ["global-init"] (NoArg Init)
    "Initializes a global configuration file in case one does not exist."
  , Option ['n'] ["new"] (NoArg New)
    "Creates a new workspace structure having the current folder as root."
  , Option ['f'] ["force-load"] (NoArg Force)
    "Forces parsing libraries even if they can be loaded from dumped objects"
  , Option ['D'] ["docs"] (OptArg (Docs . fromMaybe "") "FORMAT")
    "Dumps the documentation for current project in given format. Default is html."
  , Option [] ["pandoc-opt"] (OptArg (DocOpt . fromMaybe "") "OPT")
    "Dumps the documentation for current project in given format. Default is html."
  ]
header = "Usage: f2c [options] [projects]"

parse argv =
  case getOpt Permute flags argv of
    (args,projs,[]) -> do
      let debug  = Debug `elem` args
          force  = Force `elem` args
          docs   = case [x | Docs x <- args] of
                     []   -> Nothing
                     [""] -> Just "html"
                     [x]  -> Just x
          docopt = [ x | DocOpt x <- args]
      when (Help `elem` args) $ do
        hPutStrLn stderr (usageInfo header flags)
        exitSuccess
      when (Init `elem` args) $ do
        makeDefaultConfig
        exitSuccess
      return (debug, CLIConfig force docs docopt, projs)
    (_,_,errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header flags)
      exitWith (ExitFailure 1)
