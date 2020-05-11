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

import Control.Monad
import Data.Maybe
import Data.Text (unpack,pack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Version (showVersion)
import Text.Pretty.Simple

import Paths_coinsyde (version)
import Text.Pandoc.Definition (pandocTypesVersion)

import CoInSyDe.Internal.Config
import CoInSyDe.Internal.Docs
import CoInSyDe.Internal.LibManage
import CoInSyDe.Internal.Map
import CoInSyDe.Internal.YAML (YNode)

import CoInSyDe.Target.C

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
        createDirectoryIfMissing True (projObj conf)
        (tyLib,cpLib) <- loadLibs C gconf conf reload libLists
        when debug $ dbgPrettyLib conf "type" tyLib
        when debug $ dbgPrettyLib conf "pattern" cpLib

        unless (isNothing $ docs cliconf) $ 
          dumpLibraryDoc cliconf gconf conf "Libs" "Loaded Libraries"
          [ ("Types",      toDoc "ty" tyLib)
          , ("Components", toDoc "cp" cpLib) ]

        let proj = buildProjs cpLib (projTops conf)
        unless (isNothing $ docs cliconf) $
          forM_ proj $ \p -> do
          let pName = unpack (topModule p)
          dumpLibraryDoc cliconf gconf conf
            ("Proj-" ++ pName) ("Project for top module: " ++ pName)
            [ ("Meta",       toDoc "" p)
            , ("Components", toDoc "cp" $ projComps p) ]
          writeFile (projDocs conf </> ("Graph-" ++ pName) <.> "dot") $
            drawDotGraph p

        forM_ proj $ \p -> do
          dumpCode gconf conf p $ generateCode defaultLayoutOptions p
          
        putStrLn $ "C code generated in " ++ projCode conf

      other -> putStrLn $ "[WARNING] CoInSyDe does not support target family "
               ++ show other ++ ". Ignoring project " ++ show (projName conf) ++ "!"


-- dumpLibraryDoc :: CliConfig -> SuiteConfig -> ProjConfig -> Maybe String
--                -> [(String, Blocks)] -> IO ()
dumpLibraryDoc cliconf gconf conf filename title content = do
  createDirectoryIfMissing True (projDocs conf)
  time <- getCurrentTime
  let format   = fromJust $ docs cliconf
      otherOpt = docopts cliconf
      jsonFile = projDocs conf </> filename <.> "json"
      docFile  = projDocs conf </> filename <.> format
      pandoc   = makeDoc title (projName conf)
                 (pack $ targetidToString $ projTarget conf) (show time)
                 (coinsydeVersion gconf) (workspaceRoot gconf) content
  writePandocJson jsonFile pandoc
  _ <- createProcess (proc "pandoc" $
                       otherOpt ++ ["--toc", "--from=json", "--to=" ++ format,
                                    "--output=" ++ docFile] ++ [jsonFile])
  return ()

dumpCode :: SuiteConfig -> ProjConfig -> Proj -> Doc () -> IO ()
dumpCode gconf conf proj code = do
  -- create dump path
  createDirectoryIfMissing True (projCode conf)
  -- try to copy existing declared dependencies
  forM_ (resolveIncludes proj) $ \f -> do
    let depPath = targetLibPath gconf </> normalise (unpack f)
        trgPath = projCode conf </> depPath
    isDepFile <- doesFileExist depPath
    when isDepFile $ do
      createDirectoryIfMissing True (takeDirectory trgPath)
      copyFile depPath trgPath
  -- dump the code
  withFile (projCode conf </> (unpack $ topModule proj) <.> "c") WriteMode $
    \h -> renderIO h $ layoutPretty defaultLayoutOptions code

data Flag
  = Debug
  | Help
  | Ver
  | Init
  | New
  | Force
  | Docs String
  | DocOpt String
  deriving (Eq,Ord,Show)

flags =
  [ -- control flags
    Option ['h'] ["help"]   (NoArg Help)
    "Print this help message"
  , Option ['v'] ["version"]   (NoArg Ver)
    "Prints the version of this tool and dependencies."
  , Option ['d'] ["debug"] (NoArg Debug)
    "Logs debug info and dumps debug object files"
  , Option [] ["global-init"] (NoArg Init)
    "Initializes a global configuration file in case\none does not exist."
  , Option ['n'] ["new"] (NoArg New)
    "Creates a new workspace structure having the\ncurrent folder as root."
  , Option ['f'] ["force-load"] (NoArg Force)
    "Forces parsing libraries even if they can be\nloaded from dumped objects"
  , Option ['D'] ["docs"] (OptArg (Docs . fromMaybe "") "FORMAT")
    "Dumps the documentation for current project in\ngiven format. Default is html."
  , Option [] ["pandoc-opt"] (OptArg (DocOpt . fromMaybe "") "OPT")
    "Additional options passed to Pandoc when\nbuilding documentation."
  ]
header = "Usage: coinsyde [options] [projects]\n\n[projects] need to be defined in the 'coinsyde.yaml' configuration file for the current workspace. If none is specified, all projects are compiled.\n\nOptions:"

printVersion usrdir = "coinsyde-" ++ showVersion version 
  ++ "\nCoInSyDe -- Code Targeting System Design -- version " ++ showVersion version 
  ++ "\n(c) 2019-2020 George Ungureanu"
  ++ "\nUsing 'pandoc-types' version " ++ showVersion pandocTypesVersion
  ++ "\nDefalt user data directory: " ++ usrdir

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
      when (Ver `elem` args) $ do
        usrdir <- coinsydeDir
        hPutStrLn stderr (printVersion usrdir)
        exitSuccess
      when (Init `elem` args) $ do
        makeDefaultConfig
        exitSuccess
      when (New `elem` args) $ do
        makeWorkspace
        exitSuccess
      return (debug, CLIConfig force docs docopt, projs)
    (_,_,errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header flags)
      exitWith (ExitFailure 1)
