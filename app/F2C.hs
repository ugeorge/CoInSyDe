{-# LANGUAGE CPP #-}
module F2C where

import System.Console.GetOpt
import System.Environment
import System.Exit
#ifdef mingw32_HOST_OS
import System.FilePath.Windows
#else
import System.FilePath.Posix
#endif
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO

import Control.Monad (when,liftM)
import Control.Exception
import Data.List
import Data.Maybe
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import CoInSyDe.LibManage
import CoInSyDe.Core
import CoInSyDe.Backend.C

main = do
  cmd <- getArgs >>= parse
  initDebug cmd
  pDebug cmd $ show cmd
  
  -- check if libraries already loaded
  let tyObjPath = objp cmd </> name cmd <.> target cmd <.> "type" <.> "objdump"
      cpObjPath = objp cmd </> name cmd <.> target cmd <.> "component" <.> "objdump"
  isTyObj <- doesFileExist tyObjPath
  isCpObj <- doesFileExist cpObjPath

  -- load types and components libraries only if forced/needed
  (tyLib,cpLib') <- case isTyObj && isCpObj && not (force cmd) of
    -- if already existing, load previously-built objdumps
    True  -> do
      tyObj <- loadLibObj tyObjPath :: IO (Dict (Type C))
      cpObj <- loadLibObj cpObjPath :: IO (Dict (Comp C))
      return (tyObj,cpObj)
    -- if not, build libraries and put them in objdumps
    False -> do
      (tyLdList,cpLdList) <- buildLoadLists (target cmd) (libs cmd) 

      -- double check the load paths are correct
      pDebug cmd $ "$COINSYDE_PATH = " ++  show (libs cmd)
      pDebug cmd $ "** Loading type libs: " ++  show tyLdList
      pDebug cmd $ "** Loading component libs: " ++  show cpLdList

      tyObj <- loadTypeLibs (infile cmd) tyLdList
      cpObj <- loadCompLibs tyObj (infile cmd) cpLdList

      -- dump the built databases. If debug, then pretty-dump
      createDirectoryIfMissing True (objp cmd)
      (if isDebug cmd then dumpPrettyLibObj else dumpLibObj) tyObjPath tyObj
      (if isDebug cmd then dumpPrettyLibObj else dumpLibObj) cpObjPath cpObj
        
      -- return for further use
      return (tyObj,cpObj)
      
  -- finally with all types and template libraries, load the C project
  (topIds,cpLib) <- loadProject C tyLib cpLib' (infile cmd)

  let projs   = buildProjStructure cpLib topIds
      dbgPath = objp cmd </> name cmd <.> target cmd <.> "project" <.> "objdump"
  when (isDebug cmd) $ dumpPrettyLibObj dbgPath projs
  

  -- renderIO handler $ layoutPretty (layout) $ generateCode xml
  putDoc $ generateCode (head projs)
  
  return ()
      

data Commands
  = CFlags { name   :: String
           , debug  :: Either Bool FilePath
           , layout :: LayoutOptions
           , target :: String
           , force  :: Bool
           , docs   :: Maybe String
           , infile :: FilePath
           , outp   :: Maybe FilePath
           , objp   :: FilePath
           , libs   :: String
           } deriving (Show)

isDebug x = case debug x of
  Left False -> False
  _ -> True

initDebug x = when (isDebug x) $ init
  where
    init = case debug x of
      Left False -> error "Impossible!"
      Left True  -> return ()
      Right o    -> writeFile o ""

pDebug x = when (isDebug x) . debugOut
  where
    debugOut = case debug x of
      Left False -> error "Impossible!"
      Left True  -> putStrLn
      Right o    -> appendFile o . (++"\n")

data Flag
  -- control
  = Debug    String
  | LineW    String
  | StdOut
  | Help
  -- libraries
  | TrgName  String
  | ForceLd
  | BuildDoc String
  -- paths
  | OutPath  String
  | ObjPath  String
  | LoadPath String
  | LibsPath String
  deriving (Eq,Ord,Show)

flags =
  [ -- control flags
    Option ['d'] ["debug"] (OptArg (Debug . fromMaybe "") "PATH")
    "Logs debug info on stdout or, if path is provided, in a file"
  , Option ['w'] ["line-width"] (ReqArg LineW "NUM")
    "Maximum line width of the generated files. Default is infinite"
  , Option []    ["print"] (NoArg StdOut)
    "Prints the code on the stdout instead of a file"
  , Option ['h'] ["help"]   (NoArg Help)
    "Print this help message"
  -- Library flags
  , Option ['T'] ["target"] (ReqArg TrgName "NAME")
    "Target name (mandatory). Check documentation for supported target names. Default is .c"
  , Option ['f'] ["force-load"] (NoArg ForceLd)
    "Forces parsing and loading libraries for the current project."
  , Option ['D'] ["docs"] (OptArg (BuildDoc . fromMaybe "") "FORMAT")
    "Dumps the documentation for current project in given format. Default is html."
  -- Path flags
  , Option ['o'] ["out-path"] (ReqArg OutPath "PATH")
    "Path where the output file(s) will be generated. Default is ./"
  , Option []    ["obj-path"] (ReqArg ObjPath "PATH")
    "Path where the intermediate objects will be dumped. Tries to create it if does not exist. Default is <out-path>/obj"
  , Option ['L'] [] (ReqArg LoadPath "PATH")
    "Adds specified load libray paths at the end of $COINSYDE_PATH"
  , Option []    ["with-libs"] (ReqArg LibsPath "$PATH")
    "Replaces the $COINSYDE_PATH variable with the specified one"
  ]
header = "Usage: f2c -TNAME [options] input_file"

parse argv =
  case getOpt Permute flags argv of
    (args,n,[]) -> do
      origLibs <- case [x | LibsPath x <- args] of
                    []   -> getEnv "COINSYDE_PATH"
                    [""] -> error "You cannot leave --with-libs empty!"
                    [x]  -> return x
                    _    -> error "Error parsing library paths CLI argument."
      let debug  = case [x | Debug x <- args] of
                     []   -> Left False
                     [""] -> Left True
                     [x]  -> Right x
                     _    -> error "Too many arguments to --debug!"
          layout = case [x | LineW x <- args] of
                     []  -> defaultLayoutOptions
                     [x] -> LayoutOptions $ AvailablePerLine (read x :: Int) 1.0
                     _   -> error "Too many arguments to --line-width!"
          target = case [x | TrgName x <- args] of
                     [x] -> x
                     _   -> error "You need to specify one --target!"
          force  = case [x | x@ForceLd <- args] of
                     [] -> False
                     _  -> True
          docs   = case [x | BuildDoc x <- args] of
                     []   -> Nothing
                     [""] -> Just "html"
                     [x]  -> Just x            
          outp   = case ([x | OutPath x <- args],[x | x@(StdOut {}) <- args]) of
                     ([] ,[]) -> Just "."
                     ([x],[]) -> Just x
                     (_,_)    -> Nothing
          objp   = case [x | ObjPath x <- args] of
                     []   -> (fromMaybe "." outp) </> "obj"
                     [""] -> (fromMaybe "." outp) </> "obj"
                     [x]  -> x
          libs   = origLibs ++ ":" ++ intercalate ":" [x | LoadPath x <- args]
          inFile | null n = error "Please provide an input file!"
                 | otherwise = head n
          projNm = takeBaseName inFile
      if Help `elem` args
        then do hPutStrLn stderr (usageInfo header flags)
                exitSuccess
        else return $ CFlags projNm debug layout target force docs inFile outp objp libs
  
    (_,_,errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header flags)
      exitWith (ExitFailure 1)
