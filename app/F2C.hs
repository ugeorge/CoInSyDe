module F2C where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import Control.Monad (when)
import Data.List
import Data.Maybe

main = do
  args <- getArgs >>= parse
  print args

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
  , Option ['d'] ["docs"] (OptArg (BuildDoc . fromMaybe "") "FORMAT")
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

data Commands = CFlags
                { nameC   :: String
                , debugC  :: Either Bool String
                , widthC  :: Maybe Int
                , targetC :: String
                , forceC  :: Bool
                , docsC   :: Maybe String
                , outpC   :: Maybe FilePath
                , objpC   :: FilePath
                , libsC   :: [FilePath]
                } deriving (Show)
  
parse argv =
  case getOpt Permute flags argv of
    (_  ,[],[]) -> error "Please provide an input file!"
    (args,n,[]) -> do
      let readLib = case [x | LibsPath x <- args] of
                      []   -> getEnv "COINSYDE_PATH"
                      [""] -> error "You cannot leave --with-libs empty!"
                      [x]  -> return x
      origLibs <- readLib
      let debug  = case [x | Debug x <- args] of
                     []   -> Left False
                     [""] -> Left True
                     [x]  -> Right x
                     _    -> error "Too many arguments to --debug!"
          width  = case [x | LineW x <- args] of
                     []  -> Nothing
                     [x] -> Just (read x :: Int)
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
          libs     = splitSearchPath origLibs ++ concatMap splitSearchPath [x | LoadPath x <- args]
          projName = takeBaseName $ head n
      if Help `elem` args
        then do hPutStrLn stderr (usageInfo header flags)
                exitSuccess
        else return $ CFlags projName debug width target force docs outp objp libs
  
    (_,_,errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header flags)
      exitWith (ExitFailure 1)

