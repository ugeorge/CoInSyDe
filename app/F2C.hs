module F2C where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import Control.Monad (when)
import Data.List
import Data.Maybe
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Text.Pretty.Simple
import Text.XML.Light (parseXMLDoc)

import CoinSyDe.C.Chain

main = do
  (input, outp, debug, width) <- getArgs >>= parse
  inxml <- readFile input
  let outHandler = case outp of
                     Nothing -> return stdout
                     Just o  -> openFile o WriteMode
      debugHandler = case debug of
                       Left True -> return stdout
                       Right d   -> openFile d WriteMode
                       _ -> error "Nope!"
      pageLayout = case width of
                     Nothing -> defaultLayoutOptions
                     Just w  -> LayoutOptions (AvailablePerLine w 1.0)
      xml = getXml $ parseXMLDoc inxml
  ---- debug stuff ----
  when (doDebug debug) $ do
    handler <- debugHandler
    pHPrint handler $ debugFuncs xml
    -- hClose handler
  ---- main function ----
  handler <- outHandler
  renderIO handler $ layoutPretty pageLayout $ generateCode xml
  hClose handler
  ---- helpers ----
    where
      getXml (Just d) = d
      getXml Nothing  = error "XML input is empty!" 
      doDebug (Left False) = False
      doDebug _ = True

---- COMMAND LINE ARGUMENT HANDLING ----

data Flag
  = Debug String
  | LineW String
  | OutPath String
  | StdOut
  | Help        -- --help
  deriving (Eq,Ord,Show)

getArg (Debug s) = s
getArg (LineW s) = s
getArg (OutPath s) = s
isDebug (Debug _) = True
isDebug _ = False
isLineW (LineW _) = True
isLineW _ = False
isOutP (OutPath _) = True
isOutP _ = False
isPrint StdOut = True
isPrint _ = False

flags =
  [Option ['d'] ["debug"] (OptArg (Debug . fromMaybe "") "PATH")
    "Logs debug info on stdout or, if path is provided, in a file"
  ,Option ['w'] ["line-width"] (ReqArg LineW "NUM")
    "Maximum line width of the generated files. Default is infinite"
  ,Option ['o'] ["outpath"] (ReqArg OutPath "PATH")
    "Path where the output file(s) will be generated. Default is ./"
  ,Option [] ["print"] (NoArg StdOut)
    "Prints the code on the stdout instead of a file"
  ,Option ['h'] ["help"]   (NoArg Help)
    "Print this help message"
  ]
  
parse argv =
  case getOpt Permute flags argv of
    (args,n,[]) -> do
      let debug | null cm = Left False
                | null (getArg $ head cm) = Left True
                | otherwise = Right $ getArg $ head cm
            where cm = filter isDebug args
          width = if null cm then Nothing else Just (read $ getArg $ head cm :: Int)
            where cm = filter isLineW args
          outp  | not (null po) = Nothing
                | null op       = Just $ "." </> filen <.> "c" 
                | otherwise     = Just $ (getArg $ head op) </> filen <.> "c"
            where op = filter isOutP args
                  po = filter isPrint args
          input | null n    = error "Please provide an input file!"
                | otherwise = head n
          filen = takeBaseName input
      if Help `elem` args
        then do hPutStrLn stderr (usageInfo header flags)
                exitSuccess
        else return (input, outp, debug, width)
  
    (_,_,errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header flags)
      exitWith (ExitFailure 1)
      
  where header = "Usage: f2c [options] input_file"
