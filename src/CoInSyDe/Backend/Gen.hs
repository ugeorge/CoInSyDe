{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module CoInSyDe.Backend.Gen (
  Gen, genDocStage, genDocComp, genDocComp', genDoc',
  GenState(..), initGenState, GenDebugOpts, defaultGenDebugOpts,
  getCp, getDb, getLayoutOpts, getState, pushCallStack, throwError,
  TplContext, mkContext, fromTemplate,
  GeneratorException
  ) where

import           Control.Exception
import           Control.Monad.State.Lazy
import           Control.Monad.Writer.Lazy
import qualified Data.Aeson as JSON
import           Data.Default (def)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import           Data.Maybe
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Typeable (Typeable)
import           Data.Yaml.Pretty
import           Text.Ginger
import           Text.Ginger.GVal
import           Text.Ginger.Run.Type (GingerContext (..))

import           CoInSyDe.Core
import           CoInSyDe.Core.Dict

type Gen o l = State (GenState o l)

data GenState opt l
  = GenS { stage      :: String
         , callStack  :: [Id]
         , database   :: MapH (Comp l)
         , layoutOpts :: opt
         , debugOpts  :: GenDebugOpts
         } deriving (Show)

initGenState = GenS "" []

genDocStage st stg f      = evalState f (st {stage = stg})
genDocComp  st stg cpId f = evalState f (st {stage = stg, callStack = [cpId]})

genDocComp' st cpId f = evalState f (st {callStack = cpId : callStack st})
genDoc' = flip evalState

getCp :: Target l => Gen o l (Comp l)
getCp = get >>= \s -> return $ database s !* (head $ callStack s)

getDb :: Target l => Gen o l (MapH (Comp l))
getDb = get >>= \s -> return $ database s

getState :: Target l => Gen o l (GenState o l)
getState = get

getLayoutOpts :: Target l => Gen o l o
getLayoutOpts = get >>= \s -> return (layoutOpts s)

pushCallStack :: Target l => Id -> Gen o l ()
pushCallStack x = modify $ \s -> (s {callStack = x : callStack s})

-- only one used outside
throwError :: Target l => String -> Gen o l a
throwError msg = do
  s <- get
  let (_,stack,hist) = throwHelper s
  throw $ GeneratorError (stage s) stack hist msg

throwTemplateParse msg = do
  s <- get
  let (cpId',_,hist) = throwHelper s
      cpId = fromMaybe (error "Critical: no call stack!") cpId'
  throw $ TemplateParseError cpId hist msg

throwTemplateRun msg = do
  s <- get
  let (_,stack,hist) = throwHelper s
  throw $ TemplateRunError stack hist msg

throwHelper :: Target l => GenState o l -> (Maybe Id, [Id], [Info])
throwHelper s = (cpId,stack,hist)
  where 
    opts  = debugOpts s
    cpId  = listToMaybe $ callStack s
    stack = if showCallStack opts then callStack s else []
    h'    = if showLoadFile opts then (!^) (database s) <$> cpId else Nothing
    hist  = if showLoadHist opts then fromMaybe [] h' else maybeToList (head <$> h')

data GenDebugOpts =
  GenDebugOpts { showLoadHist  :: Bool
               , showLoadFile  :: Bool
               , showCallStack :: Bool
               } deriving Show

defaultGenDebugOpts =
  GenDebugOpts { showLoadHist  = False
               , showLoadFile  = True
               , showCallStack = True }
    
------------------------------------------------------------------

newtype WriterMonad = WM { runWriterMonad :: Either String Text }

instance Semigroup WriterMonad where
  (WM (Right x)) <> (WM (Right y)) = WM (Right (mappend x y))
  (WM (Left x))  <> _ = WM (Left x)
  _  <> (WM (Left x)) = WM (Left x)
  
instance Monoid WriterMonad where
  mempty = WM (Right T.empty)

------------------------------------------------------------------

type TplMonad   = Writer WriterMonad
type TplRunner  = Run SourcePos TplMonad Text
type TplContext = GingerContext SourcePos TplMonad Text

tellTplM  = tell . WM
newTplM x = writer (T.empty, WM $ Right x)
execTplM  = runWriterMonad . execWriter

mkFunction :: (Text -> Text) -> Function TplRunner
mkFunction f xs = do
  liftRun2 newTplM . f . asText . snd . head $ xs -- only first arg
  return def  

mkContext :: (Text -> Text) 
          -> H.HashMap VarName JSON.Value
          -> TplContext
mkContext f d = makeContextTextExM look write except
  where
    look :: VarName
           -> Run SourcePos TplMonad Text (GVal (Run SourcePos TplMonad Text))
    look k = return $ toGVal $ H.lookup k contextDict

    write :: Text -> TplMonad ()
    write = tellTplM . Right

    except :: RuntimeError SourcePos -> TplMonad ()
    except e@(RuntimeErrorAt p (IndexError i)) = tellTplM $ Left $
      T.unpack (runtimeErrorWhat e) ++ " at " ++
      L.concatMap (show . flip setSourceName "") (runtimeErrorWhere e) ++
      "\nCannot find key " ++ show i ++ " in dictionary\n" ++
      T.unpack (decodeUtf8 $ encodePretty defConfig d)
    except e = tellTplM $ Left $
      T.unpack (runtimeErrorWhat e) ++ " at " ++
      L.concatMap (show . flip setSourceName "") (runtimeErrorWhere e) ++
      "\n" ++ T.unpack (runtimeErrorMessage e)
      
    contextDict ::  H.HashMap VarName (GVal TplRunner)
    contextDict = H.insert "placeholder"
                  (fromFunction $ mkFunction f) $
                  H.insert "range"
                  (fromFunction rangeF) $
                  (H.map toGVal d)

fromTemplate :: Target l
             => TplContext
             -> Source -- -> (Writer Text) Text
             -> Gen o l Text
fromTemplate context tpl = do
  let options  = mkParserOptions (\_ -> return Nothing)
      errParse = throwTemplateParse . formatParserError (Just tpl) 
  template <- either errParse return =<< parseGinger' options tpl
  let status = execTplM $ runGingerT context (optimize template) 
  case status of
    Left err   -> throwTemplateRun err
    Right code -> return code

------------------------------------------------------------------    
-- U kidding me?! Ginger has no 'range' function. I need to define it

rangeF :: Monad m => Function (Run p m h)
rangeF [] = return def
rangeF ((_,x):_) = return . toGVal . (\u -> [0..u-1]) . toNumber $ x
  where
    toNumber :: GVal m -> Int
    toNumber = fromMaybe (error "NaI!") . toBoundedInteger . fromMaybe (error "NaN!") . asNumber

------------------------------------------------------------------

data GeneratorException 
  = GeneratorError String [Id] [Info] String
  | TemplateParseError Id [Info] String
  | TemplateRunError [Id] [Info] String
  deriving (Typeable)
instance Exception GeneratorException

instance Show GeneratorException where
  show (GeneratorError stage stack history msg) =
    "Code generator error" ++ showErrHelper stage stack history msg 
  show (TemplateParseError id history msg) =
    "Template parse error" ++ showErrHelper "" [id] history msg 
  show (TemplateRunError stack history msg) =
    "Template generation error" ++ showErrHelper "" stack history msg 

showErrHelper :: String -> [Text] -> [Info] -> String -> String
showErrHelper stg s h msg =
  showStage stg ++ showCp s ++ showFrom h ++ showMsg ++ showStack s ++ showHistory h
  where
    showStage ""  = ""
    showStage stg = " when " ++ stg
    showCp []  = ""
    showCp (x:_) = " for component " ++ show x
    showMsg  = ":\n" ++ msg
    showStack [] = ""
    showStack x  = "\n\nCall stack: " ++ (L.intercalate " <- " $ map T.unpack x)
    showFrom []    = ""
    showFrom (x:_) = " loaded from: " ++ prettyInfo x
    showHistory (_:y:ys) = "\nOverloading components with the same name from:\n"
                           ++ concatMap (((++) "\n * ") . prettyInfo) (y:ys)
    showHistory _ = ""
                         
