{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module CoInSyDe.Target.Gen (
  CodeGen, genCode, genCodeCp, spawnGen,
  initGenState, getCp, getInfo, layoutOpts,
  pushCallStack, -- popCallStack,
  fromTemplate,
  GeneratorException, genError
  ) where

import           Control.Exception
import           Control.Monad.State.Lazy
import           Data.Maybe
import           Data.Typeable
import           Text.Ginger

import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Text as T

import           CoInSyDe.Core
import           CoInSyDe.Internal.Ginger
import           CoInSyDe.Internal.Map
import           CoInSyDe.Internal.YAML (YSrcCode(..))


type CodeGen o l = State (GenState o l)
data GenState opt l = GenState
  { stage      :: String
  , debugLog   :: String
  , callStack  :: [CallInfo]
  , database   :: MapH (Comp l)
  , layoutOpts :: opt
  } deriving (Show)

data CallInfo = Call { callId :: Id, callInfo :: Info, callLevel :: String }
instance Show CallInfo where
  show (Call i info lvl) = lvl ++ "+> " ++ show i ++ "[" ++ prettyInfo info ++ "]"

initGenState = GenState "" "" []

genCode st stg f = evalState f (st {stage = stg})
genCodeCp st stg = spawnGen (st {stage = stg})
spawnGen st i f  = evalState ( pushCallStack i >> f ) st

getCp :: Target l => CodeGen o l (Comp l)
getCp = get >>= \s -> do
  let cpId = callId $ head $ callStack s
  maybe (genError $ "Component " ++ show cpId ++ "not found!")
    return $ database s !* cpId

getInfo :: Target l => CodeGen o l [Info]
getInfo = get >>= \s -> do
  let cpId = callId $ head $ callStack s
  maybe (genError $ "Component " ++ show cpId ++ "not found!")
    return $ database s !^ cpId

pushCallStack :: Target l => Id -> CodeGen o l ()
pushCallStack cpId =  get >>= \s -> do
  let cs = callStack s
      newLevel [] = ""
      newLevel (Call _ _ l:_) = ' ':l
  info <- maybe (genError $ "Component " ++ show cpId ++ "not found!")
          return $ database s !^ cpId
  modify $ \s -> s {callStack = Call cpId (head info) (newLevel cs) : cs}

-- popCallStack :: Target l => a -> CodeGen o l a
-- popCallStack a =
--   modify $ \s -> let (c:cs) = callStack s
--                  in s {callStack = c {callLevel = tail $ callLevel c} : cs}
--   return a
  
logDebug :: Target l => String -> CodeGen o l ()
logDebug msg = do
  st <- get
  let prompt = "[" ++ stage st ++ " : "
               ++ (L.intercalate "->" $ map (show . callId) $ callStack st)
               ++ "]\n\t"
  modify $ \s -> s {debugLog = debugLog st ++ prompt ++ msg ++ "\n"} 

------------------------------------------------------------------

-- TODO: format parser error based on YPos
fromTemplate :: Target l
             => GContext
             -> YSrcCode
             -> CodeGen o l T.Text
fromTemplate context tpl = do
  let template = T.unpack $ ysrcCode tpl
      options  = mkParserOptions (\_ -> return Nothing)
      errParse = gingerParseError . formatParserError (Just template) 
  ginger <- either errParse return =<< parseGinger' options template
  let status = execG $ runGingerT context (optimize ginger) 
  case status of
    Left err   -> gingerRunError err
    Right code -> return code

------------------------------------------------------------------

data GeneratorException 
  = GenError String [CallInfo] String
  | GingerParseError CallInfo  String
  | GingerRunError  [CallInfo] String
  deriving (Typeable)
instance Exception GeneratorException

instance Show GeneratorException where
  show (GenError stage stack msg) =
    "Code generator error in stage " ++ show stage ++ ":\n*  " ++ msg
    ++ "\n\n<showing call stack>\n" ++ L.unlines (reverse $ map show stack)
  show (GingerParseError call msg) =
    "Template parse error in component\n" ++ show call ++ "\n" ++ msg
  show (GingerRunError stack msg) =
    "Template execution error:\n*  " ++ msg
    ++ "\n\n<showing call stack>\n" ++ L.unlines (reverse $ map show stack)

-- only one used outside
genError :: Target l => String -> CodeGen o l a
genError msg = get >>= \s -> throw $ GenError (stage s) (callStack s) msg

gingerParseError msg = get >>= \s -> do
  let call   = fromMaybe (error errmsg) $ listToMaybe $ callStack s
      errmsg = "[CRITICAL] cannot parse template which is not in a component!"
  throw $ GingerParseError call msg

gingerRunError msg =  get >>= \s -> throw $ GingerRunError (callStack s) msg
