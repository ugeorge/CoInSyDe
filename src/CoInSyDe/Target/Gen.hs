{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module CoInSyDe.Target.Gen (
  CodeGen, initGenState, genCode, genCodeCp, spawnGen,
  -- * Monad methods
  get, getCp, getInfo, layoutOpts,
  pushCallStack, -- popCallStack,
  fromTemplate,
  -- * Error throwing
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
import           CoInSyDe.Internal.YAML (YSrcCode(..), ysrcCode)

-- | Code generation monad. 
type CodeGen o l = State (GenState o l)

-- | State for code generation monad
data GenState opt l = GenState
  { stage      :: String        -- ^ where in the compiler chain we are
  , debugLog   :: String        -- ^ convenience place to write debug messages
  , callStack  :: [CallInfo]    -- ^ updated with 'pushCallStack'
  , database   :: MapH (Comp l) -- ^ contains all components loaded in a project.
  , layoutOpts :: opt           -- ^ pretty-printer layout options.
  } deriving (Show)

data CallInfo = Call { callId :: Id, callInfo :: Info, callLevel :: String }
instance Show CallInfo where
  show (Call i info lvl) = lvl ++ "+> " ++ show i ++ "[" ++ prettyInfo info ++ "]"

-- | Returns an initial state with a fully loaded component database and a set of
-- pretty-printer-specific layout options
initGenState :: MapH (Comp l) -> layoutOpts -> GenState layoutOpts l 
initGenState = GenState "" "" []

-- | Evaluates a generator monad and returns the result. 
genCode :: Target l
        => GenState o l  -- ^ initial state
        -> String        -- ^ current stage, i.e. where in the compiler chain we are
        -> CodeGen o l a -- ^ generator
        -> a 
genCode st stg f = evalState f (st {stage = stg})

-- | Evaluates a generator after calling 'pushCallStack' first.
spawnGen :: Target l
         => GenState o l  -- ^ state (usually passed from the caller)
         -> Id            -- ^ called component ID
         -> CodeGen o l a -- ^ generator
         -> a
spawnGen st i f  = evalState ( pushCallStack i >> f ) st

-- | Same as 'genCode' but also calls 'pushCallStack' first.
genCodeCp :: Target l
          => GenState o l  -- ^ (initial) state
          -> String        -- ^ current stage, i.e. where in the compiler chain we are
          -> Id            -- ^ called component ID
          -> CodeGen o l a -- ^ generator
          -> a 
genCodeCp st stg = spawnGen (st {stage = stg})

-- | Gets current component in the monad.
getCp :: Target l => CodeGen o l (Comp l)
getCp = get >>= \s -> do
  let cpId = callId $ head $ callStack s
  maybe (genError $ "Component " ++ show cpId ++ "not found!")
    return $ database s !* cpId

-- | Gets information about the current component.
getInfo :: Target l => CodeGen o l [Info]
getInfo = get >>= \s -> do
  let cpId = callId $ head $ callStack s
  maybe (genError $ "Component " ++ show cpId ++ "not found!")
    return $ database s !^ cpId

-- | Pushes a component ID (and associated information) into the call stack, after
-- which it becomes the current component of the generator monad. See 'getCp'.
pushCallStack :: Target l => Id -> CodeGen o l ()
pushCallStack cpId =  get >>= \s -> do
  let cs = callStack s
      newLevel [] = ""
      newLevel (Call _ _ l:_) = ' ':l
  info <- maybe (genError $ "Component " ++ show cpId ++ "not found!")
          return $ database s !^ cpId
  modify $ \s -> s {callStack = Call cpId (head info) (newLevel cs) : cs}

-- | Logs a certain debug information.
logDebug :: Target l => String -> CodeGen o l ()
logDebug msg = do
  st <- get
  let prompt = "[" ++ stage st ++ " : "
               ++ (L.intercalate "->" $ map (show . callId) $ callStack st)
               ++ "]\n\t"
  modify $ \s -> s {debugLog = debugLog st ++ prompt ++ msg ++ "\n"} 

------------------------------------------------------------------

-- | Evaluates a Ginger template (see "CoInSyDe.Internal.Ginger") in the context of a
-- generator, and returns the expanded code as text.
fromTemplate :: Target l
             => GContext -- ^ Ginger context
             -> YSrcCode -- ^ Template source stored in YAML node
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

-- | Exception raised by a generator. Causes a program-wide error, but the message is
-- more meaningful. 
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

-- | Throw an error at a specific position inside a generator.
genError :: Target l => String -> CodeGen o l a
genError msg = get >>= \s -> throw $ GenError (stage s) (callStack s) msg

gingerParseError msg = get >>= \s -> do
  let call   = fromMaybe (error errmsg) $ listToMaybe $ callStack s
      errmsg = "[CRITICAL] cannot parse template which is not in a component!"
  throw $ GingerParseError call msg

gingerRunError msg =  get >>= \s -> throw $ GingerRunError (callStack s) msg
