{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module CoInSyDe.Internal.Ginger (
  GAcc, GContext, mkGingerContext, execG,
  mkPlaceholderFun, rangeFun, portLookupFun
  ) where

import           Control.Arrow (second,(&&&))
import           Control.Monad.State (gets)
import           Control.Monad.Writer.Lazy
import           Data.Default
import           Data.Either
import qualified Data.HashMap.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.YAML (Node, Pos(..))
import           Text.Ginger
import           Text.Ginger.GVal
import           Text.Ginger.Run.Type
import           Text.Ginger.Run.VM

import           CoInSyDe.Internal.YAML
import           CoInSyDe.Internal.Map

------------------------------------------------------------------

-- | Accumulator type for own writer monad to use in Ginger transformer
newtype GAcc =  Acc { getAccum :: Either String Text }

instance Semigroup GAcc where
  (Acc (Right x)) <> (Acc (Right y)) = Acc (Right (mappend x y))
  (Acc (Left x))  <> _ = Acc (Left x)
  _  <> (Acc (Left x)) = Acc (Left x)
  
instance Monoid GAcc where
  mempty = Acc (Right T.empty)

------------------------------------------------------------------

type GWriter  = Writer GAcc
type GRunner  = Run SourcePos GWriter Text
type GContext = GingerContext SourcePos GWriter Text

tellG  = tell . Acc
newG x = writer (T.empty, Acc x)
execG  = getAccum . execWriter

mkGingerContext :: YSrcCode      -- ^ the template source structure
                -> Map (Node ()) -- ^ built YAML dictionary
                -> (Text -> Map Text -> Either String Text)
                -- ^ placeholder expander function defined by the target
                -> GContext      -- ^ Ginger context
mkGingerContext srcdoc usrDict phFun = makeContextTextExM look write except
  where
    look :: VarName -> GRunner (GVal GRunner)
    look k = case contextDict !? k of
      Just val -> return $ toGVal val
      Nothing -> usrError srcdoc msg
        where msg = " Cannot find key " ++ show k ++ " in dictionary\n"
                    ++ prettyYNode (toYAML usrDict)

    write :: Text -> GWriter ()
    write = tellG . Right

    except :: RuntimeError SourcePos -> GWriter ()
    except e@(RuntimeErrorAt p (IndexError i)) = tellG $ Left $
      T.unpack (runtimeErrorWhat e) ++ " at " ++
      concatMap (prettyYSrcWithOffset srcdoc msg) (getOffset e)
      where msg = " Cannot find key " ++ T.unpack i ++ " in dictionary\n"
                  ++ prettyYNode (toYAML usrDict)
    except e = tellG $ Left $
      T.unpack (runtimeErrorWhat e) ++ " at " ++
      concatMap (prettyYSrcWithOffset srcdoc msg) (getOffset e)
      where msg = T.unpack (runtimeErrorMessage e)
      
    contextDict :: Map (GVal GRunner)
    contextDict =
      M.insert "placeholder" (fromFunction $ mkPlaceholderFun srcdoc phFun) $
      M.insert "range"       (fromFunction rangeFun) $
      M.insert "port"        (fromFunction $ portLookupFun srcdoc) $
      M.insert "exists"      (fromFunction $ existsFun srcdoc) $
      -- M.insert "reeval"      (fromFunction $ reEvalFun srcdoc) $
      M.insert "setdef"      (fromFunction $ setMetaFun srcdoc) $
      M.map toGVal usrDict

getOffset = map (sourceLine &&& sourceColumn) . runtimeErrorWhere 

usrError srcdoc msg = do
  offs <- (sourceLine &&& sourceColumn) <$> getSourcePos
  _ <- liftRun2 tellG $ Left $ prettyYSrcWithOffset srcdoc msg offs
  return def
  
------------------------------------------------------------------    

-- TODO: "tell" errors, do not throw them

-- | Makes a template placeholder function from a target function @\ id arglist@. This
-- function uses only positional arguments.
mkPlaceholderFun :: YSrcCode -> (Text -> Map Text -> Either String Text)
                 -> Function GRunner
mkPlaceholderFun src f xs = do
  let (_,(name:_),args) =  matchFuncArgs [] xs
      msg = "PlaceholderError at " ++ show (ysrcPath src)
            ++ " (" ++ show (posLine $ ysrcPos src) ++ ":"
            ++ show (posColumn $ ysrcPos src) ++ ")\n"
  res <- return $ either (Left . (++) msg) Right $ f (asText name) (fmap asText args)
  _ <- liftRun2 newG res
  return def


-- reEvalFun :: YSrcCode -> Function GRunner
-- reEvalFun _ [] = throwHere $ ArgumentsError (Just "range") "called with no arguments!"
-- reEvalFun doc [_] = usrError doc "No default value set!"
-- reEvalFun  doc ((_,expr):(_,ctxvar):_) = do
--   context    <- gets rsScope
--   let newCtx = case asDictItems ctxvar of
--         Nothing   -> fmap toGVal context
--         Just vars -> M.fromList vars `M.union` fmap toGVal context
--       contextLookup k = maybe (error "!!!!") return $ M.lookup k newCtx
--   let template = T.unpack $ asText expr
--       options  = mkParserOptions (\_ -> return Nothing)
--       errParse = error . formatParserError (Just template)  -- TODO
--   ginger <- either errParse return =<< parseGinger' options template
--   return $ toGVal $ runGinger (makeContextTextM contextLookup (tellG . Right)) ginger

  
-- | Template range function called with @range@. Usage:
--
-- > range (from = 0, step = 1, <to>)
--
-- where @<to>@ is mandatory and needs to be an integer.
rangeFun :: Function GRunner
rangeFun [] = throwHere $ ArgumentsError (Just "range") "called with no arguments!"
rangeFun args = do
  when (null pArgs) $ throwHere $ ArgumentsError (Just "range") "Range not given!"
  from <- flip (maybe (return 0)) (M.lookup "from" nArgs) $
          \x -> maybe (err x " is not an integer!") return (toInt x)
  step <- flip (maybe (return 1)) (M.lookup "range" nArgs) $
          \x -> maybe (err x " is not an integer!") return (toInt x)
  to   <- maybe (err pArgs " is not an integer!") return (toInt $ head pArgs)
  return $ toGVal ([from, from+step .. to-1] :: [Int])
  where
    (nArgs, pArgs, _) = matchFuncArgs ["from","step"] args
    err x msg = throwHere $ ArgumentsError (Just "range") $ T.pack (show x ++ msg)


-- | Custom lookup function called with @port@, useful whe the lookup key is the
-- result of a prior expression. Usage.
--
-- > port(key)
portLookupFun :: YSrcCode -> Function GRunner
portLookupFun doc [] = usrError doc "Function called with no argument!"
portLookupFun _ ((_,x):_) = do
  context <- getVar (asText x)
  return $ toGVal context

existsFun :: YSrcCode -> Function GRunner
existsFun doc [] = usrError doc "Function called with no argument!"
existsFun _ ((_,x):_) = do
  vars <- gets rsScope
  case M.lookup (asText x) vars of
    Nothing -> return $ toGVal False
    _  -> return $ toGVal True



setMetaFun :: YSrcCode -> Function GRunner
setMetaFun doc []  = usrError doc "Function called with no argument!"
setMetaFun doc [_] = usrError doc "No default value set!"
setMetaFun doc ((_,x):(_,deft):_) = do
  let keyname = asText x
  var <- getVar keyname 
  case (asDictItems var,asList var) of
    (Nothing,Nothing) -> usrError doc $ "Parameter "++ show x ++" is not a dictionary"
    (Just metadict,_) -> case toGVal <$> parseYText (asText deft) of
      Left e  -> usrError doc e
      Right v -> do let newmeta = M.fromList $ map (injectGVal v) metadict
                    setVar keyname $ toGVal newmeta
                    return def
    (_,Just metalist) -> case toGVal <$> parseYText (asText deft) of
      Left e  -> usrError doc e
      Right v -> do let newmeta = M.fromList $
                          map (\i -> injectGVal v (asText i,i)) metalist
                    setVar keyname $ toGVal newmeta
                    return def

-- injectGVal :: GVal m -> GVal m -> GVal m
-- injectGVal defv host = case (asDictItems defv, asDictItems host) of
--   (True,_,_) -> defv
--   (Just dd,Just hd)->toGVal $ M.unionWith injectGVal (M.fromList dd) (M.fromList hd)
--   (_,_)    -> host

injectGVal :: GVal m -> (Text, GVal m) -> (Text, GVal m)
injectGVal defv (i,host) = (,) i $  case (asDictItems defv, asDictItems host) of
  (Just dd,Nothing) -> toGVal $ M.insertWith (\_ v -> v) "_callback" (toGVal i)
                       $ M.fromList dd
  (Just dd,Just hd) -> toGVal $ M.insertWith (\_ v -> v) "_callback" (toGVal i)
                     $ M.unionWith inject' (M.fromList dd) (M.fromList hd)
  (_,_)    -> host
  where
    inject' defv host = case (asDictItems defv, asDictItems host) of
      (Just dd,Nothing) -> defv
      (Just dd,Just hd) ->toGVal $ M.unionWith inject' (M.fromList dd) (M.fromList hd)
      (_,_)    -> host

