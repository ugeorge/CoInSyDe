{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module CoInSyDe.Internal.Ginger (
  GAcc, GContext, mkGingerContext, execG,
  mkPlaceholderFun, rangeFun, portLookupFun
  ) where

import           Control.Monad.Writer.Lazy
import           Data.Default
import qualified Data.HashMap.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.YAML (Node)
import           Text.Ginger
import           Text.Ginger.GVal
import           Text.Ginger.Run.Type (GingerContext (..),throwHere)
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
newG x = writer (T.empty, Acc $ Right x)
execG  = getAccum . execWriter

-- | Makes a template placeholder function from a target function @\ id arglist@. This
-- function uses only positional arguments.
mkPlaceholderFun :: (Text -> [Text] -> Either Text Text) -> Function GRunner
mkPlaceholderFun f xs = do
  let (name:args) =  map (asText . snd) xs
      pArgs = T.intercalate "; " args
  transformed <- either (throwHere . ArgumentsError (Just pArgs)) return $ f name args
  _ <- liftRun2 newG transformed
  return def

mkGingerContext :: Info          -- ^ where the template source is found
                -> Map (Node ()) -- ^ built YAML dictionary
                -> (Text -> [Text] -> Either Text Text)
                -- ^ placeholder expander function defined by the target
                -> GContext      -- ^ Ginger context
mkGingerContext info usrDict phFun = makeContextTextExM look write except
  where
    look :: VarName
         -> Run SourcePos GWriter Text (GVal (Run SourcePos GWriter Text))
    look k = return $ toGVal $ contextDict !? k

    write :: Text -> GWriter ()
    write = tellG . Right

    except :: RuntimeError SourcePos -> GWriter ()
    except e@(RuntimeErrorAt p (IndexError i)) = tellG $ Left $
      T.unpack (runtimeErrorWhat e) ++ " at " ++
      concatMap (show . flip setSourceName (prettyInfo info)) (runtimeErrorWhere e)
      ++ "\nCannot find key " ++ show i ++ " in dictionary\n"
      ++ prettyYNode (toYAML usrDict)
    except e = tellG $ Left $
      T.unpack (runtimeErrorWhat e) ++ " at " ++
      concatMap (show . flip setSourceName (prettyInfo info)) (runtimeErrorWhere e)
      ++ "\n" ++ T.unpack (runtimeErrorMessage e)
      
    contextDict :: Map (GVal GRunner)
    contextDict = M.insert "placeholder" (fromFunction $ mkPlaceholderFun phFun) $
                  M.insert "range"       (fromFunction rangeFun) $
                  M.insert "port"        (fromFunction portLookupFun) $
                  M.map toGVal usrDict

------------------------------------------------------------------    

-- | Template range function called with @range@. Usage:
--
-- > range (from = 0, step = 1, <to>)
--
-- where @<to>@ is mandatory and needs to be an integer.
rangeFun :: Monad m => Function (Run p m h)
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
portLookupFun :: Monad m => Function (Run p m h)
portLookupFun [] = throwHere $ ArgumentsError (Just "port") "called with no argument!"
portLookupFun ((_,x):_) = do
  context <- getVar (asText x)
  return $ toGVal context
