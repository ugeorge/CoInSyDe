{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module CoInSyDe.Backend.Gen where

import           Control.Exception
import           Control.Monad.Writer.Lazy as W
import qualified Data.Aeson as JSON
import           Data.Default (def)
import qualified Data.HashMap.Strict as H
import           Data.Maybe (fromMaybe)
import           Data.Text hiding (map, filter, head, concatMap)
import           Data.Yaml.Pretty
import           Text.Ginger
import           Text.Ginger.Run.Type (GingerContext (..))
import           Data.ByteString.Char8 as B (unpack)

import CoInSyDe.Core
import CoInSyDe.Core.Dict

import Control.Monad.State.Lazy
-- import Control.Monad.Except
import Data.Typeable (Typeable)

data GeneratorException
  = CriticalGen Text String
  | ErrorGen Text [Info] String
  deriving (Typeable)
instance Exception GeneratorException

instance Show GeneratorException where
  show (CriticalGen stage msg) =
    "Critical error generating code for " ++ show stage ++ ":\n " ++ msg
  show (ErrorGen stage [] msg) =
    "Error generating code for " ++ show stage ++ ":\n " ++ msg
  show (ErrorGen cp hist msg) =
    "Error generating code for component " ++ show cp
    ++ " with load history :\n->" ++ concatMap ((++"\n   ") . show) hist
    ++ "What's wrong: " ++ msg

-- TODO: use Either as exception monad
type Gen o l a = State (GenS o l) a

data GenS opt l = GenS { stage  :: Text
                       , cpDb   :: MapH (Comp l)
                       , layout :: opt
                       } deriving (Show)

genDoc s stg f = evalState f (s {stage = stg})

genDoc' = flip evalState


getState :: Target l => Gen o l (GenS o l)
getState = get

getCp :: Target l => Gen o l (Comp l)
getCp = get >>= \s -> return (cpDb s !* stage s)

getDb :: Target l => Gen o l (MapH (Comp l))
getDb = get >>= \s -> return (cpDb s)

getCpAndDb :: Target l => Gen o l (Comp l, MapH (Comp l))
getCpAndDb = get >>= \s -> return (cpDb s !* stage s, cpDb s)

getLayout :: Target l => Gen o l o
getLayout = get >>= \s -> return (layout s)

changeStage :: Target l => Id -> Gen o l ()
changeStage x = get >>= \s -> put (s {stage = x})


throwCritical :: String -> Gen o l a
throwCritical msg = do
  s <- get
  throw $ CriticalGen (stage s) msg

throwError :: String -> Gen o l a
throwError msg = do
  s <- get
  throw $ ErrorGen (stage s) [] msg

throwErrorH :: Target l => String -> Gen o l a
throwErrorH msg = do
  s <- get
  let cpId = stage s
      hist = cpDb s !^ cpId
  throw $ ErrorGen cpId hist msg

------------------------------------------------------------------

type TplRunner  = Run SourcePos (Writer Text) Text
type TplContext = GingerContext SourcePos (Writer Text) Text

mkFunction :: (Text -> Text)
           -> Function TplRunner
mkFunction f xs = do
  liftRun2 (\x->writer (empty,x)) . f . asText . snd . head $ xs -- only first arg
  return def  

mkContext :: (Text -> Text) 
          -> H.HashMap VarName JSON.Value
          -> TplContext
mkContext f d = makeContextText
                (\k -> fromMaybe (err k) $ H.lookup k contextDict)
  where
    contextDict = H.insert "placeholder"
                  (fromFunction $ mkFunction f)
                  (H.map toGVal d) :: H.HashMap VarName (GVal TplRunner)
    err k = error $ "Template error: Key " ++ show k
            ++ " not found in dictionary:\n"
            ++ (B.unpack $ encodePretty defConfig d)

generateCode :: Target l => TplContext
             -> Source -- -> (Writer Text) Text
             -> Gen o l Text
generateCode context tpl = do
  let options' = mkParserOptions (\_ -> return Nothing)
      options = options' { poKeepTrailingNewline = False
                         , poLStripBlocks = False
                         , poTrimBlocks = False}
  template <- either (throwErrorH . show) return =<< parseGinger' options tpl
  return $ runGinger context (optimize template) 

