{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module CoInSyDe.Backend.Template where

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
import Data.Text.Prettyprint.Doc
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

type Gen l a = State (GenS l) a

data GenS l = DocS { stage  :: Text
                   , cpDb   :: MapH (Comp l)
                   , layout :: LayoutOptions
                   } deriving (Show)

writeDoc s stg f = execState f (s {stage = stg})

getCp :: Target l => Gen l (Comp l)
getCp = get >>= \s -> return (cpDb s !* stage s)

getCpAndDb :: Target l => Gen l (Comp l, MapH (Comp l))
getCpAndDb = get >>= \s -> return (cpDb s !* stage s, cpDb s)

throwCritical :: String -> Gen l a
throwCritical msg = do
  s <- get
  throw $ CriticalGen (stage s) msg

throwError :: String -> Gen l a
throwError msg = do
  s <- get
  throw $ ErrorGen (stage s) [] msg

throwErrorH :: Target l => String -> Gen l a
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

generateCode :: TplContext
             -> Source -> (Writer Text) Text
generateCode context tpl = do
  let options' = mkParserOptions (\_ -> return Nothing)
      options = options' { poLStripBlocks = True
                         , poTrimBlocks = False}
  template <- either throw return =<< parseGinger' options tpl
  return $ runGinger context (optimize template)

