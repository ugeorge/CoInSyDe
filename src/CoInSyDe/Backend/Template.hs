{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module CoInSyDe.Backend.Template where

import           Control.Exception
import           Control.Monad.Writer.Lazy as W
import qualified Data.Aeson as JSON
import           Data.Default (def)
import qualified Data.HashMap.Strict as H
import           Data.Maybe (fromMaybe)
import           Data.Text hiding (map, filter, head)
import           Text.Ginger
import           Text.Ginger.Run.Type (GingerContext (..))


import Data.Text.Lazy as TL (unpack)
import Text.Pretty.Simple (pShow)

type DocW = Writer Text
type Runner = Run SourcePos DocW Text

mkFunction :: (Text -> Text) -- TODO: actually (Id -> Doc)
           -> Function Runner
mkFunction f xs = do
  liftRun2 (\x->writer (empty,x)) . f .  mconcat . map (asText . snd) $ xs
  return def  

mkContext :: (Text -> Text) -- TODO: see above
          -> H.HashMap VarName JSON.Value
          -> GingerContext SourcePos DocW Text
mkContext f d = makeContextText
                (\k -> fromMaybe (err k) $ H.lookup k contextDict)
  where
    contextDict = H.insert "placeholder"
                  (fromFunction $ mkFunction f)
                  (H.map toGVal d) :: H.HashMap VarName (GVal Runner)
    err k = error $ "Template error: Key " ++ show k
            ++ " not found in dictionary:\n" ++ (TL.unpack $ pShow d)

generateCode :: GingerContext SourcePos DocW Text
             -> Source -> DocW Text
generateCode context tpl = do
  let options' = mkParserOptions (\_ -> return Nothing)
      options = options' { poLStripBlocks = False
                         , poTrimBlocks = False}
  template <- either throw return =<< parseGinger' options tpl
  return $ runGinger context (optimize template)

-- decodeFile :: (JSON.FromJSON v) => FilePath -> IO (Maybe v)
-- decodeFile fn = YAML.decodeThrow <$> (openFile fn ReadMode >>= BS.hGetContents)
