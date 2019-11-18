{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module CoInSyDe.Core.Ginger where

import Control.Monad.Writer.Lazy as W
import Data.Text hiding (map, filter, head)
import Text.Ginger
-- import Text.Ginger.Run.Type (GingerContext (..))
import Data.Default (def)
import qualified Data.HashMap.Strict as H
import qualified Data.Aeson as JSON
import qualified Data.Yaml as YAML
import Data.Maybe (fromMaybe)
import Control.Exception
import qualified Data.ByteString as BS
import System.IO


type TTm = Template SourcePos

-- nullResolver :: IncludeResolver Identity
-- nullResolver = const $ return Nothing

-- parseTTm = either (error . show) id . runIdentity . parseGinger nullResolver Nothing



-- contextDict :: [(VarName, GVal (Run SourcePos IO a))]
contextDict :: [(VarName, GVal (Run SourcePos IO Text))]
contextDict = [("print",fromFunction printF)]

printF :: Function (Run SourcePos IO Text)
printF = \xs -> do
  liftIO . putStrLn . unpack . mconcat . map (asText . snd) $ xs
  return def           


-- seOptions=id mContext=makeContextTextM valToText=id includedLookups=[]

testParse = run id makeContextTextM id []
  where
    run setOptions mContext valToText includeLookup src = do
      let resolver srcName = return $ lookup srcName includeLookup
      let options = setOptions (mkParserOptions resolver)
      template <- either throw return =<< parseGinger' options src
      -- let write h = modifyIORef output (<> h)
      let context' = mContext
                     (\key -> return $ fromMaybe def (lookup key contextDict))
                     print
          context = context' { contextWarn = liftRun2 $ print }
      runGingerT context (optimize template)


-- printP :: Function (Run SourcePos Doc Text)
-- printP = \xs -> do
--   liftIO . putDoc . pretty . unpack . mconcat . map (asText . snd) $ xs
--   return def  

type TextW = Writer Text


printD :: Function (Run SourcePos TextW Text)
printD = \xs -> do
  liftRun2 (\x->writer (empty,x)) . mconcat . map (asText . snd) $ xs
  return def  

contextDict' :: [(VarName, GVal (Run SourcePos TextW Text))]
contextDict' = [("print",fromFunction printD)]


-- makeContextDoc l = makeContext' l asText (Just textNewlines)

context :: GingerContext SourcePos TextW Text
context = makeContextText (\key -> fromMaybe def (lookup key contextDict'))


testParse' :: String -> TextW Text
testParse' src = do
  let resolver srcName = return $ lookup srcName []
  let options = mkParserOptions resolver
  template <- either throw return =<< parseGinger' options src
      -- let write h = modifyIORef output (<> h)
      -- let context' = mContext
      --                (\key -> return $ fromMaybe def (lookup key contextDict'))
      --     -- context = context' { contextWarn = liftRun2 $ print }
  return $ runGinger context (optimize template)


-- printQ :: Function (Run SourcePos Doc Text)
-- printQ = \xs -> do
--   liftRun . pretty . unpack . mconcat . map (asText . snd) $ xs
--   return def 
  

-- docNewLines :: Newlines CDoc
-- textNewlines =
--   Newlines { splitLines = reNewline . Text.splitOn "\n"
--            , joinLines = PP.vsep
--            , stripIndent = Text.stripStart
--            , endsWithNewline = ("\n" `Text.isSuffixOf`)
--            }
