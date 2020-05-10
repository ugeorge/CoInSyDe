{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Internal.YAML
-- Copyright   :  (c) George Ungureanu, 2019-2020
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains wrappers and utilities around "Data.YAML" functions, and
-- it is used throughout CoInSyDe as API for data interchange.
----------------------------------------------------------------------
module CoInSyDe.Internal.YAML (
  -- * Types
  YDoc(..), YMap, YNode, YPos, YParse, ToYAML(..),
  -- * Getters and utilities
  getPos, getLineAndColumn, getChildren, (|=),
  getNode, queryNode, (@!), (@?), (@=), (@^), nodeToMap, combineNode,
  -- * Encoding, decoding and parsing utilities
  parseYText, yamlError, prettyErr, prettyYNode,
  readYDoc, withYDoc, fromMeta, parseYDoc, writeYAML,
  -- * Source code container
  YSrcCode(..), ysrcCode, ysrcFromText, prettyYSrcWithOffset
  ) where

import           Control.Monad (liftM)
import           Data.Binary
import qualified Data.ByteString.Lazy as S
import qualified Data.ByteString.Lazy.Char8 as SC (unpack)
import           Data.Default
import           Data.Either
import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Scientific (fromFloatDigits)
import           Data.Text as T (Text,lines)
import           Data.Text.Encoding (encodeUtf8)
import           GHC.Generics
import           Data.Word (Word8)
import           Data.YAML
import           Data.YAML.Event (Tag)
import           System.Exit
import           Text.Ginger.GVal

-- | Convenience wrapper for a Yaml document, for nicely printing out
-- parser errors.
data YDoc = YDoc { yamlPath :: FilePath
                 , yamlText :: S.ByteString
                 , yamlMeta :: Maybe YMap
                 , yamlRoot :: YMap
                 }

-- | Convenience alias for a @Mapping@ 'Node' along with its Id.
type YMap   = (Text, Node Pos)
type YNode  = Node Pos
type YPos   = Pos
type YParse = Parser

-- | can be dumped to binary file
instance Binary Pos
-- | can be dumped to binary file
instance Binary Scalar
-- | can be dumped to binary file
instance Binary Tag
-- | can be dumped to binary file
instance Binary (Node Pos)

--------------------------------------------------------------------------

-- | Gets the position info of a node.
getPos :: YNode -> Parser Pos
getPos (Scalar loc _) = return loc
getPos (Mapping loc _ _) = return loc
getPos (Sequence loc _ _) = return loc
getPos (Anchor loc _ _) = return loc

-- | If you don't want to pattern-match a 'Pos'
getLineAndColumn :: YMap -> (Int,Int)
getLineAndColumn (_, Mapping loc _ _) = (posLine loc, posColumn loc)

traverseMap f (n,mp) = withMap (show n ++ " node") f mp 

-- | Returns a list of children /nodes/, no attributes i.e. @Mapping@ and @Sequence@
-- 'Node's.
getChildren :: Text -> YMap -> Parser [YMap]
getChildren str = traverseMap $ \o -> do
  ret <- o .:? str
  case ret of
    Just m@Mapping{}      -> return [(str,m)]
    Just (Sequence _ _ l) -> return $ map ((,) str) $ filter isMap l
    Just n                -> typeMismatch "is an attribute, not a node!" n
    Nothing               -> return []
  where isMap Mapping{} = True
        isMap _ = False

-- | Infix for 'getChildren'
(|=) :: YMap -> Text -> Parser [YMap]
(|=) = flip getChildren

-- | Returns the child node at a specific entry.
getNode ::FromYAML a => Text -> YMap -> Parser (Maybe a)
getNode attr = traverseMap (.:? attr)

-- | Infix for 'getNode'.
(@?) :: FromYAML a => YMap -> Text -> Parser (Maybe a)
(@?) = flip getNode

-- | Like '@?' but throws a specific parser error if key is not found.
(@!) :: FromYAML a => YMap -> Text -> Parser a
m @! attr = traverseMap (.: attr) m

-- | Returns the position of node at a given key. Throws a parser error if key is not
-- found.
(@^) :: YMap -> Text -> Parser Pos
m @^ attr = traverseMap (.: attr) m >>= getPos

-- | Sets the default value in case a previous '@?' operation returns 'Nothing'.
(@=) :: FromYAML a => Parser (Maybe a) -> a -> Parser a
m @= v = fromMaybe v <$> m

-- | Throws a parser error at a specified node location.
yamlError :: YMap -> String -> Parser a 
yamlError (_,n) = failAtNode n

-- | Returns a node based on its complete path relative to one of its ancestors.
queryNode :: FromYAML a => [Text] -> YMap -> Parser (Maybe a)
queryNode []     n = return Nothing
queryNode [q]    n = n @? q
queryNode (q:qs) n = n @? q >>= \ret -> case ret of
  Just (m@Mapping{}) -> queryNode qs (q,m)
  _                  -> return Nothing
   where
    query' []     n = return $ Just n
    query' (q:qs) n = case n of
      Mapping{} -> withMap "" (.:? q) n >>= maybe (return Nothing) (queryNode qs)
      _         -> return Nothing

-- | Pure conversion between a YAML @Mapping@ node and a
-- 'CoInSyDe.Internal.Map.Map'. In case of a Left return value, it contains an error
-- message telling why the conversion failed.
nodeToMap :: Show a => Node a -> Either String (H.HashMap Text (Node a))
nodeToMap (Mapping _ _ m) =
  let (err,mp) = partitionEithers $ map checkKey $ M.toList m
  in case err of
       [] -> Right $ H.fromList mp
       e  -> Left $ unlines e
  where checkKey (Scalar _ (SStr k),v) =  Right (k,v)
        checkKey (n,_) = Left $ "YAML key " ++ show n
                         ++ " is not text. Cannot convert to map!\n"
nodeToMap _ = Left $ "Node is not a mapping"

-- | performs a recursive union of two nodes. if two mappings contain the same keys at
-- the same locations, then the first one overrides the second.
combineNode :: Node () -> Node () -> Node ()
combineNode m@(Mapping _ _ mm) h@(Mapping _ _ mh) =
  toYAML $ M.unionWith combineNode mm mh
combineNode m h = m

-- file methods --------------------------

fromMeta :: FromYAML v => (YMap -> Parser (Maybe v)) -> YMap -> IO (Maybe v)
fromMeta f meta = either (die . message) return $ parseEither (f meta) 
  where message (_,msg) = "Error in parsing file metadata:\n" ++ msg
    
handleMeta :: FilePath -> S.ByteString -> IO (S.ByteString,Maybe YMap)
handleMeta p bs = do
  let newline   = toEnum $ fromEnum '\n' :: Word8
      space     = toEnum $ fromEnum ' '  :: Word8
      (lsm,lsc) = span (/= "---") $ S.split newline bs
  if null lsc then return (bs,Nothing)
    else do
    let metaStr = S.intercalate "\n" lsm
    meta <- either (die . inStyle p metaStr) (return . (,)"metaRoot" . getRoot . head)
            $ decodeNode metaStr
    pref <- fromMeta (queryNode ["literate", "prefix"]) meta :: IO (Maybe Text)
    offs <- fromMeta (queryNode ["literate", "offset"]) meta
    let offset = fromMaybe 6 offs
        content = S.intercalate "\n" $ 
          case fmap (S.fromStrict . encodeUtf8) pref of
            Nothing -> lsc
            Just pr -> let alignCode l = S.replicate offset space `S.append` l
                       in  map (\l -> fromMaybe (alignCode l) (S.stripPrefix pr l))
                           (map (const "") lsm ++ tail lsc)
    return (content, Just meta)
      where
        getRoot r = case docRoot r of
          r@Mapping{} -> r
          _ -> error $ " Error parsing metadata in file \n++++ " ++ p
        inStyle path inp (pos,msg) = prettyPosWithSource pos inp
          (" YAML error in metadata for\n\t+++ " ++ path ++ "\n" ) ++ msg ++ "\n"

-- | Reads and parses a YAML document from a specified file. It also extracts info in
-- the meta preamble, and translates from literate code if it is the case.
readYDoc :: FilePath -> IO YDoc
readYDoc p = S.readFile p >>= handleMeta p >>= \(s,m) ->
  either (die . inStyle p s) (return . YDoc p s m . getRoot . head) (decodeNode s)
  where
    getRoot r = case docRoot r of
      r@Mapping{} -> ("document", r)
      _ -> error $ " Parse error in\n++++ " ++ p ++ "\nDocument is not a dictionary."
    inStyle path inp (pos,msg) = prettyPosWithSource pos inp
      (" YAML parse error in file\n\t+++ " ++ path ++ "\n" ) ++ msg ++ "\n"

-- | Parses a YAML document and returns its root node in a pure environment. Returns
-- an error message in case the parsing has failed.
parseYText :: Text -> Either String (Node ())
parseYText x = case (docRoot . head) <$> decodeNode bssrc of
  Left (pos,err) -> Left $ prettyPosWithSource pos bssrc err
  Right n -> Right $ toYAML n 
  where bssrc = S.fromStrict $ encodeUtf8 x

-- | Applies a pure action on a YAML document after parsing it from a file.
withYDoc :: FilePath -> (YDoc -> a) -> IO a
withYDoc path f = liftM f (readYDoc path)

-- | Parses a YAML document.
parseYDoc :: YDoc -> YParse a -> IO a
parseYDoc doc = either (die . prettyErr doc) return  . parseEither

-- | Encodes and dumps a YAML structure into a file
writeYAML :: ToYAML v => FilePath -> v -> IO ()
writeYAML path = S.writeFile path . encode1

-- | Pretty-prints a parser error
prettyErr :: YDoc -> (Pos,String) -> String
prettyErr (YDoc f s _ _) (pos,msg) = prettyPosWithSource pos s
  (" Parse error in file\n++++ " ++ f ++ "" ) ++ msg ++ "\n"

-- | Pretty-shows and encoded YAML node.
prettyYNode :: Node () -> String
prettyYNode = SC.unpack . encode1

-- | can be converted to Ginger's 'GVal'
instance Show a => ToGVal m (Node a) where
  toGVal (Scalar _ SNull)      = def
  toGVal (Scalar _ (SBool b))  = toGVal b
  toGVal (Scalar _ (SFloat d)) = toGVal (fromFloatDigits d)
  toGVal (Scalar _ (SInt i))   = toGVal i
  toGVal (Scalar _ (SStr t))   = toGVal t
  toGVal (Sequence _ _ lst)    = toGVal lst
  toGVal (Anchor _ _ _)        = def
  toGVal (Mapping _ _ m)       = toGVal $
    H.fromList $ map (\(k,v) -> (ynodeToText k, v)) $ M.toList m
    where ynodeToText (Scalar _ (SStr t))   = t
          ynodeToText n = error $ "YAML node is not text. Cannot convert to GVal!\n"
                          ++ show n
  toGVal _ = def -- TODO: is it OK?

-----------------------------------------------

-- | A wrapper for template source code found in a YAML node. 
data YSrcCode = YSrc { ysrcPath  :: FilePath -- ^ which file contains the code
                     , ysrcPos   :: Pos      -- ^ at what position it is found
                     , yPreamble :: Text     -- ^ preamble added in post-processing
                     , ysrcMain  :: Text     -- ^ the main template code
                     } deriving (Show,Generic)
-- | can be dumped to binary file.
instance Binary YSrcCode

-- | Code that needs to be evaluated, i.e. @yPreamble ++ ysrcMain@.
ysrcCode :: YSrcCode -> Text
ysrcCode src = yPreamble src <> ysrcMain src

-- | Pretty-prints an error message after recalculating the coordinate in the original
-- source YAML file.
prettyYSrcWithOffset :: YSrcCode -> String -> (Int, Int) -> String
prettyYSrcWithOffset (YSrc path (Pos _ _ l c) pream code) msg (offsL,offsC) =
  shpath ++ "\n" ++ prettyPosWithSource newpos srcbs msg
  where
    shpath = if null path then "" else show path ++ " (" ++ show (l + offsL) ++ ":"
                                       ++ show (c + offsC) ++ ")\n"
    newpos = Pos coffs coffs (l + offsL - hPream) (offsC + 3)
    srcbs  = S.fromStrict $ encodeUtf8 code
    coffs  = let clines = take offsL $ S.split (ch2w '\n') srcbs
             in fromIntegral $ sum $ map S.length clines
    ch2w   = toEnum . fromEnum
    hPream = Prelude.length $ T.lines pream

-- | Template wrapper from given preamble and text
ysrcFromText :: Text -> Text -> YSrcCode
ysrcFromText = YSrc "" (Pos (-1) (-1) 0 0)
