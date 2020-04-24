{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings #-}
module CoInSyDe.Internal.YAML where

import           Control.Monad (liftM,when)
import qualified Data.ByteString.Lazy as S
import           Data.Maybe
import           Data.Binary
import           Data.Text (Text,append)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Word (Word8)
import           Data.YAML
import           Data.YAML.Event (Tag)
import           System.Exit

-- | Convenience wrapper for a Yaml document, for nicely printing out
-- parser errors.
data YDoc = YDoc { yamlPath :: FilePath
                 , yamlText :: S.ByteString
                 , yamlMeta :: Maybe YNode
                 , yamlRoot :: YMap
                 }

type YMap   = (Text, Node Pos)
type YNode  = Node Pos
type YPos   = Pos
type YParse = Parser

instance Binary Pos
instance Binary Scalar
instance Binary Tag
instance Binary (Node Pos)

--------------------------------------------------------------------------

-- | Gets the position info of a node.
getPos :: YNode -> Parser Pos
getPos (Scalar loc _) = return loc
getPos (Mapping loc _ _) = return loc
getPos (Sequence loc _ _) = return loc
getPos (Anchor loc _ _) = return loc

getLineAndColumn :: YMap -> (Int,Int)
getLineAndColumn (_, Mapping loc _ _) = (posLine loc, posColumn loc)


traverseMap f (n,mp) = withMap (show n ++ " node") f mp 

-- | Returns children /nodes/ without attributes.
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

(|=) :: YMap -> Text -> Parser [YMap]
(|=) = flip getChildren

getAttr ::FromYAML a => Text -> YMap -> Parser (Maybe a)
getAttr attr = traverseMap (.:? attr)
  
(@?) :: FromYAML a => YMap -> Text -> Parser (Maybe a)
(@?) = flip getAttr

(@!) :: FromYAML a => YMap -> Text -> Parser a
m @! attr = traverseMap (.: attr) m

(@^) :: YMap -> Text -> Parser Pos
m @^ attr = traverseMap (.: attr) m >>= getPos

(@=) :: FromYAML a => Parser (Maybe a) -> a -> Parser a
m @= v = fromMaybe v <$> m

yamlError (_,n) = failAtNode n

queryNode :: FromYAML a => [Text] -> YNode -> Parser (Maybe a)
queryNode l n = query' l n >>= maybe (return Nothing) parseYAML
  where
    query' []     n = return $ Just n
    query' (q:qs) n = case n of
      Mapping{} -> withMap "" (.:? q) n >>= maybe (return Nothing) (queryNode qs)
      _         -> return Nothing

-- getTextPos (Scalar p _) = p
-- getTextPos a = error "Not a text node!" 

-- parseMapWith :: FromYAML v => String -> String
--              -> (Mapping Pos -> Parser v) -> YNode -> Parser [v]
-- parseMapWith name desc f = withSeq desc (mapM (withMap name f))

-- infixl 9 >>=?
-- (>>=?) :: Monad m => m (Maybe a) -> (a -> m b) -> m (Maybe b) 
-- a >>=? b = a >>= maybe (return Nothing) (fmap Just . b)

-- file methods --------------------------

handleMeta :: FilePath -> S.ByteString -> IO (S.ByteString,Maybe YNode)
handleMeta p bs = do
  let newline   = toEnum $ fromEnum '\n' :: Word8
      space     = toEnum $ fromEnum ' '  :: Word8
      (lsm,lsc) = span (S.isPrefixOf "--") $ S.split newline bs
      metaStr   = S.intercalate "\n" $ map (fromJust . S.stripPrefix "--") lsm
  if S.null metaStr then return (bs,Nothing)
    else do
    meta <- either (die . inStyle p metaStr) (return . getRoot . head)
            $ decodeNode metaStr
    let fromMeta f = either (die . inStyle p metaStr) return $ parseEither (f meta)  
    pref <- fromMeta (queryNode ["literate", "prefix"]) :: IO (Maybe Text)
    offs <- fromMeta (queryNode ["literate", "offset"])
    let offset = fromMaybe 10 offs
        content = S.intercalate "\n" $ case fmap (S.fromStrict . encodeUtf8) pref of
          Nothing -> lsc
          Just pr -> let alignCode l = S.replicate offset space `S.append` l
                     in  map (\l -> fromMaybe (alignCode l) (S.stripPrefix pr l)) lsc
    return (content, Just meta)
      where
        getRoot r = case docRoot r of
          r@Mapping{} -> r
          _ -> error $ " Error parsing metadata in file \n++++ " ++ p
        inStyle path inp (pos,msg) = prettyPosWithSource pos inp
          (" YAML error in metadata for\n\t+++ " ++ path ++ "\n" ) ++ msg ++ "\n"

readYDoc :: FilePath -> IO YDoc
readYDoc p = S.readFile p >>= handleMeta p >>= \(s,m) ->
  either (die . inStyle p s) (return . YDoc p s m . getRoot . head) (decodeNode s)
  where getRoot r = case docRoot r of
          r@Mapping{} -> ("document", r)
          _ -> error $ " Parse error in file\n++++ " ++ p
                    ++ "\nDocument is not a dictionary."
        inStyle path inp (pos,msg) = prettyPosWithSource pos inp
          (" YAML parse error in file\n\t+++ " ++ path ++ "\n" ) ++ msg ++ "\n"

withYDoc :: FilePath -> (YDoc -> a) -> IO a
withYDoc path f = liftM f (readYDoc path)

parseYDoc :: YDoc -> Parser a -> IO a
parseYDoc doc = either (die . prettyErr doc) return  . parseEither

writeYAML :: ToYAML v => FilePath -> v -> IO ()
writeYAML path = S.writeFile path . encode1

-- | Pretty-prints a parser error
prettyErr :: YDoc -> (Pos,String) -> String
prettyErr (YDoc f s _ _) (pos,msg) = prettyPosWithSource pos s
  (" Parse error in file\n++++ " ++ f ++ "" ) ++ msg ++ "\n"

-- instance Read (Node Pos) where
--   readsPrec p str = [ (makeNode x, y) | (x, y) <- readsPrec p str ]
--     where makeNode = either (error . show) id . decode1 . packChars

-- instance NFData (Node Pos) where
--   rnf (Scalar l s)     = rnf l `seq` rnf s
--   rnf (Mapping l _ m)  = rnf l `seq` rnf m
--   rnf (Sequence l _ s) = rnf l `seq` rnf s
--   rnf (Anchor l _ a)   = rnf l `seq` rnf a


--- TODO: make Node an instance of Ginger.GVal, e.g.
-- -- | Convert Aeson 'Value's to 'GVal's over an arbitrary host monad. Because
-- -- JSON cannot represent functions, this conversion will never produce a
-- -- 'Function'. Further, the 'ToJSON' instance for such a 'GVal' will always
-- -- produce the exact 'Value' that was use to construct the it.
-- instance ToGVal m JSON.Value where
--     toGVal j = (rawJSONToGVal j) { asJSON = Just j }

-- rawJSONToGVal :: JSON.Value -> GVal m
-- rawJSONToGVal (JSON.Number n) = toGVal n
-- rawJSONToGVal (JSON.String s) = toGVal s
-- rawJSONToGVal (JSON.Bool b) = toGVal b
-- rawJSONToGVal JSON.Null = def
-- rawJSONToGVal (JSON.Array a) = toGVal $ Vector.toList a
-- rawJSONToGVal (JSON.Object o) = toGVal o
