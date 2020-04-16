{-# LANGUAGE FlexibleInstances #-}
module CoInSyDe.Internal.YAML where

import           Control.Monad (liftM)
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import           Data.YAML
import           System.Exit

-- | Convenience wrapper for a Yaml document, for nicely printing out
-- parser errors.
data YDoc = YDoc { yamlPath :: FilePath
                 , yamlText :: BL.ByteString
                 , yamlRoot :: YNode
                 }

type YNode = Node Pos

-- | Gets the position info of a node.
getPos :: YNode -> Parser Pos
getPos (Scalar loc _) = return loc
getPos (Mapping loc _ _) = return loc
getPos (Sequence loc _ _) = return loc
getPos (Anchor loc _ _) = return loc 

-- | Returns children /nodes/ without attributes.
getChildren :: Text -> YNode -> Parser [YNode]
getChildren str = withMap "parent node" $ \ o -> do
  ret <- o .:? str
  case ret of
    Just m@Mapping{}      -> return [m]
    Just (Sequence _ _ l) -> return $ filter isMap l
    Just n                -> typeMismatch "is an attribute, not a node!" n
    Nothing               -> return []
  where isMap Mapping{} = True
        isMap _ = False

-- file methods --------------------------

readYDoc :: FilePath -> IO YDoc
readYDoc p = BL.readFile p >>= \s ->
  either (die . inStyle p s) (return . YDoc p s . getRoot . head) (decodeNode s)
  where getRoot r = case docRoot r of
          r@Mapping{} -> r
          _ -> error $ " Parse error in file\n++++ " ++ p
                    ++ "\nDocument is not a dictionary."
        inStyle path inp (pos,msg) = prettyPosWithSource pos inp
          (" YAML parse error in file\n\t+++ " ++ path ++ "\n" ) ++ msg ++ "\n"

withYDoc :: FilePath -> (YDoc -> a) -> IO a
withYDoc path f = liftM f (readYDoc path)

parseYDoc :: YDoc -> Parser a -> IO a
parseYDoc doc = either (die . prettyErr doc) return  . parseEither

writeYAML :: ToYAML v => FilePath -> v -> IO ()
writeYAML path = BL.writeFile path . encode1

-- | Pretty-prints a parser error
prettyErr :: YDoc -> (Pos,String) -> String
prettyErr (YDoc f s _) (pos,msg) = prettyPosWithSource pos s
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
