{-# LANGUAGE FlexibleInstances #-}
module CoInSyDe.Internal.YAML where

import           Control.DeepSeq
import           Control.Monad (liftM, filterM)
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Lazy.Internal (packChars)
import qualified Data.Text as T
import           Data.YAML
import           System.Exit

-- | Convenience wrapper for a Yaml document, for nicely printing out
-- parser errors.
data YamlDoc = YamlDoc { yamlPath :: FilePath
                       , yamlText :: BL.ByteString
                       , yamlRoot :: YamlNode
                       }

type YamlNode = Node Pos

-- getters -----------------

-- | Gets the position info of a node.
getPos :: YamlNode -> Parser Pos
getPos (Scalar loc _) = return loc
getPos (Mapping loc _ _) = return loc
getPos (Sequence loc _ _) = return loc
getPos (Anchor loc _ _) = return loc 

-- | Returns children /nodes/ without attributes.
getChildren :: String -> YamlNode -> Parser [YamlNode]
getChildren str = withMap "parent node" $ \ o -> do
  ret <- o .:? T.pack str
  case ret of
    Just m@Mapping{}      -> return [m]
    Just (Sequence _ _ l) -> return $ filter isMap l
    Just n                -> typeMismatch "is an attribute, not a node!" n
    Nothing               -> return []
  where isMap Mapping{} = True
        isMap _ = False

-- | Infix operator for 'getChildren'.
(|=) :: YamlNode -> String -> Parser [YamlNode]
node |= name = getChildren name node

(@!) :: FromYAML v => YamlNode -> String -> Parser v
node @! attr = withMap "parent node" (\n -> n .: T.pack attr) node

(@?) :: FromYAML v => YamlNode -> String -> Parser (Maybe v)
node @? attr = withMap "parent node" (\n -> n .:? T.pack attr) node

-- | Same as 'getChildren', but looks for several node names instead of just one.
childrenOf :: [String] -> YamlNode -> Parser [YamlNode]
childrenOf names node = concat <$> mapM (`getChildren` node) names

-- | Returns the text of an attribute. If it does not exist, then it
-- returns n empty string.
getText :: String -> YamlNode -> Parser T.Text
getText str = withMap "parent node" $ \o -> do
  ret <- o .:? T.pack str
  case ret of
    Just (Scalar _ (SStr t)) -> return t
    Just n                   -> typeMismatch "does not contain text info!" n
    Nothing                  -> return (T.pack "")

-- | Predicate function for testing if an atribute exists and has a certain value.
hasValue :: String -> String -> YamlNode -> Parser Bool
hasValue attr val node = liftM (\v -> T.pack val == v) (getText attr node) 
  
-- | Filters a list of node based on a 'hasValue' predicate.
filterByAttr :: String -> String -> [YamlNode] -> Parser [YamlNode]
filterByAttr attr val = filterM (attr `hasValue` val) 


-- file methods --------------------------

readYAML :: FilePath -> IO YamlDoc
readYAML p = BL.readFile p >>= \s ->
  either (die . inStyle p s) (return . YamlDoc p s . getRoot . head) (decodeNode s)
  where getRoot r = case docRoot r of
          r@Mapping{} -> r
          _ -> error $ " YAML parse error in file\n\t+++ " ++ p
                    ++ "\nDocument is not a dictionary."
        inStyle path inp (pos,msg) = prettyPosWithSource pos inp
          (" YAML parse error in file\n\t+++ " ++ path ++ "\n" ) ++ msg ++ "\n"

withYAML :: FilePath -> (YamlDoc -> a) -> IO a
withYAML path f = liftM f (readYAML path)

writeYAML :: ToYAML v => FilePath -> v -> IO ()
writeYAML path = BL.writeFile path . encode1

-- | Pretty-prints a parser error
prettyErr :: YamlDoc -> (Pos,String) -> String
prettyErr (YamlDoc f s _) (pos,msg) = prettyPosWithSource pos s
  (" YAML parse error in file\n\t+++ " ++ f ++ "\n" ) ++ msg ++ "\n"

instance Read (Node Pos) where
  readsPrec p str = [ (makeNode x, y) | (x, y) <- readsPrec p str ]
    where makeNode = either (error . show) id . decode1 . packChars

instance NFData (Node Pos) where
  rnf (Scalar l s)     = rnf l `seq` rnf s
  rnf (Mapping l _ m)  = rnf l `seq` rnf m
  rnf (Sequence l _ s) = rnf l `seq` rnf s
  rnf (Anchor l _ a)   = rnf l `seq` rnf a

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
