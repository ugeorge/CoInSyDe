----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Frontend
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains a common wrapper API for reading different
-- tree-based frontend languages, such as XML or JSON.
----------------------------------------------------------------------
module CoInSyDe.Frontend where

import Data.Text (Text,pack)
import Data.ByteString (ByteString)

-- | This is a minimal implementation for a tree-like object parser. The only type
-- needed to be wrapped is the node element specific to the frontend representation.
class FNode f where
  -- | Returns the root node from a document file. 'FilePath' passed for error message
  -- only.
  readDoc  :: FilePath -> ByteString -> f
  -- | Returns a list with all the child nodes with a certain name
  children :: String  -> f -> [f]
  -- | Get name
  getName :: f -> String
  -- | Returns either the value of a certain attribute or a specific error message.
  getAttr  :: String -> f -> Either Text String
  -- | Gets the text content from a node.
  txtContent :: f -> Text

-- | Infix operator for 'children'.
(|=) :: FNode f => f -> String -> [f]
node |= name = children name node

-- | Maybe-wrapped infix operator for 'getAttr'.
(@?) :: FNode f => f -> String -> Maybe Text
node @? attr = case getAttr attr node of
                 Left val -> Just val
                 Right _  -> Nothing

-- | Unsafe infix operator for 'getAttr'. Throws a runtime error in case attribute not
-- found.
(@!) :: FNode f => f -> String -> Text
node @! attr = case getAttr attr node of
                 Left  val -> val
                 Right msg -> error msg

-- | Same as 'children', but looks for several node names instead of just one.
childrenOf :: FNode f => [String] -> f -> [f]
childrenOf names node = concatMap (`children` node) names

-- | Predicate function for testing if an atribute exists and has a certain value.
hasValue :: FNode f => String -> String -> f -> Bool
hasValue attr val node = case getAttr attr node of
                           Left f  -> pack val == f
                           Right _ -> False

-- | Filters a list of node based on a 'hasValue' predicate.
filterByAttr :: FNode f => String -> String -> [f] -> [f]
filterByAttr attr val = filter (attr `hasValue` val)


-- -- | @allAttrOf el attr root@ returns a list with all values for attrigute @attr@
-- -- belonging to all children named @el@. Unsafe, assumes attributes exists already.
-- allAttrOf :: FNode f => String -> String -> f -> [Text]
-- allAttrOf el attr = map (@!=attr) . children el
