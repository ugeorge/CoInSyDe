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
import Data.ByteString.Lazy (ByteString)
import Control.Exception
import Data.Typeable (Typeable)

-- | Exceptions for more meaningful frontend-related error messages.
data FrontendException
  = ParseException String String -- ^ Contains parser info + custom message
  -- | TemplateException String
  | EmptyFile
  deriving (Typeable)
instance Exception FrontendException

instance Show FrontendException where
  show (ParseException info msg) = "Parse exception (" ++ info ++ "): " ++ msg
  show EmptyFile = "Empty file!" 

-- | This is a minimal implementation for a tree-like object parser. The only type
-- needed to be wrapped is the node element specific to the frontend representation.
class Show f => FNode f where
  -- | Returns a list with all the child nodes with a certain name
  children :: String  -> f -> [f]
  -- | Returns either the value of a certain attribute or a specific error message.
  getStrAttr :: String -> f -> Either String Text
  -- | Gets the text content from a node.
  getBoolAttr :: String -> f -> Either String Bool
  -- | Gets the text content from a node.
  getTxt :: f -> Text
  -- | Gets info about element as string
  getInfo :: f -> String

-- | Infix operator for 'children'.
(|=) :: FNode f => f -> String -> [f]
node |= name = children name node

-- | Maybe-wrapped infix operator for 'getStrAttr'.
(@?) :: FNode f => f -> String -> Maybe Text
node @? attr = case getStrAttr attr node of
                 Right val -> Just val
                 Left _    -> Nothing

-- | Unsafe infix operator for 'getStrAttr'. Throws a 'ParseException' in
-- case attribute not found.
(@!) :: FNode f => f -> String -> Text
node @! attr = case getStrAttr attr node of
                 Right val -> val
                 Left  msg -> throw (ParseException (getInfo node) msg)

-- | Same as 'getBoolAttr' but with the default set to 'False'.
(@^) :: FNode f => f -> String -> Bool
node @^ attr = case getBoolAttr attr node of
                 Right val -> val
                 Left  _   -> False

-- | Same as 'children', but looks for several node names instead of just one.
childrenOf :: FNode f => [String] -> f -> [f]
childrenOf names node = concatMap (`children` node) names

-- | Predicate function for testing if an atribute exists and has a certain value.
hasValue :: FNode f => String -> String -> f -> Bool
hasValue attr val node = case getStrAttr attr node of
                           Right f -> pack val == f
                           Left  _ -> False

-- | Filters a list of node based on a 'hasValue' predicate.
filterByAttr :: FNode f => String -> String -> [f] -> [f]
filterByAttr attr val = filter (attr `hasValue` val)

