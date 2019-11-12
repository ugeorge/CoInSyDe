{-# OPTIONS_HADDOCK hide #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Frontend.XML
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Instance of the XML frontend parser.
----------------------------------------------------------------------
module CoInSyDe.Frontend.XML where

import CoInSyDe.Frontend
import Data.Text (pack)
import Text.XML.Light
import Control.Exception (throw)
import Data.Maybe (fromMaybe)

type XML = Element

-- | XML 'Element' type from "Text.XML.Light".
instance FNode Element where
  getInfo        = (++) "line ". maybe "_" show . elLine
  getTxt         = pack . strContent
  getName        = qName . elName
  children str   = findChildren (qn str)
  readDoc        = fromMaybe (throw EmptyFile) . parseXMLDoc
  getAttr attr n =
    case findAttr (qn attr) n of
      Just a -> Right $ pack a
      Nothing -> Left $ "cannot find attribute " ++ show attr ++
                 " in node of type " ++ show (getName n)

qn name = blank_name {qName=name}
  
