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

instance FNode Element where
  children str = findChildren (qn str)
  getAttr attr n =
    case findAttr (qn attr) n of
      Just a -> Left $ pack a
      Nothing -> Right $ "XML Line " ++ show (elLine n) ++
                 ": Cannot find attribute '" ++ attr ++
                 "' in node of type '" ++ qName (elName n) ++ "'!"

qn name = blank_name {qName=name}
  
