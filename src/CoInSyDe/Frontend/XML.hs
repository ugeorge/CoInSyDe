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

import Control.Exception (throw)
import Text.XML.Light
import Data.Text as T (pack,unpack,strip,lines,unlines,null)
import Data.Text.Lazy as TL (unpack)
import Text.Pretty.Simple (pShow)
import qualified Data.ByteString.Lazy as BL

type XML = Element

-- | XML 'Element' type from "Text.XML.Light".
instance FNode Element where
  getInfo      = (++) "line ". maybe "_" show . elLine
  getTxt       = T.unlines . filter (not . T.null) . map T.strip . T.lines . T.pack . strContent
  children str = findChildren (qn str)
  getStrAttr attr n =
    case findAttr (qn attr) n of
      Just a  -> Right $ pack a
      Nothing -> Left $ "cannot find attribute " ++ show attr ++
                 " in node\n " ++ (TL.unpack $ pShow n)
  getBoolAttr attr n =
    case findAttr (qn attr) n of
      Just a  -> Right $ a == "true" || a == "TRUE" || a == "yes" || a == "YES"
      Nothing -> Left $ "cannot find attribute " ++ show attr ++
                 " in node\n " ++ (TL.unpack $ pShow n)
                 
qn name = blank_name {qName=name}
  
readXML :: FilePath -> IO Element
readXML path  = BL.readFile path >>= return . maybe (throw EmptyFile) id . parseXMLDoc


