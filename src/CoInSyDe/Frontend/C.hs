{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoInSyDe.Frontend.C
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Instance of the C template frontend parser.
----------------------------------------------------------------------
module CoInSyDe.Frontend.C where

import qualified CoInSyDe.Frontend.JSON as JSON
import qualified CoInSyDe.Frontend.XML as XML

import           Control.Monad (liftM)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char
import           Data.List

cMarkup :: FilePath -> IO String
cMarkup path = do
  doc <- readFile path 
  let preamble = head $ lines doc
      format   = map toLower $ dropWhileEnd isSpace preamble
      is form  = form `isSuffixOf` format
  case (is "xml", is "json", is "yaml") of
    (True, False , False) -> return "xml"
    (False, True , False) -> return "json"
    (False, False , True) -> return "yaml"
    _ -> return ""

withCXML :: FilePath -> (XML.XML -> a) -> IO a
withCXML path f = liftM f (alter <$> BL.readFile path >>= XML.readXML)
  where alter = BL.unlines . map (BL.dropWhile (=='/')) . tail . BL.lines

withCJSON :: FilePath -> (JSON.JSON -> a) -> IO a
withCJSON path f = liftM f (alter <$> BL.readFile path >>= JSON.readJSON)
  where alter = BL.unlines . map (BL.dropWhile (=='/')) . tail . BL.lines

withCYAML :: FilePath -> (JSON.JSON -> a) -> IO a
withCYAML path f = liftM f (alter <$> BS.readFile path >>= JSON.readYAML)
  where alter = BS.unlines . map stripComments . tail . BS.lines
        stripComments line
          | "//" `BS.isPrefixOf` line = BS.dropWhile (=='/') line
          | otherwise = "    " `BS.append` line
