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
import           Data.Maybe

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
  where alter = BL.unlines . map stripComments . tail . BL.lines
        reFormat ln = BL.drop 3 $ fromMaybe ln $ BL.stripSuffix "*/" ln
        stripComments line
          | "///" `BL.isPrefixOf` line || "/**" `BL.isPrefixOf` line
          = reFormat line
          | otherwise = line

withCJSON :: FilePath -> (JSON.JSON -> a) -> IO a
withCJSON path f = liftM f (alter <$> BL.readFile path >>= JSON.readJSON)
  where alter = foldl stripComments "" . tail . BL.lines
        reFormat ln = BL.drop 3 $ fromMaybe ln $ BL.stripSuffix "*/" ln
        stripComments buff line
          | "///" `BL.isPrefixOf` line || "/**" `BL.isPrefixOf` line
          = buff +++ "\n" +++ reFormat line
          | otherwise = buff +++ "\\n" +++ line
        
withCYAML :: FilePath -> (JSON.JSON -> a) -> IO a
withCYAML path f = liftM f (alter <$> BS.readFile path >>= JSON.readYAML)
-- withCYAML path f = alter <$> BS.readFile path
  where alter = getBuff . foldl stripComments (False,0,-2,"") . tail . BS.lines
        getBuff (_,_,_,b) = b
        reFormat n ln = BS.drop (3 + n) $ fromMaybe ln $ BS.stripSuffix "*/" ln
        stripComments (False,_,n,buff) line
          | "///" `BS.isPrefixOf` line || "/**" `BS.isPrefixOf` line
          = (True,  nSpaces line, 0, buff +-+ "\n" +-+ reFormat (nSpaces line) line)
          | otherwise
          = (False, 0,            n, buff +-+ "\n" +-+ BS.replicate (n+2) ' ' +-+ line)
        stripComments (True,n,c,buff) line
          | "///" `BS.isPrefixOf` line || "/**" `BS.isPrefixOf` line
          = (True,  n, nSpaces line, buff +-+ "\n" +-+ reFormat n line)
          | otherwise
          = (False, 0,            c, buff +-+ "\n" +-+ BS.replicate (c+2) ' ' +-+ line)

----------------------------------------------------------------

infixr 5 +-+, +++
(+-+) = BS.append
(+++) = BL.append
nSpaces = countSpaces 0 . BS.drop 3
  where countSpaces n xs
          | " " `BS.isPrefixOf` xs = countSpaces (n+1) (BS.tail xs)
          | otherwise = n 
