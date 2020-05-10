{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
-- |
-- Module      :  CoinSyDe.Internal.Config
-- Copyright   :  (c) George Ungureanu, 2019
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  ugeorge@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- As exchange format for documentation, CoInSyDe uses
-- <https://hackage.haskell.org/package/pandoc-types pandoc-types>. This module
-- provides a few utilities around "Text.Pandoc.Builder" to dump CoInSyDe AST into
-- Pandoc format.
------------------------------------------------------------------------
module CoInSyDe.Internal.Docs where

import           Data.Aeson (toJSON, encodeFile)
import           Data.String (fromString)
import qualified Data.Text as T
import           Data.Version (Version,showVersion)
import           Text.Pandoc.Builder

import           CoInSyDe.Internal.Config

-- | Whatever instantiates this class can be exported to Pandoc. Used to build
-- documentations.
class ToDoc a where
  toDoc :: T.Text -- ^ prefix added to link anchors
        -> a
        -> Blocks
  
-- | Compiles a list of 'Blocks' into a Pandoc document.
makeDoc :: String             -- ^ document title
        -> String             -- ^ project name
        -> T.Text             -- ^ project target
        -> String             -- ^ compilation time, converted into a string format
        -> Version            -- ^ CoInSyDe version
        -> FilePath           -- ^ Workspace root
        -> [(String, Blocks)] -- ^ list of (Section title, 'Blocks')
        -> Pandoc
makeDoc title proj target time ver wroot content
  = setTitle (fromString title)
  $ setDate (fromString time)
  $ doc $ foldr1 (<>) $ [ header 1 (fromString title) , preamble ]
  ++ map (\(n,c) -> header 2 (fromString n) <> c) content
  where
    preamble = simpleTable []
      [ [btext "Project", btext $ fromString proj]
      , [btext "Target", plain $ code target]
      , [btext "Date", plain $ code $ fromString time]
      , [btext "Workspace", plain $ code $ fromString wroot]
      , [btext "Made with", btext "CoInSyDe v."
                            <> btext (fromString $ showVersion ver)]
      ]

-- | Writes a compiled 'Pandoc' into a JSON file, that can further be converted to a
-- useful format using the @pandoc@ CLI tool. For more info see the
-- <https://pandoc.org/ Pandoc website>.
writePandocJson :: FilePath -> Pandoc -> IO ()
writePandocJson path doc = encodeFile path (toJSON doc)


cpAnchor name = ("cp" ++ name, ["def"], [])
tyAnchor name = ("ty" ++ name, ["def"], [])

boldtext     = plain . strong . text
btext        = plain . text
rowTab       = simpleTable [] . map (:[])
rowTabWith f = simpleTable [] . map ((:[]) . f)

catMap f = foldr1 (<>) . map f

ilink pref l = link ("#" `T.append` pref `T.append` l) l (code l)
ibracks b = text "[" <> b <> text "]"
iparens b = text "(" <> b <> text ")"
iangles b = text "<" <> b <> text ">"
ibold = strong . text
