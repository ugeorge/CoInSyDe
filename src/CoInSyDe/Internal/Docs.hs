{-# LANGUAGE OverloadedStrings #-}
module CoInSyDe.Internal.Docs where

import           Data.Aeson (toJSON, encodeFile)
import           Data.String (fromString)
import qualified Data.Text as T
import           Data.Version (Version,showVersion)
import           Text.Pandoc.Builder

import           CoInSyDe.Internal.Config

class ToDoc a where
  toDoc :: T.Text -> a -> Blocks
  
writePandocJson :: FilePath -> Pandoc -> IO ()
writePandocJson path doc = encodeFile path (toJSON doc)

-- TODO -- modification file. 
makeDoc :: String -> String -> T.Text -> String
        -> Version -> FilePath
        -> [(String, Blocks)] -> Pandoc
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
