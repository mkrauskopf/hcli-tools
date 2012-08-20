{-# LANGUAGE OverloadedStrings #-}

module CLI.Tool.Normalizer (normalizePath) where

import Prelude as P hiding (FilePath)
import Data.Text.Lazy as LT hiding (foldl, map)
import Shelly
import Data.Char
import Filesystem.Path(filename, directory)
default (LT.Text)

-- Normalizes filename part of the given path.
normalizePath :: FilePath -- ^ path to file to be normalized
              -> FilePath -- ^ normalized path
normalizePath filePath = directory filePath </> normalized
  where
    fname = toTextIgnore . filename $ filePath
    normalized = fromText . underscore $ foldl translate fname table

translate :: Text -> (Text, Text) -> Text
translate text (f, t) = replace f t text

underscore :: Text -> Text
underscore text = pack $ map f (unpack text)
  where
    f :: Char -> Char
    f c = if not $ isStandard c then '_' else c
    isStandard :: Char -> Bool
    isStandard c = isAlphaNum c || c `elem` "._-"

-- Characters mapping table.
table :: [ (Text, Text) ] 
table = [ ("á", "a")
        , ("é", "e")
        , ("í", "i")
        , ("ú", "u")
        , ("ý", "y")
        , ("č", "c")
        , ("ě", "e")
        , ("ř", "r")
        , ("š", "s")
        , ("ů", "u")
        , ("ž", "z")
        , ("Á", "A")
        , ("É", "E")
        , ("Í", "I")
        , ("Ú", "U")
        , ("Ý", "Y")
        , ("Č", "C")
        , ("Ě", "E")
        , ("Ř", "R")
        , ("Š", "S")
        , ("Ů", "U")
        , ("Ž", "Z")
        ]

