{-# LANGUAGE OverloadedStrings #-}

module Shelly.Debug
    ( echoPaths
    )
  where

import Data.Text.Lazy(intercalate)
import Prelude hiding (FilePath)
import Shelly

echoPaths :: [FilePath] -> Sh ()
echoPaths = echo . intercalate "\n" . map toTextIgnore

