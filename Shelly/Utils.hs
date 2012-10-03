{-# LANGUAGE OverloadedStrings #-}
module Shelly.Utils
    ( lsF
    , move
    , canonicAbsPath
    , canonicRelPath
    )
  where

import Control.Monad(filterM)
import Prelude hiding (FilePath)
import Shelly hiding (path)
import Data.Monoid((<>))

-- | List files in the given directory.
lsF :: FilePath -> Sh [FilePath]
lsF fp = filterM test_f =<< ls fp

move :: Bool -> Bool -> FilePath -> FilePath -> Sh ()
move dryRun verbose from to = do
  from' <- canonicAbsPath from
  to' <- canonic =<< absPath to
  unless (from' == to') $ do
    when (verbose || dryRun) $ echo $ toTextIgnore from' <> " -> " <> toTextIgnore to
    unless dryRun $ mv from to

canonicAbsPath :: FilePath -> Sh FilePath
canonicAbsPath path = absPath path >>= canonic

canonicRelPath :: FilePath -> Sh FilePath
canonicRelPath path = canonic path >>= relPath

