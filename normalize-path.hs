{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import CLI.Tool.Normalizer(normalizePath)
import Prelude as P hiding (FilePath)
import Shelly
import Shelly.Utils
import System.Console.CmdArgs.Implicit
import Control.Monad(zipWithM_)
import Data.String(fromString)

data CmdOptions = CmdOptions {
  verbose :: Bool,
  dryRun  :: Bool,
  files   :: [String]
} deriving (Show,Data,Typeable)

main :: IO ()
main = shelly $ do
  CmdOptions{..} <- liftIO $ cmdArgs CmdOptions {dryRun=False, verbose=False, files=def&=args}
  when (P.null files) $ errorExit "No paths passed for normalizing"
  let filePaths = map fromString files
  let normPaths = map normalizePath filePaths
  zipWithM_ (move dryRun verbose) filePaths normPaths

