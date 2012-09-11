{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude as P hiding (FilePath)
import Data.Text.Lazy as LT
import Shelly
import Shelly.Utils
import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Utils
import Data.Monoid((<>))
import Filesystem.Path(filename)
import Data.Int(Int64)
import Control.Monad(zipWithM_)

nfVersion, nfCopyright, nfProgram, nfSummary :: String
nfVersion   = "0.1.1"
nfCopyright = "2012"
nfProgram   = "number-files"
nfSummary   = nfProgram ++ " v" ++ nfVersion ++ ", (C) Martin Krauskopf " ++ nfCopyright

data CmdOptions = CmdOptions
  { dir     :: String
  , step    :: Int
  , lstrip  :: Int64
  , verbose :: Bool
  , dryRun  :: Bool
  } deriving (Show,Data,Typeable)

defOpts :: CmdOptions
defOpts = CmdOptions
  { dir     = ""    &= argPos 0 &= typDir
  , step    = 1     &= help "Numbering step. Default to 1."
  , lstrip  = 0     &= help "Number of initial(left) characters to strip from original filename"
                    &= explicit &= name "strip" &= name "S"
  , dryRun  = False &= help "Perform a trial run with no changes made"
  , verbose = False &= help "Prints more information"
  } &= summary nfSummary
    &= program nfProgram
    &= help "Number files in the given directory"

main :: IO ()
main = shelly $ do
  CmdOptions{..} <- liftIO $ cmdArgsOrHelp defOpts
  files <- lsF . fromText . pack $ dir
  filenames <- mapM (toTextWarn . filename) files
  let numberWidth = fromIntegral . P.length . show $ P.length filenames * step + 1
  let newFiles = P.zipWith (genNewName numberWidth)
                         [1,1+step..]
                         (P.map (LT.drop lstrip) filenames)
  zipWithM_ (move dryRun verbose) files (P.map fromText newFiles)

genNewName :: Int64 -> Int -> Text -> Text
genNewName numberWidth n f = justifyRight numberWidth '0' (pack . show $ n) <> "-" <> f

