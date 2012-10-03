{-# LANGUAGE RecordWildCards, DeriveDataTypeable, OverloadedStrings  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Text.Lazy as LT
import Prelude as P hiding (FilePath)
import Shelly
import Shelly.Utils
import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Utils
import Data.String(fromString)
import Data.Monoid((<>))
import Filesystem.Path(filename)
default (LT.Text)

nfVersion, nfCopyright, nfProgram, nfSummary :: String
nfVersion   = "0.0.1"
nfCopyright = "2012"
nfProgram   = "resize-imgs"
nfSummary   = nfProgram ++ " v" ++ nfVersion ++ ", (C) Martin Krauskopf " ++ nfCopyright

data CmdOptions = CmdOptions {
  resolution :: String,
  from       :: String,
  to         :: String
} deriving (Show, Data, Typeable)

defOpts :: CmdOptions
defOpts = CmdOptions
  { resolution = "" &= argPos 0 &= typ "RESOLUTION"
  , from = ""       &= argPos 1 &= typ "FROM"
  , to =   ""       &= argPos 2 &= typ "TO"
  } &= summary nfSummary
    &= program nfProgram
    &= help "Resize images"

convert :: Text -> FilePath -> FilePath -> Sh ()
convert res from to = do
  run_ "convert" ["-resize", res, toTextIgnore from, toTextIgnore to]
  echoCoverted res from to

processDirectory :: Text -> FilePath -> FilePath -> Sh ()
processDirectory resolution srcDir outDir = do
  whenM (test_d outDir) (errorExit $ "\"" <> toTextIgnore outDir <> "\" directory already exists")
  imageFiles <- P.filter isImageType <$> lsF srcDir
  mkdir_p outDir
  mapM_ (\img -> convert resolution img (outDir </> filename img)) imageFiles

isImageType :: FilePath -> Bool
isImageType fp = P.any (`hasExt` fp) ["jpg", "JPG", "png", "PNG"]

main :: IO ()
main = shellyNoDir $ do
  CmdOptions{..} <- liftIO $ cmdArgsOrHelp defOpts
  let (fromPath, toPath) = (fromString from, fromString to)
  let resText = pack resolution
  isDir <- test_d fromPath
  if isDir
     then processDirectory resText fromPath toPath
     else convert resText fromPath toPath

echoCoverted :: Text -> FilePath -> FilePath -> Sh ()
echoCoverted res from to = do
  fromC <- canonicalizeT from
  toC <- canonicalizeT to
  echo $ "resized \"" <> fromC <> "\" -> \"" <> toC <> "\" by " <> res
    where
      canonicalizeT path' = toTextIgnore <$> canonicRelPath path'

