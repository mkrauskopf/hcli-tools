{-# Language OverloadedStrings #-}
module NormalizerSpec (main, spec) where

import CLI.Tool.Normalizer
import Test.Hspec.HUnit ()
import Test.HUnit hiding (path)
import Test.Hspec.Monadic
import Prelude hiding (catch, FilePath)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "normalizePath" $ do
    it "normalizes relative paths" $ do
      let normalized = map normalizePath
                [ "Atlantské pláže jako žádné jiné.pdf"
                , "Fantaskní ostrov jako z Marsu a opuštěné pláže.pdf"
                , "Lanzarote, krása stvořená dechem pekla.pdf"
                , "Lanzarote na kole a pěšky.pdf"
                ]
      assertEqual "successfully normalized"
                  [ "./Atlantske_plaze_jako_zadne_jine.pdf"
                  , "./Fantaskni_ostrov_jako_z_Marsu_a_opustene_plaze.pdf"
                  , "./Lanzarote__krasa_stvorena_dechem_pekla.pdf"
                  , "./Lanzarote_na_kole_a_pesky.pdf"
                  ]
                  normalized

    it "normalizes filenames in full paths" $
      assertEqual "successfully normalized filename in full path"
                  (normalizePath "/home/joe/tmp/Atlantské pláže jako žádné jiné.pdf")
                  "/home/joe/tmp/Atlantske_plaze_jako_zadne_jine.pdf"

