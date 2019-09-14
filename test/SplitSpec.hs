{-# LANGUAGE OverloadedStrings #-}

module SplitSpec where

import Data.Text (Text, pack)
import System.Random (randomR)
import System.Random.SplitMix
import Test.Hspec

populateRandomString :: SMGen -> Text
populateRandomString smGen =
  let (char, ngen) = randomR ('A', 'Z') smGen
      (char2, gen2) = randomR ('A', 'Z') ngen
   in pack (char : [char2])

spec :: Spec
spec = do
  describe "Splitmax test" $ do
    it "seed/unseed" $ do
      let seed = (10000, 10000)
          gen = seedSMGen' seed
          outputSeed = unseedSMGen gen
      seed `shouldBe` outputSeed
    it "random string" $ do
      let seed = (10000, 10000)
      let txt = populateRandomString (seedSMGen' seed)
      txt `shouldBe` "QH"
