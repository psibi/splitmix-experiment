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
    it "random string" $ do
      let txt = populateRandomString $ mkSMGen 10000
      txt `shouldBe` "QH"
    it "splitSMGen" $ do
      let seed = (10000, 10001)
          (gen1, gen2) = splitSMGen $ seedSMGen' seed
          gen1unseed = unseedSMGen gen1
          gen2unseed = unseedSMGen gen2
      gen1unseed `shouldBe` (30002, 10001)
      gen2unseed `shouldBe` (6937453752537963546, 11190925850544447621)
