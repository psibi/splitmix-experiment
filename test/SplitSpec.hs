{-# LANGUAGE OverloadedStrings #-}

module SplitSpec where

import Data.Text (Text, pack)
import System.Random (randomR)
import System.Random.SplitMix
import Test.Hspec

populateRandomString :: SMGen -> Text
populateRandomString smGen = helper smGen 4 []
  where
    helper :: SMGen -> Int -> [Char] -> Text
    helper gen counter acc =
      if counter == 0
        then pack acc
        else do
          let (char, ngen) = randomR ('A', 'Z') gen
          helper ngen (counter - 1) (acc ++ [char])

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
      txt `shouldBe` "QHUH"
