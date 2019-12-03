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
      txt `shouldBe` "RK"
