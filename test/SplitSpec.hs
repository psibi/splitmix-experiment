module SplitSpec where

import System.Random.SplitMix
import Test.Hspec

spec :: Spec
spec = do
  describe "Splitmax test" $ do
    it "seed/unseed" $ do
      let seed = (10000, 10000)
          gen = seedSMGen' seed
          outputSeed = unseedSMGen gen
      seed `shouldBe` (10000, 10001)
