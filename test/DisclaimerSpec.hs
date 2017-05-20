module DisclaimerSpec where

import Disclaimer

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "rectArea" $ do
      it "calculates the area of a square"    $ rectArea 5 5 `shouldBe` 25
      it "calculates the area of a rectangle" $ rectArea 2 3 `shouldBe`  6

    describe "fixLegacyNumber" $
      it "returns positive numbers" $ property propFixLegacyPositive
  where
    propFixLegacyPositive x = (fst . runWriter . fixLegacyNum) x >= 0
