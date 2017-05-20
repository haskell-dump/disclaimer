module DisclaimerSpec where

import Disclaimer
import SimpleCheck
import qualified SimpleWriter as S

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "rectArea" $ do
      it "calculates the area of a square"    $ rectArea 5 5 `shouldBe` 25
      it "calculates the area of a rectangle" $ rectArea 2 3 `shouldBe`  6

    describe "fixLegacyNumber" $
      it "returns positive numbers" $ property propFixLegacyPositive

    describe "SimpleWriter" $ do

      it "can multiply without disclaimers" $
        do
          x <- checkInt 2
          y <- checkInt 21
          return $ x * y
        `shouldBe` S.writer (42, [])

      it "can multiply with disclaimers" $
        do
          x <- checkInt   13
          y <- checkInt 5000
          return $ x * y
        `shouldBe` S.writer (13993, [ "13 Boss hates 13, 7 is better"
                                    , "5000 can cause Y2K bug. Back to 1999" ] )
  where
    propFixLegacyPositive x = (fst . runWriter . fixLegacyNum) x >= 0
