module Format.NumericTest where

import Test.Hspec
import Test.QuickCheck
import Format.Numeric

test :: IO ()
test = hspec $ do
    describe "showDouble" $ do
      it "returns a big Double formatted as a String" $
        (showDouble 10000000000) `shouldBe` "10000000000.0"
