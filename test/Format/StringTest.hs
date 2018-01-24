module Format.StringTest where

import Test.Hspec
import Test.QuickCheck
import Format.String

test :: IO ()
test = hspec $ do
  describe "lpad" $ do
    it "returns a value with the given size, padding with spaces to the left" $
      (lpad 5 ' ' "s" ) `shouldBe` "    s"
    
    it "returns the same string if it's size is equal the target size" $
      (lpad 5 ' ' "sssss") `shouldBe` "sssss"

    it "returns te same string if it's size is bigger than the target size" $
      (lpad 5 ' ' "ssssssssss") `shouldBe` "ssssssssss"

    it "returns a string padded with a custom '-' char" $
      (lpad 5 '-' "s") `shouldBe` "----s"

  describe "rpad" $ do
    it "returns a value with the given size, padding with spaces to the right" $
      (rpad 5 ' ' "s" ) `shouldBe` "s    "
    
    it "returns the same string if it's size is equal the target size" $
      (rpad 5 ' ' "sssss") `shouldBe` "sssss"

    it "returns te same string if it's size is bigger than the target size" $
      (rpad 5 ' ' "ssssssssss") `shouldBe` "ssssssssss"

    it "returns a string padded with a custom '-' char" $
      (rpad 5 '-' "s") `shouldBe` "s----" 
