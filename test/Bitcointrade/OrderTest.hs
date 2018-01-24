module Bitcointrade.OrderTest where

import Test.Hspec
import Test.QuickCheck
import Bitcointrade.Order

order1 = Order 10000 "SOME CODE" 100
order2 = Order 20000 "SOME CODE" 200
order3 = Order 30000 "SOME CODE" 300
entries1 = Entries [order1, order2] [order2, order3]

test :: IO ()
test = hspec $ do
  describe "totalValue" $ do
    it "return the total value of a transaction with value 10000 and amount 100" $
      totalValue order1 `shouldBe` 10000.0 * 100.0

  describe "totalAsks and totalBids" $ do
    it "return the sum of all total values of the bids orders list" $
      totalBidsValue entries1 `shouldBe` (10000.0 * 100.000 + 20000.0 * 200.0)

    it "return the sum of all total values of the asks orders list" $
      totalAsksValue entries1 `shouldBe` (20000.0 * 200.000 + 30000.0 * 300.0)

  describe "number of bids and asks" $ do
    it "return the corret number of asks" $
       numberOfAsks entries1 `shouldBe` 2

    it "return the corret number of bids" $
       numberOfBids entries1 `shouldBe` 2

  describe "avarage bids and asks value" $ do
    it "return the average bid value" $
      averageBidValue entries1 `shouldBe` ((10000.0 * 100.000 + 20000.0 * 200.0) / 2)

    it "return the average ask value" $
      averageAskValue entries1 `shouldBe` ((20000.0 * 200.000 + 30000.0 * 300.0) / 2)

  describe "order group" $ do
    it "return bids separated by groups" $
      generateBidsOrderGroups 0 10000 entries1 `shouldBe` [(0, 0), (10000, 1), (20000, 1)]

    it "return asks separated by groups" $
      generateAsksOrderGroups 0 10000 entries1 `shouldBe` [(0, 0), (10000, 0), (20000, 1), (30000, 1)]

  describe "prettify" $ do
    it "return an Ordergroup prettified" $
      prettify (generateBidsOrderGroups 0 10000 entries1) `shouldBe` "10000.0: 1\n20000.0: 1\n"