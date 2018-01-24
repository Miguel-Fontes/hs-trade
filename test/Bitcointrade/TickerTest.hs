module Bitcointrade.TickerTest where


import Test.Hspec
import Test.QuickCheck
import Bitcointrade.Ticker

ticker1 = Ticker 1000.0 1000.0 1000.0 100 1000.0 1000.0 1000.0 "some stub date"

test :: IO ()
test = hspec $ do
  describe "show Ticker" $ do
    it "return 1" $
      show ticker1 `shouldBe`    "high            : R$ 1000.0\n"          
                              ++ "low             : R$ 1000.0\n"
                              ++ "volume          : BTC 1000.0\n"
                              ++ "trades_quantity : 100\n"
                              ++ "lastTrade       : R$ 1000.0\n"
                              ++ "sell            : R$ 1000.0\n"
                              ++ "buy             : R$ 1000.0\n"
                              ++ "date            : \"some stub date\"\n"

      
      